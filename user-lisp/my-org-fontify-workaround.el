;;; user-lisp/org-fontify-workaround.el -*- lexical-binding: t; -*-

;; Created by Claude, to fix Org fontifying bogus emphasis inside
;; ~code~/=verbatim= (wrong faces always; hidden characters with
;; org-pretty-mode/org-hide-emphasis-markers).
;;
;; `org-do-emphasis-faces' resumes scanning right after an opening marker
;; (to allow nested emphasis), so it also rescans inside ~code~/=verbatim=.
;; Markers in there preceded by a "pre" char then start bogus spans: the
;; first ~ in ~foo(~~bar)~, the slashes in ~cd /tmp/ now~, the tildes in
;; ~echo '~/x'~.  Verbatim contents are opaque, so skip past the closing
;; marker instead.
;;
;; src_lang{...}, call_...() and @@...@@ are opaque too, and so are LaTeX
;; fragments ($...$, $$...$$, \(...\), \[...\], \name{...}) and
;; environments (\begin{...}...\end{...}).  For those, look back through
;; the span's block of text for an object that starts before the span
;; and ends after it, using Org's own parser for each object type (but
;; not the element cache), and leave such a span's markers visible.
;; Colors inside such objects can still be wrong, but nothing is hidden.
;; (`my/org-emphasis-mode' in modules/lang/org-pretty handles them with
;; the parser; while that mode is on, this advice steps aside.)
;;
;; It also leaves the ~ and = markers themselves out of the code/verbatim
;; face when they are visible (Org already does this when they are
;; hidden), so the face shows exactly the span's contents.
(require 'org-element)
(eval-when-compile (require 'cl-lib))

(defun my/org-verbatim-markers-unface (positions)
  "Remove `org-code'/`org-verbatim' from the face at each of POSITIONS.
Other faces there (e.g. bold from an enclosing span) are kept."
  (with-silent-modifications
    (dolist (pos positions)
      (let* ((face (get-text-property pos 'face))
             (kept (if (and (listp face) (not (keywordp (car face))))
                       (seq-remove (lambda (f) (memq f '(org-code org-verbatim))) face)
                     (unless (memq face '(org-code org-verbatim)) face))))
        (put-text-property pos (1+ pos) 'face kept)))))

(defconst my/org-opaque-object-or-blank-re
  (concat "src_\\|call_\\|@@"
          ;; LaTeX: $ and $$, \( and \[, \name{ and \name[; also the
          ;; closers \) and \], which rule out earlier openers.
          "\\|\\$\\|\\\\\\(?:[][()]\\|[a-zA-Z]+[[{]\\)"
          "\\|^[ \t]*$")
  "Possible start or closer of an opaque object, or a blank line.")

(defun my/org-opaque-object-end (block-end)
  "End of the opaque object starting at point, or nil if there is none.
BLOCK-END is the end of the block of text (the next blank line), which
bounds the object as the paragraph bounds it for Org's parser."
  (save-restriction
    (narrow-to-region (point-min) block-end)
    (if (looking-at "\\\\begin{\\([A-Za-z0-9*]+\\)}")
        ;; A LaTeX environment if alone at the start of its line
        ;; (otherwise the fragment parser takes it as a command).
        (and (save-excursion (skip-chars-backward " \t") (bolp))
             (save-excursion
               (if (re-search-forward
                    (format "^[ \t]*\\\\end{%s}" (regexp-quote (match-string 1)))
                    nil t)
                   (point)
                 block-end)))
      (let ((object
             (ignore-errors
               (pcase (char-after)
                 (?s (org-element-inline-src-block-parser))
                 (?c (org-element-inline-babel-call-parser))
                 (?@ (org-element-export-snippet-parser))
                 (_ (org-element-latex-fragment-parser))))))
        (and object
             (- (org-element-end object) (org-element-post-blank object)))))))

(defvar-local my/org-opaque-object--memo nil
  "(TICK POS . END) from the last `my/org-opaque-object-end-after' call.")

(defvar-local my/org-opaque-object--parsed nil
  "(TICK BLOCK . ENDS) for `my/org-opaque-object-end-after'.
ENDS maps each possible object start checked so far to its end (or
nil), so each is parsed once per buffer state, whatever order spans are
checked in.  BLOCK is (FROM . END): the block end (next blank line)
found from FROM, which holds for any position from FROM up to END.")

(defun my/org-opaque-object-end-after (pos)
  "End of an opaque object that starts before POS and ends after it, or nil.
Opaque objects are src_lang{...}, call_name(...), @@backend:...@@ and
LaTeX fragments ($...$, $$...$$, \\(...\\), \\[...\\], \\name{...}) and
environments (\\begin{...}...\\end{...}).  Their contents are never
parsed for markup, so markers there are not markers.  When several
contain POS, the furthest end is returned.

Only the block of text around POS is searched: it runs between the
nearest blank lines, looking at most 50 lines back.  Each possible start is
checked with Org's own parser for that object, with the buffer narrowed
to the block (`my/org-opaque-object-end'), so its rules apply exactly.
Results are kept until the buffer changes (`my/org-opaque-object--parsed').

Spans are usually checked in buffer order, so when the buffer is
unchanged and POS follows the last position checked, only the text
since then is searched, and the previous answer carries over while it
is still past POS."
  (let* ((tick (buffer-chars-modified-tick))
         (memo my/org-opaque-object--memo)
         (limit (save-excursion (goto-char pos) (forward-line -50) (point)))
         (resume (and (eq (car memo) tick)
                      (<= limit (cadr memo) pos)))
         ;; The previous answer, if still past POS.
         (carried (and resume (cddr memo) (> (cddr memo) pos) (cddr memo)))
         ;; Set up on the first match only, so plain text pays nothing.
         parsed ends line-start
         ;; Closers seen so far, between the current match and POS.
         seen-dollar seen-at seen-paren seen-bracket
         best block-end)
    (save-excursion
      (goto-char pos)
      (while (and (re-search-backward my/org-opaque-object-or-blank-re
                                      (if resume (cadr memo) limit)
                                      t)
                  (if (looking-at-p "^[ \t]*$")
                      ;; Block start: nothing before it counts.
                      (progn (setq carried nil) nil)
                    t))
        ;; Most objects end at the first closer after the opener, so
        ;; once one lies between a possible opener and POS, that opener
        ;; cannot enclose POS and need not be parsed.  Only $$, src_,
        ;; call_ and environments, which close differently, always are.
        (unless
            (pcase (char-after)
              (?$ (prog1 (and seen-dollar
                              (not (eq (char-before) ?$))
                              (not (eq (char-after (1+ (point))) ?$)))
                    (setq seen-dollar t)))
              (?@ (prog1 seen-at (setq seen-at t)))
              (?\\
               (pcase (char-after (1+ (point)))
                 (?\) (setq seen-paren t))
                 (?\] (setq seen-bracket t))
                 (?\( seen-paren)
                 (?\[ seen-bracket)
                 ;; \name[...]{...} cannot span lines, unlike an
                 ;; environment (\begin{...} alone at a line start).
                 (_ (and (< (point)
                            (or line-start
                                (setq line-start
                                      (save-excursion
                                        (goto-char pos)
                                        (line-beginning-position)))))
                         (not (and (looking-at-p "\\\\begin{")
                                   (save-excursion
                                     (skip-chars-backward " \t")
                                     (bolp)))))))))
          (unless parsed
            (setq parsed (if (eq (car my/org-opaque-object--parsed) tick)
                             my/org-opaque-object--parsed
                           (setq my/org-opaque-object--parsed
                                 (cons tick (cons nil (make-hash-table)))))
                  ends (cddr parsed)))
          (let ((end (gethash (point) ends 'unknown)))
            (when (eq end 'unknown)
              (unless block-end
                (let ((block (cadr parsed)))
                  (setq block-end
                        (if (and block (<= (car block) pos) (< pos (cdr block)))
                            (cdr block)
                          ;; No line limit here: then every start in the
                          ;; block gets the same end, so cached results do
                          ;; not depend on which span asked first.
                          (save-excursion
                            (goto-char pos)
                            (let ((bend (if (re-search-forward "^[ \t]*$" nil t)
                                            (match-beginning 0)
                                          (point-max))))
                              (setcar (cdr parsed) (cons pos bend))
                              bend))))))
              (setq end (save-excursion (my/org-opaque-object-end block-end)))
              (puthash (point) end ends))
            (when (and end (> end pos) (or (null best) (> end best)))
              (setq best end))))))
    (when (and carried (or (null best) (> carried best)))
      (setq best carried))
    (setq my/org-opaque-object--memo (cons tick (cons pos best)))
    best))

(defvar my/org--saved-match-data nil
  "List reused by `my/org-do-emphasis-faces-skip-verbatim-a' to save match data.")

(defun my/org-do-emphasis-faces-skip-verbatim-a (fn limit)
  (let ((res (funcall fn limit)))
    ;; After an accepted span, point is right after its opening marker.
    ;; Font-lock reads the match data `org-do-emphasis-faces' leaves
    ;; behind, so the searches here must not change it.  Saved as
    ;; integers, not markers (as `save-match-data' would): cheaper, and
    ;; safe since only text properties change here.
    (when (and res (not (bound-and-true-p my/org-emphasis-mode)))
      (let ((md (match-data t my/org--saved-match-data)))
        (setq my/org--saved-match-data md)
        (unwind-protect
            (let ((verbatim (memq (char-before) '(?~ ?=)))
                  (start (if (save-excursion (backward-char) (bolp))
                             (1- (point))
                           (- (point) 2))))
              (when (save-excursion
                      (goto-char start)
                      (looking-at (if verbatim org-verbatim-re org-emph-re)))
                ;; Group 3 is the opener; the closer is a backreference right
                ;; after the contents (group 4).
                (let ((markers (list (match-beginning 3) (match-end 4)))
                      (span-end (match-end 2)))
                  (cond
                   ((not org-hide-emphasis-markers)
                    (when verbatim (my/org-verbatim-markers-unface markers)))
                   ((my/org-opaque-object-end-after (car markers))
                    (with-silent-modifications
                      (dolist (m markers)
                        (remove-text-properties m (1+ m) '(invisible nil))))))
                  (when verbatim (goto-char span-end)))))
          (set-match-data md t))))
    res))

(advice-add #'org-do-emphasis-faces :around #'my/org-do-emphasis-faces-skip-verbatim-a)

;; With `org-pretty-entities', `org-fontify-entities' draws \alpha as an
;; alpha even inside ~code~/=verbatim=, which export keeps literal.  (Org
;; skips both elsewhere: `org-do-latex-and-related' checks for these
;; faces, and `org-raise-scripts' skips emphasized text.)  It runs after
;; `org-do-emphasis-faces', so the code/verbatim face is already there to
;; check.
(defun my/org-fontify-entities-skip-verbatim-a (fn limit)
  (let (res)
    (while (and (setq res (funcall fn limit))
                (let ((face (get-text-property (match-beginning 0) 'face)))
                  (when (if (listp face)
                            (or (memq 'org-code face) (memq 'org-verbatim face))
                          (memq face '(org-code org-verbatim)))
                    ;; Undo the composition and look for the next entity.
                    (decompose-region (match-beginning 0) (match-end 0))
                    t))))
    res))

(advice-add #'org-fontify-entities :around #'my/org-fontify-entities-skip-verbatim-a)

;; `org-do-latex-and-related' skips code/verbatim by checking the face
;; with `face-at-point', which lets overlays mask text properties.  So
;; whenever an overlay with a face covers the text (`hl-line' on the
;; current line, the region, spell-check marks), it misses the verbatim
;; face and fontifies the contents as LaTeX; natively, that replaces the
;; verbatim face altogether.  Make that check read the text's own face.
(defun my/org-do-latex-and-related-text-faces-a (fn limit)
  (cl-letf (((symbol-function 'face-at-point)
             (lambda (&optional _text multiple)
               (let ((faces (ensure-list (get-text-property (point) 'face))))
                 (if multiple faces (car (last faces)))))))
    (funcall fn limit)))

(advice-add #'org-do-latex-and-related :around #'my/org-do-latex-and-related-text-faces-a)

(provide 'my-org-fontify-workaround)
