;;; lang/org-pretty/config.el -*- lexical-binding: t; -*-

;; Created by Claude, to fix Org fontifying bogus emphasis inside opaque
;; objects (wrong faces always; hidden characters with
;; org-pretty-mode/org-hide-emphasis-markers).

;; `org-do-emphasis-faces' finds emphasis with a regexp that only
;; approximates Org syntax, and it fontifies spans inside objects whose
;; contents are opaque: it rescans after an opener, so the first ~ in
;; ~foo(~~bar)~ starts a bogus span, and it also fontifies inside
;; src_lang{...} bodies.  Those spans get emphasis faces (e.g. italic
;; inside ~a /b/ c~) whether or not markers are hidden, and with
;; `org-hide-emphasis-markers' their "markers" are hidden too.  This is a copy of the Org 9.8.9
;; function with two additions:
;;  1. After a ~code~/=verbatim= span, resume scanning after its closing
;;     marker.  This alone fixes the rescan everywhere, keywords included.
;;  2. Veto a candidate the parser places inside an opaque object.  The
;;     parser is only consulted when the surrounding text contains an
;;     opaque object the skip cannot handle (src_, call_, @@), so ordinary
;;     text never touches it.
;; Otherwise it behaves exactly like stock Org.  Re-sync with upstream when
;; upgrading Org.
(defconst my/org-emphasis-opaque-types
  '(code verbatim inline-src-block inline-babel-call export-snippet)
  "Object types whose contents are never parsed for markup.")

(defconst my/org-emphasis--opaque-hint-re "src_\\|call_\\|@@"
  "Text that must be present nearby for the parser check to be worth running.
Covers the opaque objects other than code and verbatim, which the skip
in `my/org-do-emphasis-faces' already handles.")

(defvar-local my/org-emphasis--opaque-cache nil
  "(TICK ELEMENT-BEGIN . SPANS) for the last container parsed.")

(defun my/org-emphasis--container-opaque-spans (element)
  "Return (TYPE BEGIN . CLOSER-END) for each opaque object in ELEMENT.
ELEMENT is a paragraph, table row or verse block.  Parsing the whole
container once avoids `org-element-context' re-lexing it from the start
for every candidate marker.  Uses the same lexer `org-element-context' does."
  (let ((tick (buffer-chars-modified-tick))
        (ebeg (org-element-begin element)))
    (if (and (eq (car my/org-emphasis--opaque-cache) tick)
             (eq (cadr my/org-emphasis--opaque-cache) ebeg))
        (cddr my/org-emphasis--opaque-cache)
      (let (spans)
        (org-with-wide-buffer
         (named-let walk ((beg (org-element-contents-begin element))
                          (end (org-element-contents-end element))
                          (restriction (org-element-restriction
                                        (org-element-type element))))
           (save-restriction
             (narrow-to-region beg end)
             (goto-char (point-min))
             (let (next)
               ;; The table-cell lexer does not stop at the end of the
               ;; row by itself, and a non-advancing object would loop.
               (while (and (not (eobp))
                           (setq next (org-element--object-lex restriction))
                           (> (org-element-end next) (point)))
                 (if (memq (org-element-type next) my/org-emphasis-opaque-types)
                     (push (cons (org-element-type next)
                                 (cons (org-element-begin next)
                                       (- (org-element-end next)
                                          (org-element-post-blank next))))
                           spans)
                   (let ((cbeg (org-element-contents-begin next))
                         (cend (org-element-contents-end next)))
                     (when (and cbeg cend)
                       (save-excursion
                         (walk cbeg cend (org-element-restriction next))))))
                 (goto-char (org-element-end next)))))))
        (setq my/org-emphasis--opaque-cache (cons tick (cons ebeg spans)))
        spans))))

(defun my/org-emphasis-inside-opaque-p (marker)
  "Non-nil if the parser puts the current emphasis match inside an opaque object.
That is, its opener lies within a `my/org-emphasis-opaque-types' object
other than the one the match itself describes.  Match data must be from
`org-emph-re' or `org-verbatim-re'.  Any parser failure means no veto."
  (let ((beg (match-beginning 2))
        (end (match-end 2))
        (type (pcase marker ("=" 'verbatim) ("~" 'code))))
    (save-excursion
      (save-match-data
        (goto-char beg)
        (ignore-errors
          (let* ((element (org-element-at-point))
                 (cbeg (org-element-contents-begin element))
                 (cend (org-element-contents-end element))
                 (container-p (and (memq (org-element-type element)
                                         '(paragraph table-row verse-block))
                                   cbeg (<= cbeg beg) (< beg cend)))
                 (span
                  (when (progn
                          ;; Other objects (headline titles, item tags,
                          ;; keywords) live on a single line.
                          (goto-char (if container-p cbeg (line-beginning-position)))
                          (re-search-forward my/org-emphasis--opaque-hint-re
                                             (if container-p cend (line-end-position))
                                             t))
                    (goto-char beg)
                    (if container-p
                        (seq-find (lambda (s) (and (<= (cadr s) beg)
                                                   (< beg (cddr s))))
                                  (my/org-emphasis--container-opaque-spans
                                   element))
                      ;; Single-line containers are cheap to query directly.
                      (let ((obj (org-element-context element)))
                        (when (memq (org-element-type obj)
                                    my/org-emphasis-opaque-types)
                          (cons (org-element-type obj)
                                (cons (org-element-begin obj)
                                      (- (org-element-end obj)
                                         (org-element-post-blank obj))))))))))
            (and span
                 (not (and (eq (car span) type)
                           (= (cadr span) beg)
                           (= (cddr span) end))))))))))

(defun my/org-do-emphasis-faces (limit)
  "Run through the buffer and emphasize strings."
  (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\)"
                          (car org-emphasis-regexp-components))))
    (catch :exit
      (while (re-search-forward quick-re limit t)
        (let* ((marker (match-string 2))
               (verbatim? (member marker '("~" "="))))
          (when (save-excursion
                  (goto-char (match-beginning 0))
                  (and
                   ;; Do not match table hlines.
                   (not (and (equal marker "+")
                             (org-match-line
                              "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
                   ;; Do not match headline stars.  Do not consider
                   ;; stars of a headline as closing marker for bold
                   ;; markup either.
                   (not (and (equal marker "*")
                             (save-excursion
                               (forward-char)
                               (skip-chars-backward "*")
                               (looking-at-p org-outline-regexp-bol))))
                   ;; Match full emphasis markup regexp.
                   (looking-at (if verbatim? org-verbatim-re org-emph-re))
                   ;; Do not span over paragraph boundaries.
                   (not (string-match-p org-element-paragraph-separate
                                        (match-string 2)))
                   ;; Do not span over cells in table rows.
                   (not (and (save-match-data (org-match-line "[ \t]*|"))
                             (string-match-p "|" (match-string 4))))
                   ;; Added: not inside an opaque object.
                   (not (my/org-emphasis-inside-opaque-p marker))))
            (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist))
                        (m (if org-hide-emphasis-markers 4 2))
                        ;; Added: where to resume after a verbatim span.
                        (skip-to (and verbatim? (match-end 2))))
              (font-lock-prepend-text-property
               (match-beginning m) (match-end m) 'face face)
              (when verbatim?
                (org-remove-flyspell-overlays-in
                 (match-beginning 0) (match-end 0))
                (remove-text-properties (match-beginning 2) (match-end 2)
                                        '(display t invisible t intangible t)))
              (add-text-properties (match-beginning 2) (match-end 2)
                                   '(font-lock-multiline t org-emphasis t))
              (when (and org-hide-emphasis-markers
                         (not (org-at-comment-p)))
                (add-text-properties (match-end 4) (match-beginning 5)
                                     '(invisible t))
                (org-rear-nonsticky-at (match-beginning 5))
                (add-text-properties (match-beginning 3) (match-end 3)
                                     '(invisible t)))
              ;; Added: verbatim contents are opaque; do not rescan them.
              (when skip-to (goto-char skip-to))
              (throw :exit t))))))))

;; Only override while upstream's function is the one we copied: compare a
;; hash of its (whitespace-normalized) source, so Org upgrades that don't
;; touch it stay silent.  cli.el records upstream's hash at `doom sync' time.
;; After re-syncing the copy, set this to the hash the warning reports.
(defconst my/org-emphasis--synced-hash "6bf6b250c6fb3e1688712d43da7889c34ce61521"
  "Hash of the upstream `org-do-emphasis-faces' that `my/org-do-emphasis-faces' copies.")

(defconst my/org-emphasis--upstream-hash-file
  (expand-file-name "upstream-hash.eld" (dir!))
  "File where cli.el records upstream's hash at `doom sync' time.")

(after! org
  (let ((hash (ignore-errors
                (with-temp-buffer
                  (insert-file-contents my/org-emphasis--upstream-hash-file)
                  (read (current-buffer))))))
    (if (equal hash my/org-emphasis--synced-hash)
        (advice-add #'org-do-emphasis-faces :override #'my/org-do-emphasis-faces)
      (display-warning
       'org-pretty
       (if hash
           (format "Upstream `org-do-emphasis-faces' changed (hash %s); using \
stock Org.  Re-sync `my/org-do-emphasis-faces' with it, then update \
`my/org-emphasis--synced-hash' (modules/lang/org-pretty/config.el)." hash)
         "No recorded hash of upstream `org-do-emphasis-faces'; using stock Org \
emphasis fontification.  Run 'doom sync' to record it.")))))
