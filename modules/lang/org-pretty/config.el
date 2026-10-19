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
;;     text never touches it, and even then it bypasses the element cache.
;; Otherwise it behaves exactly like stock Org.  Toggle it with
;; `my/org-emphasis-mode' (off by default).
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

(defvar my/org-emphasis-element-source 'uncached
  "How the parser check obtains the element around a candidate marker.

`uncached' (default): parse without Org's element cache, reusing the
  last result via `my/org-emphasis--element-memo'.  About as fast as
  `cache' in practice, since candidates repeat within a paragraph.

`cache': query the element cache normally (`org-element-at-point').
  This is what originally hung Emacs: in a live GUI session, a
  corrupted table-row entry (contents cut short mid-row) appeared in
  the cache, and `org-element-context', called later by Doom's eldoc
  link helper, looped forever lexing it.  Cause never found; it did
  not reproduce in batch.  Note that the cache was then queried for
  every candidate, before the text check; now it is only queried
  near src_/call_/@@, so this setting alone may not reproduce it.

`cache-read-only': read the cache without ever writing to it
  (`org-element-at-point' with CACHED-ONLY), sanity-check the entry,
  and fall back to `uncached' when it is missing or looks wrong.
  Cannot leave entries behind for other callers.  Untested live.")

(defvar-local my/org-emphasis--element-memo nil
  "(TICK . ELEMENT) for the last element parsed without the element cache.")

(defun my/org-emphasis--element-sane-p (element pos)
  "Non-nil if ELEMENT plausibly is the leaf element at POS.
Guards `cache-read-only' against stale or truncated cache entries."
  (and element
       (<= (org-element-begin element) pos)
       (< pos (org-element-end element))
       (or (not (eq (org-element-type element) 'table-row))
           (null (org-element-contents-end element))   ; rule rows
           ;; A row's contents run to the end of its line.
           (= (org-element-contents-end element)
              (save-excursion
                (goto-char (org-element-begin element))
                (end-of-line)
                (skip-chars-backward " \t")
                (point))))))

(defun my/org-emphasis--element-uncached (pos)
  "Element at POS, parsed without Org's element cache.
Reuses the previous result while the buffer is unchanged and POS is
still inside it, since uncached parsing starts from the section top.
Only leaf containers are reused: a headline or item also spans the
elements nested in it, so POS being inside one says nothing."
  (let ((tick (buffer-chars-modified-tick))
        (memo my/org-emphasis--element-memo))
    (if (and (eq (car memo) tick)
             (memq (org-element-type (cdr memo))
                   '(paragraph table-row verse-block))
             (<= (org-element-begin (cdr memo)) pos)
             (< pos (org-element-end (cdr memo))))
        (cdr memo)
      (let ((element (save-excursion
                       (goto-char pos)
                       (let ((org-element-use-cache nil))
                         (org-element-at-point)))))
        (setq my/org-emphasis--element-memo (cons tick element))
        element))))

(defun my/org-emphasis--element-at (pos)
  "Element at POS, obtained per `my/org-emphasis-element-source'."
  (pcase my/org-emphasis-element-source
    ('cache
     (save-excursion (goto-char pos) (org-element-at-point)))
    ('cache-read-only
     (let ((cached (save-excursion (goto-char pos) (org-element-at-point pos 'cached))))
       (if (my/org-emphasis--element-sane-p cached pos)
           cached
         (my/org-emphasis--element-uncached pos))))
    (_ (my/org-emphasis--element-uncached pos))))

(defun my/org-emphasis--opaque-hint-nearby-p (pos)
  "Non-nil if `my/org-emphasis--opaque-hint-re' occurs in the block around POS.
The block runs between the nearest blank lines, capped at 50 lines each
way.  Any opaque object containing POS starts within it.  This is plain
text search, so ordinary text never reaches the parser."
  (save-excursion
    (let ((bbeg (progn (goto-char pos) (forward-line -50) (point)))
          (bend (progn (goto-char pos) (forward-line 51) (point))))
      (goto-char pos)
      (when (re-search-backward "^[ \t]*$" bbeg t) (setq bbeg (point)))
      (goto-char pos)
      (when (re-search-forward "^[ \t]*$" bend t) (setq bend (point)))
      (goto-char bbeg)
      (re-search-forward my/org-emphasis--opaque-hint-re bend t))))

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
        (when (my/org-emphasis--opaque-hint-nearby-p beg)
          (goto-char beg)
          (ignore-errors
            ;; How the element is obtained (with or without the element
            ;; cache) is up to `my/org-emphasis-element-source'.
            (let* ((element (my/org-emphasis--element-at beg))
                   (cbeg (org-element-contents-begin element))
                   (cend (org-element-contents-end element))
                   (span
                    (if (and (memq (org-element-type element)
                                   '(paragraph table-row verse-block))
                             cbeg (<= cbeg beg) (< beg cend))
                        (seq-find (lambda (s) (and (<= (cadr s) beg)
                                                   (< beg (cddr s))))
                                  (my/org-emphasis--container-opaque-spans
                                   element))
                      ;; Single-line containers (headline titles, item
                      ;; tags) are cheap to query directly.
                      (let ((obj (org-element-context element)))
                        (when (memq (org-element-type obj)
                                    my/org-emphasis-opaque-types)
                          (cons (org-element-type obj)
                                (cons (org-element-begin obj)
                                      (- (org-element-end obj)
                                         (org-element-post-blank obj)))))))))
              (and span
                   (not (and (eq (car span) type)
                             (= (cadr span) beg)
                             (= (cddr span) end)))))))))))

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
                        ;; Added: for code/verbatim, face only the contents
                        ;; even when markers are visible.
                        (m (if (or org-hide-emphasis-markers verbatim?) 4 2))
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

;; Only enable while upstream's function is the one we copied: compare a
;; hash of its (whitespace-normalized) source, so Org upgrades that don't
;; touch it stay silent.  cli.el records upstream's hash at `doom sync' time.
;; After re-syncing the copy, set this to the hash the warning reports.
(defconst my/org-emphasis--synced-hash "6bf6b250c6fb3e1688712d43da7889c34ce61521"
  "Hash of the upstream `org-do-emphasis-faces' that `my/org-do-emphasis-faces' copies.")

(defconst my/org-emphasis--upstream-hash-file
  (expand-file-name "upstream-hash.eld" (dir!))
  "File where cli.el records upstream's hash at `doom sync' time.")

(defun my/org-emphasis--recorded-hash ()
  "Upstream's hash as recorded by cli.el at `doom sync' time, or nil."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents my/org-emphasis--upstream-hash-file)
      (read (current-buffer)))))

(defun my/org-emphasis--refontify ()
  "Refontify all Org buffers, so toggling the mode takes effect at once."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        (font-lock-flush)))))

(define-minor-mode my/org-emphasis-mode
  "Keep Org emphasis fontification out of opaque objects.
Replaces `org-do-emphasis-faces' with `my/org-do-emphasis-faces', which
does not fontify markup inside ~code~, =verbatim=, src_lang{...},
call_...() or @@...@@.  Refuses to turn on, with a warning, unless the
upstream function still matches the one the copy was made from.

Off by default; to turn it on at startup, use
  (after! org (my/org-emphasis-mode +1))"
  :global t
  :group 'org-appearance
  (if my/org-emphasis-mode
      (let ((hash (my/org-emphasis--recorded-hash)))
        (if (equal hash my/org-emphasis--synced-hash)
            (advice-add #'org-do-emphasis-faces :override #'my/org-do-emphasis-faces)
          (setq my/org-emphasis-mode nil)
          (display-warning
           'org-pretty
           (if hash
               (format "Upstream `org-do-emphasis-faces' changed (hash %s); not \
enabling `my/org-emphasis-mode'.  Re-sync `my/org-do-emphasis-faces' with it, \
then update `my/org-emphasis--synced-hash' (modules/lang/org-pretty/config.el)." hash)
             "No recorded hash of upstream `org-do-emphasis-faces'; not enabling \
`my/org-emphasis-mode'.  Run 'doom sync' to record it."))))
    (advice-remove #'org-do-emphasis-faces #'my/org-do-emphasis-faces))
  (my/org-emphasis--refontify))
