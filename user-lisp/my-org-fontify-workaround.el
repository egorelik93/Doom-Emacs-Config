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
;; src_lang{...}, call_...() and @@...@@ are opaque too, but finding them
;; needs the parser.  Instead, be conservative: when one of those may
;; precede a span in its block of text, leave that span's markers visible.
;; Colors inside such objects can still be wrong, but nothing is hidden.
;; (`my/org-emphasis-mode' in modules/lang/org-pretty handles them with
;; the parser; while that mode is on, this advice steps aside.)
;;
;; It also leaves the ~ and = markers themselves out of the code/verbatim
;; face when they are visible (Org already does this when they are
;; hidden), so the face shows exactly the span's contents.
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

(defvar-local my/org-opaque-object--memo nil
  "(TICK POS . ANSWER) from the last `my/org-opaque-object-may-precede-p' call.")

(defun my/org-opaque-object-may-precede-p (pos)
  "Non-nil if src_, call_ or @@ occurs before POS in its block of text.
The block starts after the previous blank line (at most 50 lines back);
an opaque object containing POS would have to start there.

A single backward search stops at whichever comes first, an opaque
object or a blank line.  Spans are usually checked in buffer order, so
when the buffer is unchanged and POS follows the last position checked,
only the text since then is searched and the previous answer carries
over if nothing is found."
  (let* ((tick (buffer-chars-modified-tick))
         (memo my/org-opaque-object--memo)
         (limit (save-excursion (goto-char pos) (forward-line -50) (point)))
         (resume (and (eq (car memo) tick)
                      (<= limit (cadr memo) pos)))
         (answer
          (save-excursion
            (goto-char pos)
            (if (re-search-backward "src_\\|call_\\|@@\\|^[ \t]*$"
                                    (if resume (cadr memo) limit)
                                    t)
                ;; Found an opaque object, or a blank line (block start).
                (not (looking-at-p "^[ \t]*$"))
              (and resume (cddr memo))))))
    (setq my/org-opaque-object--memo (cons tick (cons pos answer)))
    answer))

(defun my/org-do-emphasis-faces-skip-verbatim-a (fn limit)
  (let ((res (funcall fn limit)))
    ;; After an accepted span, point is right after its opening marker.
    (when (and res (not (bound-and-true-p my/org-emphasis-mode)))
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
             ((my/org-opaque-object-may-precede-p (car markers))
              (with-silent-modifications
                (dolist (m markers)
                  (remove-text-properties m (1+ m) '(invisible nil))))))
            (when verbatim (goto-char span-end))))))
    res))

(advice-add #'org-do-emphasis-faces :around #'my/org-do-emphasis-faces-skip-verbatim-a)

(provide 'my-org-fontify-workaround)
