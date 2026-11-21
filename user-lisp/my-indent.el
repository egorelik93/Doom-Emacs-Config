;;; user-lisp/my-indent.el -*- lexical-binding: t; -*-

(require 'doom-lib)
(require 'el-patch)

  ;; Only needed for el-patch validation
(eval-when-compile (progn
                     (require 'doom)
                     (require 'doom-modules)
                     (autoload 'doom/backward-delete-whitespace-to-column
                       (doom-module-locate-path '(:config . default) "autoload/text.el"))))


(defvar my-override-soft-tab-mode)

(defvar my-override-soft-tab-width tab-width
  "Overrides tab-width used for soft TAB key presses
when my-override-soft-tab-mode is enabled.
Does not change display of TAB characters using tab-width.")

;; Copied from insert-tab in indent.el
(defun my-insert-tab (&optional arg)
  (let ((count (prefix-numeric-value arg)))
    (if (and abbrev-mode
             (eq (char-syntax (preceding-char)) ?w))
        (expand-abbrev))
    (if indent-tabs-mode
        (insert-char ?\t count)
      (indent-to (* my-override-soft-tab-width (+ count (/ (current-column) my-override-soft-tab-width)))))))


(defun my-indent-for-tab-command (orig-fn &optional arg)
  (if my-override-soft-tab-mode
      (letf! ((#'insert-tab #'my-insert-tab))
        (funcall orig-fn arg))
    (funcall orig-fn arg)))


;; Copied from doom/backward-delete-whitespace-to-column
;; in doom's modules/config/default/autoload/text.el
(el-patch-defun
  (el-patch-swap
    doom/backward-delete-whitespace-to-column
    my-doom/backward-delete-whitespace-to-column)
  ()
  "Delete back to the previous column of whitespace, or as much whitespace as
possible, or just one char if that's not possible."
  (interactive)
  (let* ((context
          (if (bound-and-true-p smartparens-mode)
              (ignore-errors (sp-get-thing))))
         (op (plist-get context :op))
         (cl (plist-get context :cl))
         open-len close-len current-column)
    (cond ;; When in strings (sp acts weird with quotes; this is the fix)
     ;; Also, skip closing delimiters
     ((and op cl
           (string= op cl)
           (and (string= (char-to-string (or (char-before) 0)) op)
                (setq open-len (length op)))
           (and (string= (char-to-string (or (char-after) 0)) cl)
                (setq close-len (length cl))))
      (delete-char (- open-len))
      (delete-char close-len))

     ;; Delete up to the nearest tab column IF only whitespace between
     ;; point and bol.
     ((and (not indent-tabs-mode)
           (> (el-patch-swap tab-width my-override-soft-tab-width) 1)
           (not (bolp))
           (not (doom-point-in-string-p))
           (>= (abs (save-excursion (skip-chars-backward " \t")))
               (setq current-column (current-column))))
      (delete-char (- (1+ (% (1- current-column) (el-patch-swap tab-width my-override-soft-tab-width))))))

     ;; Otherwise do a regular delete
     ((delete-char -1)))))



(defun my-doom/backward-delete-whitespace-to-column-advice (orig-fn)
    (if (not my-override-soft-tab-mode)
        (funcall orig-fn)
      (my-doom/backward-delete-whitespace-to-column)))


;;;###autoload
(define-minor-mode my-override-soft-tab-mode
  "Make TAB, when inserting whitespace, go by `my-override-soft-tab-width'
instead of `tab-width'."
  :lighter nil
  (when my-override-soft-tab-mode
    (advice-add 'indent-for-tab-command :around #'my-indent-for-tab-command)
    (advice-add 'doom/backward-delete-whitespace-to-column :around #'my-doom/backward-delete-whitespace-to-column-advice)
    )
  )

(provide 'my-indent)
