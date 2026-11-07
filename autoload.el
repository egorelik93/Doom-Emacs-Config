;;; $DOOMDIR/autoload.el -*- lexical-binding: t; -*-

;; Originally created for use by my/org-latex-preview-all in org.el
;; Created by Claude
;;;###autoload
(defun my/shift-prefix (arg n)
  "Shift raw prefix ARG up by N levels of C-u (each level = x4).
Handles the common raw-prefix forms: nil, a cons like (4), or
a plain integer (from C-u 3 etc.)."
  (let ((base (cond
               ((null arg) 1)                 ; no prefix => treat as C-u^0
               ((consp arg) (car arg))         ; (4), (16), ...
               ((integerp arg) arg)            ; numeric arg, e.g. C-u 3
               (t 1))))
    (list (* base (expt 4 n)))))

;; Originally created for insertion into +dashboard-menu-sections in org.el
;;;###autoload
(defun insert-into-list (list n el)
  "Insert into list LIST an element EL at index N.

If N is 0, EL is inserted before the first element.

The resulting list is returned.  As the list contents is mutated
in-place, the old list reference does not remain valid."
  (let* ((padded-list (cons nil list))
         (c (nthcdr n padded-list)))
    (setcdr c (cons el (cdr c)))
    (cdr padded-list)))
