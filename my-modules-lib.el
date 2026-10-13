;;; my-modules-lib.el -*- lexical-binding: t; -*-

(defun my-modules-replace (replace-list modules)
  (let ((replace-alist (seq-map (lambda (x) (if (consp x) x (cons x nil))) replace-list)))
    (my-modules-replace--rec replace-alist modules t nil)))

(defun my-modules-get-replacement (spec &optional ret-multi keyword)
  (if (not spec) nil
    (when (eq (car spec) 'doom!)
      (setq spec (cdr spec))
      (if (and ret-multi keyword)
          (setq keyword nil)
        (setq spec (cdr spec)))
      (unless ret-multi
        (setq spec (car spec))
        ))
    (cond
     ((not ret-multi) (list spec))
     ((keywordp keyword) (cons keyword spec))
     (t spec)
     )))

(defun my-modules-replace--rec (replace-list modules top inclusions)
  (require 'cl-lib)
  (let* ((category-to-replace nil))
    (seq-mapcat
     (lambda (x)
       (cond
        ((and top (keywordp x))
         (if-let* ((a (assoc x replace-list)))
             (progn
               (setq replace-list (cl-delete x replace-list :key #'car :count 1))
               (if (eq (car (cdr a)) :keep)
                   (progn
                     (setq inclusions (cdr (cdr a)))
                     (when inclusions (setq category-to-replace x))
                     (list x)
                     )
                 (setq category-to-replace x)
                 (my-modules-get-replacement (cdr a) t x)))
           (setq category-to-replace nil)
           (setq inclusions nil)
           (list x)))
        ((and category-to-replace (not inclusions)) nil)
        ((and (listp x) (keywordp (car x)))
         (if (memq (car x) '(:if :when :unless))
             (when-let* ((module (cdr (cdr x)))
                         (r (my-modules-replace--rec replace-list module nil inclusions)))
               (cons (car x) (cons (car (cdr x)) r)))
           (list (my-modules-replace--rec replace-list x nil inclusions))))
        ((and (listp x) category-to-replace)
         (if (memq (car x) inclusions) (list x) nil))
        ((listp x)
         (if-let* ((a (assoc (car x) replace-list)))
             (my-modules-get-replacement (cdr a))
           (list (my-modules-replace--rec replace-list x nil inclusions))))
        (category-to-replace
         (if (memq x inclusions) (list x) nil))
        ((not (keywordp x))
         (if-let* ((a (assoc x replace-list)))
             (my-modules-get-replacement (cdr a))
           (list x)))
        (t (list x))))
     modules)))
