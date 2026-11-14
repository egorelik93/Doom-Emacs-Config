;;; org-automate/org-automate.el -*- lexical-binding: t; -*-


;; The following utilities are based on code from vulpea,
;; in particular the blog series on scaling agenda.

(defvar org-automate-directories nil
  "Directories that org automation utilities should include.

If using vulpea, probably will want to set this to vulpea-db-sync-directories.")

(defun org-automate-buffer-p ()
  "Return non-nil if the currently visited buffer should accept automation."
  (or (member "org-automate" (org-get-tags))
      (when-let* ((file (buffer-file-name))
                  (dirs (bound-and-true-p org-automate-directories)))
        (let ((file (expand-file-name file)))
          (seq-some
           (lambda (dir)
             (string-prefix-p
              (file-name-as-directory (expand-file-name dir))
              file))
           dirs
           ))))
  ;;(and buffer-file-name
  ;;     (string-prefix-p
  ;;      (expand-file-name (file-name-as-directory org-roam-directory))
  ;;      (file-name-directory buffer-file-name)))
  )

(defvar org-automate-file-update-hook nil)
(defun org-automate--file-update ()
  (when (and (not (active-minibuffer-window))
             (org-automate-buffer-p))
    (run-hooks 'org-automate-file-update-hook)
    ))



(define-minor-mode org-automate-mode
  "Run automation based on org mode."
  :lighter nil
  (cond
   ((and org-automate-mode (org-automate-buffer-p))
    (add-hook 'find-file-hook #'org-automate--file-update nil 'local)
    (add-hook 'before-save-hook #'org-automate--file-update nil 'local))
   (t
    (remove-hook 'find-file-hook #'org-automate--file-update)
    (remove-hook 'before-save-hook #'org-automate--file-update))))

(provide 'org-automate)
