;;; lang/org-pretty/cli.el -*- lexical-binding: t; -*-

;; Record a hash of the installed Org's `org-do-emphasis-faces' at sync time,
;; so config.el can tell at load time whether its copy is still in sync
;; without reading Org's source on every startup.

(defun +org-pretty--upstream-source-hash ()
  "SHA-1 of the installed `org-do-emphasis-faces' source, whitespace-normalized.
Nil if the source cannot be found."
  (ignore-errors
    (let ((file (or (and (fboundp 'straight--build-file)
                         (let ((f (straight--build-file "org" "org.el")))
                           (and (file-exists-p f) f)))
                    (progn (require 'find-func)
                           (find-library-name "org")))))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "^(defun org-do-emphasis-faces " nil t)
          (let ((beg (match-beginning 0)))
            (goto-char beg)
            (with-syntax-table emacs-lisp-mode-syntax-table (forward-sexp))
            (secure-hash 'sha1 (replace-regexp-in-string
                                "[ \t\n]+" " "
                                (buffer-substring-no-properties beg (point))))))))))

(defun +org-pretty--synced-hash ()
  "The hash config.el's copy was synced against, read without loading it."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents (expand-file-name "config.el" (dir!)))
      (goto-char (point-min))
      (let (form)
        (while (not (eq (car-safe form) 'defconst))
          (setq form (read (current-buffer)))
          (unless (eq (cadr form) 'my/org-emphasis--synced-hash)
            (setq form nil)))
        (nth 2 form)))))

(add-hook! 'doom-after-sync-hook
  (print! "> Recording org-do-emphasis-faces hash ...")
  (let ((hash (+org-pretty--upstream-source-hash))
        (synced (+org-pretty--synced-hash)))
    (with-temp-file (expand-file-name "upstream-hash.eld" (dir!))
      (prin1 hash (current-buffer)))
    (cond
     ((null hash)
      (print! (warn "Could not read upstream `org-do-emphasis-faces'; org-pretty will use stock Org")))
     ((not (equal hash synced))
      (print! (warn "Upstream `org-do-emphasis-faces' changed (hash %s); org-pretty will use stock Org until its copy is re-synced" hash))))))
