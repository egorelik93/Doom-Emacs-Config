;;; cli.el -*- lexical-binding: t; -*-

(defun my/dir-fingerprint (dir)
  "Newest mtime among DIR's files, as a float, ignoring .git."
  (let ((files (directory-files-recursively
                dir "\\.el\\'" nil
                (lambda (d) (not (string-match-p "/\\.git\\'" d))))))
    (if files
        (apply #'max (mapcar (lambda (f)
                               (float-time (file-attribute-modification-time
                                            (file-attributes f))))
                             files))
      0)))

(cl-defmacro local-package! (package &key (local-repo nil) (files nil))
  `(let ((fingerprint (my/dir-fingerprint (expand-file-name ,local-repo (dir!)))))
     (package! ,package
       :type 'local
       :recipe (
                :local-repo ,local-repo
                ,@(when files `((:files ,files)))
                :build t)
       :env (list (cons 'my-fingerprint fingerprint)))
    )
  )

;; Suggested by Google
(defun my-erase-log-buffer (buf-name)
  (let ((log-buf (get-buffer buf-name)))
    (when log-buf
      (with-current-buffer log-buf
        (let ((inhibit-read-only t))
          (erase-buffer)))))
    )

(defun my-compile-doomdir-elisp (file &optional dir)
  (interactive "f")
  (print! "> Compiling %s ..." file)
  (my-erase-log-buffer "*Compile-Log*")
  (let ((byte-compile-warnings nil)
        (result (byte-compile-file (expand-file-name file (or dir doom-user-dir)))))
    (unless (or (eq 'no-byte-compile result) (eq t result))
      (with-current-buffer "*Compile-Log*"
        (print! "  - Compilation failed: %s" (buffer-string))))
    )
  (when (native-comp-available-p)
    (my-erase-log-buffer "*Native-compile-log*")
    (let ((result (native-compile (expand-file-name file (or dir doom-user-dir)))))
      (unless result
        (with-current-buffer "*Native-compile-log*"
          (print! "  - Native Compilation failed: %s" (buffer-string))))
      )))


(when (or my-redirect-evil-emacs-maps my-redirect-evil-maps)
  (add-hook! 'doom-before-sync-hook
    (my-compile-doomdir-elisp "redirect-evil-to-boon.el")))

(add-hook! 'doom-after-sync-hook
  (my-erase-log-buffer "*Compile-Log*")
  (my-erase-log-buffer "*Async-native-compile-log*")

  (let ((user-lisp-directory (concat doom-user-dir "user-lisp")))
    (when (file-directory-p user-lisp-directory)
      (print! "Preparing %s" user-lisp-directory)
      (prepare-user-lisp nil nil t)))
  (when (file-directory-p user-lisp-directory)
    (print! "Preparing %s" user-lisp-directory)
    (prepare-user-lisp nil nil t))

  (with-current-buffer "*Compile-Log*"
    (when (string-search "Error" (buffer-string))
      (print! "  - Compilation failed: %s" (buffer-string))))
  (with-current-buffer "*Async-native-compile-log*"
    (when (string-search "Error" (buffer-string))
      (print! "  - Native Compilation failed: %s" (buffer-string))))
  )
