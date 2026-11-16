;;; cli.el -*- lexical-binding: t; -*-

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
