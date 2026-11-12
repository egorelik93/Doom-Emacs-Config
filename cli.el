;;; cli.el -*- lexical-binding: t; -*-

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
