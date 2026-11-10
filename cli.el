;;; cli.el -*- lexical-binding: t; -*-

(add-hook! 'doom-after-sync-hook
  (let ((user-lisp-directory (concat doom-user-dir "user-lisp")))
    (when (file-directory-p user-lisp-directory)
      (print! "Preparing %s" user-lisp-directory)
      (prepare-user-lisp)))
  (when (file-directory-p user-lisp-directory)
    (print! "Preparing %s" user-lisp-directory)
    (prepare-user-lisp))
  )
