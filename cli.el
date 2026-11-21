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


(defun my-prepare-user-lisp (&optional dir)
  (my-erase-log-buffer "*Compile-Log*")

  (defvar el-patch-validate-during-compile)

  ;; We are forcing byte-compilation below, but
  ;; it doesn't force native compilation;
  ;; we have to set this to ensure that the eln doesn't
  ;; get stale and force a startup compile.
  ;; Because this variable is evaluated by a job queue,
  ;; it is not enough to set it within a scope.
  (setq native-comp-always-compile t)

  (let ((user-lisp-directory (or dir user-lisp-directory))
        ;; This only has an effect during byte-compilation,
        ;; which is thankfully synchronously.
        (el-patch-validate-during-compile t))
      (when (file-directory-p user-lisp-directory)
        (print! "Preparing %s" user-lisp-directory)
        (prepare-user-lisp nil nil t)))

  (with-current-buffer "*Compile-Log*"
    (let ((string (buffer-string)))
      (when (or (string-search "Error" string)
                (string-search "Warning: (el-patch):" string))
        (print! "  - Compilation failed: %s" string))))
  )

(defun my-log-el-patch-warnings (orig-fn &rest args)
  (letf!
    ((defadvice display-warning (:around (orig-fn type message &optional level _buffer-name))
       (if (eq type 'el-patch)
           (byte-compile-warn "(%S): %s" type message)
         (funcall orig-fn type message level _buffer-name))
       ))
    (apply orig-fn args)))

(add-hook! 'doom-after-sync-hook
  ;; Unlike the synchronous compile log, we only want to erase this one once.
  (my-erase-log-buffer comp-async-buffer-name)

  (advice-add 'el-patch-validate :around #'my-log-el-patch-warnings)

  (my-prepare-user-lisp (concat doom-user-dir "user-lisp"))
  (my-prepare-user-lisp)
  )


;; Because native compilation is async, we can only catch
;; errors at the very end of the doom sync.
(add-hook 'doom-after-sync-hook
          (lambda ()
            ;; When everything else is done, we have to wait
            ;; to get output from native-compile-async
            ;; Suggested by Google.
            (while (or comp-files-queue
                       (> (hash-table-count comp-async-compilations) 0))
              (accept-process-output nil 0.5))

            (with-current-buffer comp-async-buffer-name
              (let ((string (buffer-string)))
                (when (or (string-search "Error" string))
                  (print! "> Native Compilation failed:\n%s" string)))))
          90)
