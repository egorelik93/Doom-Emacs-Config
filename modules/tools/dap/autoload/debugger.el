;;; tools/debugger/autoload/debugger.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +dap/start ()
  "Start a debugger in the current project and buffer."
  (interactive)
  (call-interactively
   (when (and (bound-and-true-p lsp-mode)
            (require 'dap-mode nil t))
       #'dap-debug)))

;;;###autoload
(defun +dap/quit ()
  "Quit the active debugger session."
  (interactive)
  (when-let* ((conn (and (require 'dap-mode nil t)
                       (dap--cur-session))))
      (dap-disconnect conn)))
