;;; term/ghostel/autoload.el -*- lexical-binding: t; -*-

(defvar +ghostel--id nil)

;;;###autoload
(defun +ghostel/toggle (arg)
  "Toggles a terminal popup window at project root.

If prefix ARG is non-nil, recreate ghostel buffer in the current project's root.

Returns the ghostel buffer."
  (interactive "P")
  (+ghostel--configure-project-root-and-display
   arg
   (lambda ()
     (let ((buffer-name
            (format "*doom:ghostel-popup:%s*"
                    (if (bound-and-true-p persp-mode)
                        (safe-persp-name (get-current-persp))
                      "main")))
           confirm-kill-processes
           current-prefix-arg)
       (when arg
         (let ((buffer (get-buffer buffer-name))
               (window (get-buffer-window buffer-name)))
           (when (buffer-live-p buffer)
             (kill-buffer buffer))
           (when (window-live-p window)
             (delete-window window))))
       (if-let* ((win (get-buffer-window buffer-name)))
           (delete-window win)
         (let ((buffer (or (cl-loop for buf in (doom-buffers-in-mode 'ghostel-mode)
                                    if (equal (buffer-local-value '+ghostel--id buf)
                                              buffer-name)
                                    return buf)
                           (get-buffer-create buffer-name))))
           (with-current-buffer buffer
             (unless (eq major-mode 'ghostel-mode)
               ; Ghostel doesn't yet seem to support vterm's ability to switch to a named buffer
               ; so had to replicate the functionality of (ghostel) here.
               ; May need to keep this code up to date.
               (require 'ghostel)
               (ghostel--load-module t)
               (ghostel--init-buffer buffer)
               (setq ghostel--managed-buffer-name (buffer-name))
               (setq ghostel--buffer-identity (buffer-name))
               (ghostel--start-process))
             (setq-local +ghostel--id buffer-name))
           (pop-to-buffer buffer)))
       (get-buffer buffer-name)))))

;;;###autoload
(defun +ghostel/here (arg)
  "Open a terminal buffer in the current window at project root.

If prefix ARG is non-nil, cd into `default-directory' instead of project root.

Returns the ghostel buffer."
  (interactive "P")
  (+ghostel--configure-project-root-and-display
   arg
   (lambda()
     (require 'ghostel)
     ;; HACK: Force ghostel to redraw to fix artefacting in tty.
     (save-window-excursion
       (pop-to-buffer "*scratch*"))
     (let (display-buffer-alist)
       (ghostel ghostel-buffer-name)))))

(defun +ghostel--configure-project-root-and-display (arg display-fn)
  "Sets the environment variable PROOT and displays a terminal using `display-fn`.

If prefix ARG is non-nil, cd into `default-directory' instead of project root.

Returns the ghostel buffer."
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load ghostel"))
  (let* ((project-root (or (doom-project-root) default-directory))
         (default-directory
           (if arg
               default-directory
             project-root)))
    (setenv "PROOT" project-root)
    (funcall display-fn)))

;;;###autoload
(defun +ghostel/beginning-of-line ()
  "Equivalent to C-a in the shell."
  (interactive)
  (ghostel-send-key "a" nil nil t)
  (when (bound-and-true-p evil-local-mode)
    (evil-refresh-cursor)))

;;;###autoload
(defun +ghostel/delete-line ()
  "Equivalent to C-e C-u in the shell."
  (interactive)
  (ghostel-send-key "e" nil nil t)
  (ghostel-send-key "u" nil nil t)
  (when (bound-and-true-p evil-local-mode)
    (evil-refresh-cursor)))
