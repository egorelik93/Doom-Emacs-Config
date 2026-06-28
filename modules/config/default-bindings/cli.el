;;; config/default-bindings/cli.el -*- lexical-binding: t; -*-

(defun my-keybindings-files ()
      (list
       ;(expand-file-name "+emacs-bindings.el" (doom-module-locate-path '(:config . default)))
       (expand-file-name "+evil-bindings.el" (doom-module-locate-path '(:config . default)))
       (expand-file-name "emacs-bindings.el" (dir!))
       ;(expand-file-name "no-evil-bindings.el" (dir!))
       (expand-file-name "evil-bindings-overrides.el" (dir!))
       ))

; Depends on code from $DOOMDIR/init.el

(defun my/combine-and-expand (output &rest files)
  "Read forms from FILE1 and FILE2, macroexpand each, write to OUTPUT."
  (let ((indent-tabs-mode nil)
        (forms '()))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (condition-case err
            (progn (check-parens) nil)
          (error (error "Paren mismatch in %s: %s" file err)
             err))
        (goto-char (point-min))
        (condition-case nil
            (while t
              (push (read (current-buffer)) forms))
          (end-of-file nil)
          (error (error "Read error in %s at position %s: %s"
                file (point) err)))  ; re-signal as a real error, aborting combine
          ))
    (setq forms (nreverse forms))
    (with-temp-file output
      ;;; no-evil-bindings.el -*- lexical-binding: t; -*-
      (insert (format! ";;; %s -*- lexical-binding: t; -*-\n" output))
      (dolist (form forms)
        (let ();((expanded (macroexpand-all form)))
          (prin1 form (current-buffer))
          (insert "\n")))
      (pp-buffer))))

(unless my-enable-evil
  (add-hook! 'doom-after-sync-hook
    (my-compile-doomdir-elisp "no-evil-windows.el" (dir!))
    (print! "> Combining keybindings ...")
    (apply #'my/combine-and-expand (expand-file-name "all-bindings.el" (dir!)) (my-keybindings-files)))
  )
