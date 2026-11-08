;;; config/default-bindings/config.el -*- lexical-binding: t; -*-

; Load doom emacs bindings that evil mode disables
(unless (doom-context-p 'reload)
  (if (modulep! :editor evil)
      (progn (load! "emacs-bindings.el") (setopt persp-keymap-prefix (kbd "C-c W")))
    ;(load! "no-evil-bindings.el")
    ;(load! "+evil-bindings" (doom-module-locate-path '(:config . default)))
    ;(load! "evil-bindings-overrides")

    ; Attempting to replace by loading bindings manually.
    ;(load! "+evil-bindings" (doom-module-locate-path '(:config . default)))
    ;(load! "emacs-bindings")
    ;(load! "evil-bindings-overrides")

    ; Set the contributing files in init.el; doom sync will generate this file.
    (if my-log-evil-keybinds
        (load! "all-bindings.el" (dir!))
      (let* ((byte-compile-warnings nil)
             (byte-compile-verbose nil)
             (result (byte-recompile-file (expand-file-name "all-bindings.el" (dir!)) nil 0))
             )
        (when (and (eq result t) (native-comp-available-p))
          (native-compile (expand-file-name "all-bindings.el" (dir!)))
          )
        )
      (load! "all-bindings" (dir!)))

    ; Need to set this or else persp will override C-c w
    (setopt persp-keymap-prefix (kbd "C-c W"))
    (after! projectile
      (define-key projectile-mode-map (kbd "C-c p") nil)
      (define-key projectile-mode-map (kbd "C-c P") 'projectile-command-map))
    ))
