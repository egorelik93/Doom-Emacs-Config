;;; themes.el -*- lexical-binding: t; -*-

(defvar theme-configuration-do-not-recurse nil)
(defun setup-theme-configuration (applicable-theme when configure)
  (let* (
         (applicable-theme-p
          (if
              (functionp applicable-theme)
              applicable-theme
            (lambda (loading-theme)
              (eq loading-theme applicable-theme)
                )))
          (configure-advice
           (lambda (loading-theme &rest args)
             "Configure theme"
             (let ((do-not-recurse theme-configuration-do-not-recurse))
               (setq theme-configuration-do-not-recurse t)
               (when (and
                      (not do-not-recurse)
                      (funcall applicable-theme-p loading-theme)
                      (not (member :no-enable args)))
                 (funcall configure loading-theme))
               (setq theme-configuration-do-not-recurse nil))
             )))
    (advice-add #'load-theme when configure-advice)
    (advice-add #'consult-theme when configure-advice)

    (when (funcall applicable-theme-p doom-theme)
      (funcall configure-advice doom-theme))
  ))

(use-package! catppuccin-theme
  :init
  ; Inside Windows CLIs, Catpuccin's 256-color quantization
  ; gets mapped by emacs to something completely unusable.
  ; We thus map it to 16 colors ourselves.
  ;
  ; Important requirements:
  ; - Default text color must be "white".
  ; - Background should be "black" (treated by Windows Terminal as transparent).
  ; - Highlights are used by vertico and must be visible without being unreadable.
  ; - If Highlights of the current line are visible (which they have to be because of prev point),
  ;   then it should ideally be the same color as region selection.
  (when (featurep :system 'windows)
    ; Created by Claude
    (defun my-catppuccin-quantize-to-16 (color)
      "Map a 256-color quantized hex to a Campbell 16-color hex."
      (when (and color (string-match-p "^#[0-9a-fA-F]\\{6\\}$" color))
        (let* ((r (string-to-number (substring color 1 3) 16))
               (g (string-to-number (substring color 3 5) 16))
               (b (string-to-number (substring color 5 7) 16))
               (lum (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)))
               (r1 (/ r 255.0))
               (g1 (/ g 255.0))
               (b1 (/ b 255.0))
               (cmax (max r1 g1 b1))
               (cmin (min r1 g1 b1))
               (delta (- cmax cmin))
               (l (/ (+ cmax cmin) 2.0))
               (s (if (= delta 0) 0
                    (/ delta (- 1 (abs (- (* 2 l) 1))))))
               (h (cond
                   ((= delta 0) 0)
                   ((= cmax r1) (* 60 (mod (/ (- g1 b1) delta) 6)))
                   ((= cmax g1) (* 60 (+ (/ (- b1 r1) delta) 2)))
                   (t           (* 60 (+ (/ (- r1 g1) delta) 4)))))
               (h (if (< h 0) (+ h 360) h)))
          (cond
           ;; Explicit palette mappings for purple/pink family
           ((string-equal color "#bb99ee") "#b4009e")  ; mauve ? brightmagenta
           ((string-equal color "#aabbee") "#881798")  ; lavender ? dark magenta
           ((string-equal color "#eebbdd") "#b4009e")  ; pink ? brightmagenta
           ((string-equal color "#eebbbb") "#cccccc")  ; flamingo ? white
           ((string-equal color "#eecccc") "#cccccc")  ; rosewater ? white
           ;; Very dark ? black/transparent
           ((< lum 40) "#0c0c0c")
           ;; High luminance blue-purple ? white (catches text #bbccee)
           ((and (> lum 150) (> h 200) (< h 310)) "#cccccc")
           ;; Low saturation dispatch by luminance
           ((<= s 0.20)
            (cond
             ((< lum 70)  "#0037da")   ; surface0/highlight/region ? dark blue
             ((< lum 130) "#767676")   ; overlays/comments ? brightblack
             (t           "#cccccc"))) ; subtext/text ? white
           ;; Reds
           ((or (< h 15) (> h 340))
            (if (> lum 120) "#e74856" "#c50f1f"))
           ;; Peach/orange ? dark red (distinct from bright red)
           ((< h 40) "#c50f1f")
           ;; Yellow
           ((< h 70)
            (if (> lum 150) "#f9f1a5" "#c19c00"))
           ;; Green
           ((< h 165)
            (if (> lum 150) "#16c60c" "#13a10e"))
           ;; Teal ? dark cyan (distinct from sky/brightcyan)
           ((< h 210) "#3a96dd")
           ;; Sky/sapphire ? brightcyan
           ((< h 260) "#61d6d6")
           ;; Blue/lavender ? brightblue
           ((< h 310) "#3b78ff")
           ;; Pink/mauve ? magenta family
           (t
            (if (> lum 150) "#b4009e" "#881798"))))))

    (advice-add 'catppuccin-quantize-color :filter-return #'my-catppuccin-quantize-to-16)
  )
  :config

  ; I discovered when trying to remove default +bindings,
  ; theme loading now breaks without this.
  (require 'ring)

  (setq catppuccin-flavor 'macchiato)

  (defun set-catppuccin-flavor-and-reload (flavor)
    (interactive "MOne of latte, frappe, macchiato, or mocha: ")
    (setq catppuccin-flavor (intern flavor))
    (catppuccin-reload)
    (my-configure-catppuccin)
    (catppuccin-reload))

  (setq my-catppuccin-backup-emacs-cursor nil)

  (setq my-catppuccin-base-color "#272b38")
  (defun my-configure-catppuccin ()
    ;(catppuccin-set-color 'base "#282c34" 'macchiato) ; doom-one background
    ;(catppuccin-set-color 'base "#262938" 'macchiato) ; a bit less intense than macchiato
    ;(catppuccin-set-color 'base "#272b38" 'macchiato) ; a little less purple, more grey than the above
    ;(catppuccin-set-color 'base "#282c3a" 'macchiato) ; should be less than the above, but I can't tell.

    (catppuccin-set-color 'base my-catppuccin-base-color 'macchiato)

    (let ((macchiato-red "#ed8796")
          (macchiato-pink "#f5bde6")
          (macchiato-yellow "#eed49f")
          (macchiato-sky "#91d7e3"))
      (custom-theme-set-faces! 'catppuccin
        `(show-paren-match :foreground "#ee82ee" :weight bold)
        ;`(font-lock-builtin-face :foreground ,macchiato-pink)
        `(font-lock-preprocessor-face :foreground "#f0c6c6")
        `(warning :foreground "#cdbe70")
        `(highlight-quoted-symbol :foreground ,macchiato-yellow)
        `(highlight-quoted-quote  :foreground ,macchiato-sky)
        )

      (defun my-evil-update-cursor-color-h ()
        (+evil-update-cursor-color-h)
        (when (member 'catppuccin custom-enabled-themes)
            (put 'cursor 'evil-emacs-color (catppuccin-color 'yellow 'macchiato))))

      (remove-hook! '(doom-load-theme-hook doom-after-modules-config-hook) #'+evil-update-cursor-color-h)
      (add-hook! '(doom-load-theme-hook doom-after-modules-config-hook) #'my-evil-update-cursor-color-h)

      (my-evil-update-cursor-color-h)
      )
    )

  (setup-theme-configuration
   'catppuccin
   :after
   (lambda (_)
     (when (not (eq (catppuccin-color 'base 'macchiato) my-catppuccin-base-color))
       (my-configure-catppuccin)
       (catppuccin-reload)
       ))
   )
  )

(use-package! ef-themes
  :init
  ;(ef-themes-take-over-modus-themes-mode 1)
  (modus-themes-include-derivatives-mode 1)

  :config

  (defun my-modus-theme-p (theme)
    (or
     (string-prefix-p "ef-" (symbol-name theme))
     (string-prefix-p "modus-" (symbol-name theme))))

  ;; solaire-mode
  (defvar my-ef-themes-solaire-faces-do-not-recurse nil)
  (defun my-ef-themes-solaire-faces (loading-theme &rest args)
    (when (and
           (not my-ef-themes-solaire-faces-do-not-recurse)
           (my-modus-theme-p loading-theme)
           ; For whatever reason, loading-theme is not directly available inside modus-themes-with-colors
           (eq loading-theme (modus-themes-get-current-theme)))
      (setq my-ef-themes-solaire-faces-do-not-recurse t)
      (modus-themes-with-colors
        (let ((theme (modus-themes-get-current-theme)))
          (custom-theme-set-faces!
           theme
           `(solaire-default-face :inherit default :background ,bg-dim :foreground ,fg-dim)
           `(solaire-fringe-face :inherit solaire-default-face :foreground ,fg-dim)
           `(solaire-line-number-face :inherit solaire-default-face :foreground ,fg-dim)
           `(solaire-mode-line-face :foreground ,fg-dim :background ,bg-mode-line-active)
           `(solaire-mode-line-inactive-face :inverse-video nil
             :background ,bg-mode-line-active
             :foreground ,fg-prose-verbatim)
           `(solaire-header-line-face :inherit 'solaire-mode-line-face)
           `(solaire-hl-line-face :background ,bg-active)
           `(solaire-org-hide-face :background ,bg-dim :foreground ,bg-dim))))

      ; solaire-mode checks this, so necessary to provide manually
      (put loading-theme 'theme-feature t)
      )
    )

  (advice-add #'solaire-mode--prepare-for-theme-a :before #'my-ef-themes-solaire-faces)

  ; Loading these themes is expensive but necessary to preview theme
  (defvar my-all-modus-themes-loaded nil)
  (advice-add #'consult-theme :before
              (lambda (theme &rest _)
                "Load all modus themes"
                (when (and (my-modus-theme-p theme) (not my-all-modus-themes-loaded))
                  (modus-themes-get-all-known-themes)
                  (setq my-all-modus-themes-loaded t)
                )))

  (advice-add #'consult-theme :after
              (lambda (theme &rest _)
                "Reload config"
                (when (my-modus-theme-p theme)
                  (modus-themes-load-theme theme)
                )))

  (when (my-modus-theme-p doom-theme)
    (my-ef-themes-solaire-faces doom-theme)
    ;(put doom-theme 'theme-feature t)
    (modus-themes-load-theme doom-theme)
    )
  )

; Emacs' only supports 16 colors on Windows CLIs,
; but its assumed shades are nothing like the actual color scheme in Windows Terminal.
; Emacs uses these values to compute the best approximation.

; Created by Claude
(defun my-hex-to-tty-rgb (hex)
  "Convert hex color string to 0-65535 RGB list."
  (list (* (string-to-number (substring hex 1 3) 16) 257)
        (* (string-to-number (substring hex 3 5) 16) 257)
        (* (string-to-number (substring hex 5 7) 16) 257)))

(when (featurep :system 'windows)
  ; According to the Campbell scheme that is Windows Terminal's default.
  ; Note that emacs slot numbers and names are completely different from ANSI.
  (defun my-setup-wt-tty-colors ()
    (tty-color-clear)
    (tty-color-define "black"       0   (my-hex-to-tty-rgb "#0c0c0c"))
    (tty-color-define "blue"        1   (my-hex-to-tty-rgb "#0037da"))
    (tty-color-define "green"       2   (my-hex-to-tty-rgb "#13a10e"))
    (tty-color-define "cyan"        3   (my-hex-to-tty-rgb "#3a96dd"))
    (tty-color-define "red"         4   (my-hex-to-tty-rgb "#c50f1f"))
    (tty-color-define "purple"      5   (my-hex-to-tty-rgb "#881798"))
    (tty-color-define "yellow"      6   (my-hex-to-tty-rgb "#c19c00"))
    (tty-color-define "white"       7   (my-hex-to-tty-rgb "#cccccc"))
    (tty-color-define "brightblack" 8   (my-hex-to-tty-rgb "#767676"))
    (tty-color-define "brightblue"  9   (my-hex-to-tty-rgb "#3b78ff"))
    (tty-color-define "brightgreen" 10  (my-hex-to-tty-rgb "#16c60c"))
    (tty-color-define "brightcyan"  11  (my-hex-to-tty-rgb "#61d6d6"))
    (tty-color-define "brightred"   12  (my-hex-to-tty-rgb "#e74856"))
    (tty-color-define "brightpurple" 13 (my-hex-to-tty-rgb "#b4009e"))
    (tty-color-define "brightyellow" 14 (my-hex-to-tty-rgb "#f9f1a5"))
    (tty-color-define "brightwhite" 15  (my-hex-to-tty-rgb "#f2f2f2"))
    )
  (add-hook 'tty-setup-hook #'my-setup-wt-tty-colors)
  )

(when (and (featurep :system 'windows) (not (display-graphic-p)))
  (add-hook! 'tty-setup-hook
    (unless (memq default-terminal-coding-system '(utf-8 cp65001 utf-8-dos utf-8-unix))
        ; On CMD, this will cause the boon modeline icon to at least
        ; show a state name instead of a code point.
        (setq doom-modeline-icon nil))))
