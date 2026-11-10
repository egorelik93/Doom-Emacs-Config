;;; themes.el -*- lexical-binding: t; -*-

(defvar use-theme-enable-theme-alist nil)
(defun use-theme-enable-theme-h (theme)
  (dolist (f (alist-get theme use-theme-enable-theme-alist))
    (funcall f)))
(add-hook 'enable-theme-functions #'use-theme-enable-theme-h -50)

(defmacro use-theme-eval-after-enable (theme form)
  `(push ,(if (listp form) `(lambda () (eval ',form)) form) (alist-get ,theme use-theme-enable-theme-alist))
  )

(defvar use-theme-disable-theme-alist nil)
(defun use-theme-disable-theme-h (theme)
  (dolist (f (alist-get theme use-theme-disable-theme-alist))
    (funcall f)))
(add-hook 'disable-theme-functions #'use-theme-disable-theme-h -50)

(defmacro use-theme-eval-after-disable (theme form)
  `(push ,(if (listp form) `(lambda () (eval ',form)) form) (alist-get ,theme use-theme-disable-theme-alist))
  )


(defmacro use-theme-package! (package &rest args)
  "Adds detection of doom disabled packages to use-theme-package"
  (declare (indent 1))
  (unless (memq package doom-disabled-packages)
    `(use-theme-package ,package ,@args)))

(cl-defmacro use-theme-package (package
                                 &key
                                 (theme nil)
                                 (theme-fn nil)
                                 (preface nil)
                                 (defer nil)
                                 (init nil)
                                 (first-config nil)
                                 (every-config nil)
                                 (first-enable nil)
                                 (every-enable nil)
                                 (every-disable nil))
  "Single-file themes and use-package :defer do not play as expected together;
because themes are loaded, not required, and use-package defers using eval-after-load,
:config will get evaluated on every load, but only when deferred.

This macro wraps use-package in ways that make sense for themes that
use a single file or feature for both theme and underlying machinery,
leaving us no distinct :config to use.
For other themes, this is usable but consider the ordinary use-package.

:preface        Evaluated at expansion time.
                Can be used to define functions for use by :defer.
:theme          Either a theme name, quoted list of theme names, or an sexp.
                Must be provided if the theme name differs from package.
                If an sexp, it is interpreted as a test of the provided variable \"theme\".
:theme-fn       A function.
                An alternative to specifying :theme.
:defer          Any boolean expression.
                Unlike use-package, the expression will be evaluated before getting passed
                to use-package.
                We suggest enabling this, or possibly conditioning it on
                whether the theme is unused if appropriate.
                Doom Emacs startup behavior seems to be that if the theme is already loaded,
                it will just enable it. Combined with :defer t, that can created
                slight differences in load behavior and timing.
:init           A list of sexps.
                Same behavior as use-package.
:first-config   A list of of sexps.
                As the name suggests, runs only the first time the package is loaded,
                which is usually the expected and desired behavior of :config.
                Theme permitting, we recommend doing typical package config here.
:every-config   A list of sexps.
                Runs every time the package is loaded.
                Unless running at enable-time is insufficient, we recommend not using this.
:first-enable   A list of sexps.
                Runs only the first time the theme is enabled.
                May be a reasonable alternative to first-config if that is too early.
                Note that it is run only once per package, even if multiple themes are provided.
                The variable \"theme\" is available to determine which is being enabled.
:every-enable   A list of sexps.
                Runs every time the theme is enabled.
                We recommend setting any final face modifications here.
                The variable \"theme\" is available to determine which is being enabled.
:every-disable  A list of sexps.
                Runs every time the theme is disabled.
                The variable \"theme\" is available to determine which is being enabled."
  (declare (indent 1))
  (when preface
    (eval preface)
    )
  (let* ((themes (cond
                  ((symbolp theme) (list theme))
                  ((and (listp theme) (eq (car theme) 'quote)) (ensure-list (cadr theme)))
                  (theme-fn nil)
                  (t package)))
         (theme-test (cond
                      (theme-fn `(funcall ,theme-fn theme))
                      (themes nil)
                      (t theme)))
         (defer (when defer (eval defer)))
         (theme-enabled-sym (when first-enable (intern (concat "use-theme--" (symbol-name package) "--enabled"))))
         (package-configured-sym (when first-config (intern (concat "use-theme--" (symbol-name package) "--configured")))))
    `(use-package ,package
       ,@(when defer `(:defer ,defer))
       ,@(when (or init first-config first-enable every-enable every-disable)
           (remq
            nil
            `(:init
              ,@init
              ,(when first-enable `(setq ,theme-enabled-sym nil))
              ,(when first-config `(setq ,package-configured-sym nil))
              ,(when (or first-enable every-enable)
                 (let ((enable-h
                        (append
                         (when first-enable
                           `((unless ,theme-enabled-sym
                               (setq ,theme-enabled-sym t)
                               ,@first-enable
                               ))
                           )
                         every-enable
                         )))
                   (if theme-test
                       `(add-hook 'enable-theme-functions
                         (defun ,(intern (concat "use-theme--" (symbol-name package) "--enable-h")) (theme)
                           ,@enable-h
                           )
                         -50)
                     (mapcan
                      (lambda (theme)
                        `(use-theme-eval-after-enable ',theme (let ((theme ',theme)) ,@enable-h)))
                      themes))))
              ,(when every-disable
                 (if theme-test
                     `(add-hook 'disable-theme-functions
                       (lambda (theme) @,every-disable)
                       -50)
                   (mapcan
                    (lambda (theme)
                      `(use-theme-eval-after-disable ',theme (let ((theme ',theme))) ,@every-disable)))))
              )))
       ,@(when (or first-config every-config)
           (remq
            nil
            (if defer
                `(:config
                  ,(when first-config
                     `(unless ,package-configured-sym
                        (setq ,package-configured-sym t)
                        ,@first-config
                        ))
                  ,@every-config)
              `(:config
                ,@first-config
                ,@every-config
                ,(when every-config
                   `(eval-after-load ,package ,every-config)
                   )
                ))))
       )))

(use-theme-package! catppuccin-theme
  :theme catppuccin
  :defer t
  :first-enable
  (
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

  ; I discovered when trying to remove default +bindings,
  ; theme loading now breaks without this.
  (require 'ring)

  (setq catppuccin-flavor 'macchiato)

  (defun set-catppuccin-flavor-and-reload (flavor)
    (interactive "MOne of latte, frappe, macchiato, or mocha: ")
    (setq catppuccin-flavor (intern flavor))
    (load-theme 'catppuccin t t)
    (my-configure-catppuccin)
    (catppuccin-reload))

  ;; Requires catppuccin to already be loaded
  (defun my-configure-catppuccin ()
    (setq my-catppuccin-base-color "#272b38")

    ;(catppuccin-set-color 'base "#282c34" 'macchiato) ; doom-one background
    ;(catppuccin-set-color 'base "#262938" 'macchiato) ; a bit less intense than macchiato
    ;(catppuccin-set-color 'base "#272b38" 'macchiato) ; a little less purple, more grey than the above
    ;(catppuccin-set-color 'base "#282c3a" 'macchiato) ; should be less than the above, but I can't tell.

    (catppuccin-set-color 'base my-catppuccin-base-color 'macchiato))
  (my-configure-catppuccin)

  (defun my-customize-catppuccin-faces ()
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
      )
    )

  (defun my-catppuccin-evil-update-cursor-color-h ()
    (put 'cursor 'evil-emacs-color (catppuccin-color 'yellow 'macchiato)))

  ;; Set to run before doom-load-theme-hook which runs +evil-update-cursor-color-h
  (add-hook 'enable-theme-functions
            (lambda (theme)
              (when (and (eq theme 'catppuccin) (eq catppuccin-flavor 'macchiato))
                (advice-add '+evil-update-cursor-color-h :after #'my-catppuccin-evil-update-cursor-color-h)
                (my-customize-catppuccin-faces)
                )))

  (add-hook 'disable-theme-functions
            (lambda (theme)
              (when (and (eq theme 'catppuccin) (eq catppuccin-flavor 'macchiato))
                (advice-remove '+evil-update-cursor-color-h #'my-catppuccin-evil-update-cursor-color-h)
                )))

  (load-theme 'catppuccin t t)
  ))

(use-package! ef-themes
  :defer (not (my-modus-theme-p doom-theme))
  :init
  (defun my-modus-theme-p (theme)
    (or
     (string-prefix-p "ef-" (symbol-name theme))
     (string-prefix-p "modus-" (symbol-name theme))))

  :config
  ;(ef-themes-take-over-modus-themes-mode 1)
  (modus-themes-include-derivatives-mode 1)

  ;; solaire-mode
  (defun my-ef-themes-solaire-faces (loading-theme)
    (when (my-modus-theme-p loading-theme)
      ; For whatever reason, loading-theme is not directly available inside modus-themes-with-colors
      (eval
       `(modus-themes-with-colors
          (custom-theme-set-faces!
            ',loading-theme
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

  ;; This hook is run *by* enable-theme/load-theme - which means
  ;; technically it runs /before/ solaire-mode--prepare-for-theme-a
  ;; which is :after advice on load-theme.
  (add-hook 'enable-theme-functions #'my-ef-themes-solaire-faces -90)
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
