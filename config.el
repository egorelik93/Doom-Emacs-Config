;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


; Put exit modifications first, to make sure they still run even with an error in this file.

(defvar my-prompt-on-exit t)
(defvar my-popup-on-exit nil)

(defun my-quit-p (&optional prompt)
  "Do not prompt the user for confirmation when killing Emacs.

Returns t if it is safe to kill this session. Does not prompt if no real buffers
are open."
  (interactive "P")
  (or (not (ignore-errors (doom-real-buffer-list)))
      (if my-prompt-on-exit
          (yes-or-no-p (format "%s" (or prompt "Really quit Emacs?")))
          (if (or use-dialog-box my-popup-on-exit)
              (x-popup-dialog
               t `("Really quit Emacs?"
                   ("Yes" . t)
                   ("Cancel" . nil)))
            t))
      (ignore (message "Aborted"))))

(defun my-quit-fn (&rest _)
  (interactive)
  (my-quit-p
   (format "%s  %s"
           (propertize (nth (random (length +doom-quit-messages))
                            +doom-quit-messages)
                       'face '(italic default))
           "Really quit Emacs?")))

(setq confirm-kill-emacs #'my-quit-fn)

(when (eq confirm-kill-emacs #'my-quit-fn)
  (advice-add 'handle-delete-frame :around
            (lambda (orig-fn event &rest args)
              (setq my-prompt-on-exit nil)
              (unwind-protect
                  (apply orig-fn event args)
                (setq my-prompt-on-exit t)
                (when (server-running-p)
                  (server-edit)
                  )
                )
              )))

(defun my-noask-quit-message (&rest _)
  (interactive)
  (message (format "%s"
           (propertize (nth (random (length +doom-quit-messages))
                            +doom-quit-messages)
                       'face '(italic default))))
  t)

(when (not confirm-kill-emacs)
  (add-hook! kill-emacs-query-functions #'my-noask-quit-message))


; Doom apparently sets the default scratch to Fundamental for faster loading,
; compared to the emacs default of lisp.
; Setting this to t causes doom's own scratch to inherit
; whatever was the mode of the previous buffer - but only the first time that scratch is created.
; Since Doom also persists scratch and its mode, as far as I can tell
; this is pretty much only useful for a project-specific scratch starting in that project's mode.
; See this link: https://github.com/doomemacs/doomemacs/issues/490
; For my purposes, running elisp like in the original *scratch* buffer is probably the most useful.
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)


; Compenstates for cursor functionality assumed in this file
(unless (modulep! :editor evil)
  ; Copied from evil-core.el

  (defun evil-set-cursor-color (color)
    "Set the cursor color to COLOR."
    (unless (equal (frame-parameter nil 'cursor-color) color)
      ;; `set-cursor-color' forces a redisplay, so only
      ;; call it when the color actually changes
      (set-cursor-color color)))
  (defun +evil-update-cursor-color-h ()
      ;; Use a flashy color for emacs state.
      (put 'cursor 'evil-emacs-color  (face-foreground 'warning))
      (put 'cursor 'evil-normal-color (face-background 'cursor)))
  (defun +evil-default-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-normal-color)))
  (defun +evil-emacs-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))

  (defun evil-set-cursor (specs)
  "Change the cursor's apperance according to SPECS.
SPECS may be a cursor type as per `cursor-type', a color
string as passed to `set-cursor-color', a zero-argument
function for changing the cursor, or a list of the above.

If SPECS is nil, this function does not have an effect;
pass (list nil) instead to indicate a nil `cursor-type'
\(i.e., to disable the cursor)."
  (unless (and (not (functionp specs))
               (listp specs)
               (null (cdr-safe (last specs))))
    (setq specs (list specs)))
  (dolist (spec specs)
    (cond
     ((functionp spec)
      (ignore-errors (funcall spec)))
     ((stringp spec)
      (evil-set-cursor-color spec))
     (t
      (setq cursor-type spec)))))

  (setq evil-state 'emacs)
  (defun evil-emacs-state-p () t)
  )


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;(setq user-full-name "John Doe"
;      user-mail-address "john@doe.com")
(when (file-exists-p "~/.doom.d/private.el")
  (load-file "~/.doom.d/private.el"))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Cascadia Code" :size 16 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Calibri" :size 16 :weight 'regular))

(when (file-exists-p "~/.doom.d/local-font-config.el")
  (load-file "~/.doom.d/local-font-config.el"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;(setq doom-theme 'doom-one)

;(setq doom-theme 'my-doom-oceanic-next)
;(setq doom-theme 'ef-owl)
(setq doom-theme 'catppuccin)


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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq doom-leader-alt-key "C-c"
      doom-localleader-alt-key "C-c l")

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
    (let ((byte-compile-warnings nil)
          (byte-compile-verbose nil))
      (byte-recompile-file (expand-file-name "all-bindings.el" doom-user-dir) nil 0))
    ;(my-compile-doomdir-elisp "all-bindings.el")
    (load! "all-bindings.elc")

    ; Need to set this or else persp will override C-c w
    (setopt persp-keymap-prefix (kbd "C-c W"))
    (after! projectile
      (define-key projectile-mode-map (kbd "C-c p") nil)
      (define-key projectile-mode-map (kbd "C-c P") 'projectile-command-map))
    ))

;; This works for leader but not localleader
;(map! :map 'override "<f13>" #'doom/leader)
;(map! :map 'override "<Tools>" #'doom/leader)

;(map! :map 'override "<Launch6>" #'my-localleader-alias)

(defconst alt-tap-wsl "M-<Tools>")
;;; In WSL, F13 is getting mapped to <Tools>
(defconst alt-tap "M-<f13>")
(defconst alt-tap-term-raw "\e[25;3~")

(defconst o-hold-wsl "<Launch7>")
(defconst o-hold "<f16>")

(defconst alt-dbl-tap-wsl "M-<Launch5>")
(defconst alt-dbl-tap "M-<f14>")
(defconst alt-dbl-tap-term-raw "\e[26;3~")

(defconst ctl-tap-wsl "C-<Launch6>")
(defconst ctl-tap "C-<f15>")
(defconst ctl-tap-term-raw "\e[28;5~")

(defconst ctl-dbl-tap-wsl "C-<Launch9>")
(defconst ctl-dbl-tap "C-<f18>")
(defconst ctl-dbl-tap-term-raw "\e[32;5~")

(defun translate-to-leader (prompt)
  (if (or
       (equal (this-single-command-raw-keys) (vector last-input-event))
       (equal (this-single-command-raw-keys) (vconcat (listify-key-sequence alt-tap-term-raw))))
      (if (not (evil-emacs-state-p))
          (kbd doom-leader-key)
        (kbd doom-leader-alt-key))
    (kbd alt-tap)
    ))

(defun translate-to-localleader (prompt)
  (if (or
       (equal (this-single-command-raw-keys) (vector last-input-event))
       (equal (this-single-command-raw-keys) (vconcat (listify-key-sequence alt-dbl-tap-term-raw))))
      (if (not (evil-emacs-state-p))
          (kbd doom-localleader-key)
        (kbd doom-localleader-alt-key))
    (kbd alt-dbl-tap)
    ))

(defvar my-esc-pred nil)
(defvar my-esc-delay 0.01)

; Modified from evil-esc from evil-core.el
(defun my-esc (map)
  "Translate \\e to `escape' if no further event arrives.
This function is used to translate a \\e event either to `escape'
or to the standard ESC prefix translation map. If \\e arrives,
this function waits for `my-esc-delay' seconds for another
event. If no other event arrives, the event is translated to
`escape', otherwise it is translated to the standard ESC prefix
map stored in `input-decode-map'. When `my-esc-pred' is
a function that returns false, the event is
translated to the ESC prefix.

The translation to `escape' happens only if the current command
has indeed been triggered by \\e. In other words, this will only
happen when the keymap is accessed from `read-key-sequence'. In
particular, if it is access from `define-key' the returned
mapping will always be the ESC prefix map."
  (if (and (or (not my-esc-pred) (funcall my-esc-pred))
           (let ((keys (this-single-command-keys)))
             (and (> (length keys) 0)
                  (= (aref keys (1- (length keys))) ?\e)))
           (sit-for my-esc-delay))
      (prog1 [escape]
        (when defining-kbd-macro
          (end-kbd-macro)
          (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
          (start-kbd-macro t t)))
    map))

; Must place all modifications to the low-level key maps here;
; They are "terminal-local" variables, which means they will be reset
; for any frames in a second terminal window.
(defun my-init-key-translation-maps ()
  (map! :map 'key-translation-map alt-tap-wsl #'translate-to-leader)
  (map! :map 'key-translation-map alt-tap #'translate-to-leader)
  (map! :map 'input-decode-map alt-tap-term-raw (kbd alt-tap))

  (map! :map 'key-translation-map o-hold-wsl (kbd (concat "C-c " alt-tap)))
  (map! :map 'key-translation-map o-hold (kbd (concat "C-c " alt-tap)))

  (map! :map 'key-translation-map alt-dbl-tap-wsl #'translate-to-localleader)
  (map! :map 'key-translation-map alt-dbl-tap #'translate-to-localleader)
  (map! :map 'input-decode-map alt-dbl-tap-term-raw #'translate-to-localleader)

  ; I have a number of old wsl-specific duplicate keybindings
  ; before I added this, and I don't feel like cleaning them up,
  ; but try to rely on this from now on.
  ; Does not work for other key-translations however; we can't have duplicate
  ; translations, so those cannot be used
  ; here.
  ; If their respective translations do not handle this, we'll need to continue specifying both.
  ; Update as needed.
  (map! :map 'key-translation-map
        ctl-tap-wsl ctl-tap
        ctl-dbl-tap-wsl ctl-dbl-tap)
  (map! :map 'input-decode-map
        ctl-tap-term-raw (kbd ctl-tap)
        ctl-dbl-tap-term-raw (kbd ctl-dbl-tap))

  ; Based on evil-init-esc from evil-core.el
  ; My alt-taps don't work without it.
  (let ((esc-map (lookup-key input-decode-map [?\e])))
    (map! :map input-decode-map [?\e]
          `(menu-item "" ,esc-map :filter ,#'my-esc)))
  )
(unless (daemonp) (my-init-key-translation-maps))
(add-hook 'server-after-make-frame-hook #'my-init-key-translation-maps)
(add-hook 'kkp-terminal-setup-complete-hook #'my-init-key-translation-maps)

; Suppress evil's ESC handling in favor of our version.
(setq evil-intercept-esc nil)

(defun evil-exit-emacs-state-unless-god ()
  (interactive)
  (if (bound-and-true-p god-local-mode) (god-local-mode 0) (evil-exit-emacs-state)))

; I like the idea, but other than hjkl I just can't get
; used to the commands.
(map! :after evil
      ; :e ctl-tap-wsl #'evil-exit-emacs-state-unless-god
      ; :e ctl-tap #'evil-exit-emacs-state-unless-god
      :nm ctl-tap-wsl #'evil-emacs-state
      :nm ctl-tap #'evil-emacs-state
      :nm ctl-dbl-tap-wsl #'evil-emacs-state
      :nm ctl-dbl-tap #'evil-emacs-state
      :viro ctl-tap-wsl #'evil-escape
      :viro ctl-tap #'evil-escape
      )

; Created by Claude
;
; Works for visual modes, but now doesn't seem to work for text object commands.
; Replaced with mapping directly to a (combined) normal state keymap.
(defun my-evil-execute-in-normal-state ()
  "Execute a command in normal state, with special handling for visual modes.
 If the command enters visual or visual-block mode, call those commands
 directly to preserve proper state transitions."
  (interactive)
  (let ((original-state evil-state))
     ;; Temporarily switch to normal state
     (evil-normal-state)

     ;; Set the next state to return to after the command
     (setq evil-next-state original-state)

     ;; Read the key sequence
     (let* ((keys (read-key-sequence nil))
            (cmd (key-binding keys)))

       ;; Debug message
       ; (message "Keys: %s, Command: %s" keys cmd)

       ;; Check if this is a visual mode command
       (cond
        ;; Visual mode commands - call them directly from original state
        ((or (memq cmd '(evil-visual-char evil-visual-line evil-visual-block))
             (and (symbolp cmd)
                  (string-match-p "evil-visual" (symbol-name cmd))))
         ;; Return to original state first
         (funcall (intern (format "evil-%s-state" original-state)))
         ;; For visual block, it neds to be enteres specially,
         (if (eq cmd 'evil-visual-block)
             (evil-visual-state 'block)
           ;; Now call the visual command - it will handle state transition
           (call-interactively cmd)))

        ;; For all other commands, execute in normal state and return
        (cmd
         (funcall (intern (format "evil-%s-state" original-state)))
         (evil-execute-in-normal-state)
         (funcall cmd)
         )

        (t
         (execute-kbd-macro keys)
         ;; If we're still in normal state (didn't enter visual mode),
         ;; return to original state
         (when (eq evil-state 'normal)
           (funcall (intern (format "evil-%s-state" original-state)))
           ; Some versions had this, others didn't
           (setq evil-next-state nil)
           ))))))


;(map! :map evil-emacs-state-map "M-o" #'my-evil-execute-in-normal-state)
;(map! :map evil-emacs-state-map "<Launch5>" #'my-evil-execute-in-normal-state)
;(map! :map evil-emacs-state-map "<f14>" #'my-evil-execute-in-normal-state)

(map! "C-RET" #'cua-rectangle-mark-mode)
(map! "C-<return>" #'cua-rectangle-mark-mode)

(map! :leader
      (:prefix "b"
               "0" #'+workspace/close-window-or-workspace
               "1" #'delete-other-windows
               "2" #'split-window-below
               "3" #'split-window-right
               "4" ctl-x-4-map
               "5" ctl-x-5-map
               (:after 2C-mode
                       "6" 2C-mode-map)
               "8" iso-transl-ctl-x-8-map
               "o" #'other-window
               "d" #'dired)
      ; This was a good idea in theory,
      ; but in practice I am not using it, and it moved the doom scratch buffer
      ; to an inconvenient combination on a normal keyboard.
      ; Doom apparently wants you to use their scratch buffer instead of the built-in *scratch*.
      ; :leader b x is almost the same command but apparently not a toggle?
      ; Besides, this is messing up preferred bindings for C-x commands.
      ;:desc "C-x" "x" ctl-x-map
      ;"M-x" #'doom/open-scratch-buffer
      )

; Because why not.
(map! "C-x M-c M-b u t t e r f l y" #'butterfly)

; An initial attempt at creating shortcuts in rectangle mode.
; Deprecated in favor of speedrect.
;; (let ((map (lookup-key (current-global-map) (kbd "C-x r"))))
;;   (when (keymapp map)
;;     (map-keymap
;;       (lambda (event binding)
;;           (let ((key-description (single-key-description event)))
;;             (map! :map rectangle-mark-mode-map key-description binding))
;;         )
;;       map)))

(use-package repeat
  :custom
  (repeat-mode +1))

(use-package! speedrect
  :config
    (speedrect-mode)
  )

(defun update-evil-emacs-cursor ()
  (evil-set-cursor #'my-evil-emacs-cursor-fn)
  )

; Based on conditions in evil-esc from evil-core.el, but adds my own.
(setq my-esc-pred (lambda () (and
                         (not (bound-and-true-p boon-command-state))
                         (or (not (boundp 'evil-local-mode)) evil-local-mode (evil-ex-p)
                             (active-minibuffer-window))
                         (not (bound-and-true-p god-local-mode))
                         )))

(defun boon-exit-insert-state-unless-god ()
  (interactive)
  (if (bound-and-true-p god-local-mode) (god-local-mode 0) #'boon-quit))

; Boon is cool, but still needs a lot of configuration to work with anything else.
(use-package! boon
  :config
  (require 'boon-qwerty)

  ; We need to run our redirected bindings after boon-qwerty has been loaded.
  (provide 'my-redirect-evil-to-boon)

  ; powerline is an older modeline package, distinct from doom modeline.
  ; No point in running both.
  ;(require 'boon-powerline)
  ;(boon-powerline-theme)


  (defun my-special-mode-class-p ()
    (and (eq (get major-mode 'mode-class) 'special)
         (not (boon-shell-mode-p))
         (not (string-prefix-p "doom" (symbol-name major-mode)))
         (not (eq major-mode 'messages-buffer-mode)))
    )

  ; The normal implementation applies to any special mode, which includes lots of modes
  ; without custom keybindings.
  (defun boon-special-mode-p ()
  "Should the mode use `boon-special-state'? Less aggressive than original."
  (or ;(and (eq (get major-mode 'mode-class) 'special)
      ;     (not (boon-shell-mode-p))
      ;     (not (string-prefix-p "doom" (symbol-name major-mode))))
      (my-special-mode-class-p)
      (-some 'eval boon-special-conditions)
      ;(memq major-mode boon-special-mode-list)
      (derived-mode-p boon-special-mode-list)))

  (setq boon-insert-conditions '((and (not (string= (buffer-name (current-buffer)) "BOON-TUTORIAL"))
                                      (not (and (eq (get major-mode 'mode-class) 'special)
                                                (not (boon-shell-mode-p)))))))

  (after! evil
    ; Removing this in favor of boon usage.
    (defvar evil-backup-motion-state-modes evil-motion-state-modes)
    (setq evil-motion-state-modes nil)

    (defvar evil-backup-normal-state-modes evil-normal-state-modes)
    (setq evil-normal-state-modes nil)

    (advice-add 'evil-set-initial-state :around
                (lambda (orig-fn mode state)
                  (if (and state (eq state 'normal))
                      (add-to-list 'evil-backup-normal-state-modes mode)
                    (funcall orig-fn mode state))))

    (setq boon-insert-conditions `((or ,(car boon-insert-conditions)
                                       (derived-mode-p evil-backup-normal-state-modes))))

    (after! evil-collection
      (setq evil-normal-state-modes nil)

      (evil-set-initial-state 'messages-buffer-mode 'insert)

      ; This suppresses *Messages* from starting in evil normal mode.
      ; However, the default boon behavior is to put this in special mode,
      ; which has no keybindings.
      (with-current-buffer "*Messages*"
        (evil-change-to-initial-state)
        )
      )
    )

  (remove-hook 'minibuffer-setup-hook 'boon-minibuf-hook)

  (advice-add #'boon-update-cursor :around
              (lambda (_)
                "Do not rely on boon's update cursor"
                (when (and (eq (current-buffer) (window-buffer (selected-window)))
                           (eq evil-state 'emacs))
                  (update-evil-emacs-cursor))))

  ; Savehist gets extremely large if we allow boon to store changes in shells.
  (advice-add 'boon/after-change-hook :before-until
            (lambda (&rest _) (boon-shell-mode-p)))

  ; Override boon's behavior to not care if it's read-only.
  (defun boon-set-insert-state ()
    "Switch to insert state."
    (interactive)
    (boon-set-state 'boon-insert-state))

  ; Boon does not save the state normally, which I find annoying.
  (remove-hook 'window-selection-change-functions 'boon-reset-state-for-switchw)

  (boon-mode)
  (boon-insert)

  (defvar boon-map-property-alist
    '((command . boon-map)
      (insert . boon-insert-map)
      (special . boon-special-map)))
  (defvar boon-map-alist
    `((command . ,boon-command-map)
      (insert . ,boon-insert-map)
      (special . ,boon-special-map)))

  (defun make-boon-map (mode state &optional merge-with)
    (let* ((map (if (listp merge-with) (make-composed-keymap merge-with) (make-sparse-keymap)))
          (boon-map-property (cdr (assq state boon-map-property-alist)))
          (boon-map (if-let ((existing (get mode boon-map-property)))
                        existing
                     (cdr (assq state boon-map-alist)))))
      (set-keymap-parent map boon-map)
      (put mode boon-map-property map)
    ))

  (map! :map 'boon-moves-map
        "j" #'backward-char
        "l" #'forward-char
        "i" #'previous-line
        "k" #'next-line
        "u" #'boon-smarter-backward
        "o" #'boon-smarter-forward
        "h" #'boon-beginning-of-line
        ";" #'boon-end-of-line
        "m" #'avy-goto-word-1
        "p" nil
        "J" #'backward-paragraph
        "L" #'forward-paragraph
        "I" #'boon-smarter-upward
        "K" #'boon-smarter-downward
        "M" #'avy-goto-char
        "H" nil
        "O" nil)

  ; While these are clearly motions, boon itself places the scroll functions on the command map.
  ; Most likely this has something to do with the motion map being special for selectors.
  (map! :map 'boon-command-map
        "C-j" #'scroll-down-line
        "C-l" #'scroll-up-line
        "<C-i>" #'pixel-scroll-interpolate-up ; C-i is always equivalent to terminal TAB.
                                              ; Thus, picking a frequent command that already has
                                              ; a less reachable key, namely Page Up.
        "C-k" #'pixel-scroll-interpolate-down
        ; discourage use while in termimal
        )

  (map! :e ctl-tap #'boon-set-command-state)
  (map! :map 'boon-command-map ctl-tap #'boon-set-insert-like-state)
  (map! :map 'boon-command-map ctl-dbl-tap #'boon-set-insert-like-state)

  (defvar +dashboard-mode-boon-map (make-boon-map '+dashboard-mode 'command +dashboard-mode-map))
  (defvar +dashboard-mode-boon-special-map (make-boon-map '+dashboard-mode 'special +dashboard-mode-map))

  (map! :map (+dashboard-mode-boon-map +dashboard-mode-boon-special-map)
        "i" #'+dashboard/backward-button
        "k" #'+dashboard/forward-button)

  (map! :leader :desc "boon" alt-tap boon-command-map)
  (unless (modulep! :editor evil)
    (map! :e "M-o" boon-command-map)
    )

  ; I am not using evil mode much; more convenient for C-z to be boon when I only
  ; have a laptop keyboard.
  ; Can use <leader> z to access visual modes directly.
  (after! evil
    ; It is too late to set evil-toggle-key.
    ; For whatever reason, this fails if we try to set that too.
    (evil-set-toggle-key "C-M-z")

    ; To keep a consistent state, don't allow switching evil states from boon.
    (map! :map boon-command-map "C-M-z" #'ignore)
    )

  (map! (:after evil :nvomr "C-z" #'evil-emacs-state)
        (:map boon-insert-map
         :ei "C-z" #'boon-set-command-state)
        (:map boon-special-map
         :ei "C-z" #'boon-set-command-state)
        (:map boon-command-map
         "C-z" #'boon-set-insert-like-state))

  ; Redefine quit bindings to not overwrite evil

  (when (modulep! :editor evil)
    (map! :map boon-command-map boon-quit-key nil)
    (map! :map boon-command-map :ei boon-quit-key #'boon-quit)

    (map! :map boon-special-map boon-quit-key nil)
    (map! :map boon-special-map :ei boon-quit-key #'boon-set-command-state)

    (map! :map boon-insert-map boon-quit-key nil)
    (map! :map boon-insert-map :ei boon-quit-key #'boon-set-command-state)

    (map! :map global-map boon-quit-key nil)
    (map! :map global-map :ei boon-quit-key #'keyboard-quit)

    (map! :map minibuffer-local-map boon-quit-key nil)
    (map! :map minibuffer-local-map :ei boon-quit-key #'keyboard-quit)

    (map! :map minibuffer-local-ns-map boon-quit-key nil)
    (map! :map minibuffer-local-ns-map :ei boon-quit-key #'keyboard-quit)

    (map! :map minibuffer-local-completion-map boon-quit-key nil)
    (map! :map minibuffer-local-completion-map :ei boon-quit-key #'keyboard-quit)

    (map! :map minibuffer-local-must-match-map boon-quit-key nil)
    (map! :map minibuffer-local-must-match-map :ei boon-quit-key 'keyboard-quit)

    (map! :map isearch-mode-map boon-quit-key nil)
    (map! :map isearch-mode-map :ei boon-quit-key 'isearch-abort))
  )

(use-package! theist-mode
  :config
  (map! :leader :desc "C-c" "M-c" #'theist-C-c)
  )

;(map! :map '+dashboard-mode-map "i" #'+dashboard/backward-button)
;(map! :map '+dashboard-mode-map "k" #'+dashboard/forward-button)

; Copied and modified from module

(defvar +god-read-only-mode-color "Gray"
  "Cursor and bar color when `read-only-mode' is enabled.")

(defvar +god-overwrite-mode-color "Yellow"
  "Cursor and bar color when `overwrite-mode' is enabled.")

(defvar +god-fill-overflow-color "IndianRed"
  "Cursor and bar color when fill column width has been exceeded.")

(defun +god--toggle-on-overwrite-h ()
  (if (bound-and-true-p overwrite-mode)
      (god-local-mode-pause)
    (god-local-mode-resume)))

(defun +god--configure-modeline-h ()
  "Configure doom-modeline bar color depending on mode."
  (let* ((is-fill-overflow (> (current-column) fill-column))
         (previous-cursor-color (frame-parameter nil 'cursor-color))
         (previous-modeline-color (and (facep 'doom-modeline-bar)
                                       (face-background 'doom-modeline-bar)))
         (is-god-mode (bound-and-true-p god-local-mode))
         (next-modeline-color
          (cond (buffer-read-only +god-read-only-mode-color)
                (is-fill-overflow +god-fill-overflow-color)
                (overwrite-mode +god-overwrite-mode-color)
                ((and is-god-mode (or previous-cursor-color (face-background 'cursor))))
                (t 'unspecified))
          ))
    (when (and (facep 'doom-modeline-bar)
               (fboundp 'doom-modeline-refresh-bars)
               (not (eq previous-modeline-color next-modeline-color)))
      (set-face-attribute 'doom-modeline-bar nil :background next-modeline-color)
      (doom-modeline-refresh-bars))))

(use-package! god-mode
  :defer
  :config

  (add-hook! 'god-mode-enabled-hook #'+god--configure-modeline-h)
  (add-hook! 'god-mode-disabled-hook #'+god--configure-modeline-h)

  (add-hook! 'overwrite-mode-hook #'+god--toggle-on-overwrite-h)

  (after! evil
    (add-hook! 'god-mode-enabled-hook #'update-evil-emacs-cursor)
    (add-hook! 'god-mode-disabled-hook #'update-evil-emacs-cursor))

  ; Not really what god mode is meant for, but I like this.
  ; Easier than trying to learn evil mode just to use hjkl.
  (map! :map god-local-mode-map
        (:after evil
         "j" #'evil-next-line
         "k" #'evil-previous-line
         "h" #'evil-backward-char
         "l" #'evil-forward-char)
        ; Doesn't seem to override other custom shortcuts
        ; Make sure to configure taps in other maps to not change the evil state if in god mode.
        :g ctl-dbl-tap-wsl #'god-local-mode
        :g ctl-dbl-tap #'god-local-mode
        :g ctl-tap-wsl #'god-local-mode
        :g ctl-tap #'god-local-mode
        ; Normally would be used to switch evil states, which we don't want from here.
        ; Instead, let's use it to get back the lost kill-line.
        "z" #'kill-line
        )

  (after! which-key
    (which-key-enable-god-mode-support)))

(map! :e ctl-dbl-tap-wsl #'god-local-mode
      :e ctl-dbl-tap #'god-local-mode)

(defun +set-cursor-color (color)
  (unless (eq color (frame-parameter nil 'cursor-color))
    (set-cursor-color color)))

(defun my-evil-emacs-cursor-fn ()
  "Configure cursor type and color and doom-modeline bar color depending on mode."
  (let* ((is-fill-overflow (> (current-column) fill-column))
         (is-god-mode (bound-and-true-p god-local-mode))
         (next-cursor-type
          (cond (buffer-read-only 'box)
                ((and overwrite-mode is-god-mode) 'hollow)
                ((bound-and-true-p boon-command-state) boon-command-cursor-type)
                ((bound-and-true-p boon-special-state) boon-special-cursor-type)
                ((or is-god-mode overwrite-mode) 'box)
                (t 'bar))))
    (unless (eq cursor-type next-cursor-type)
      (setq cursor-type next-cursor-type))
    (cond (buffer-read-only (evil-set-cursor-color +god-read-only-mode-color))
          (is-fill-overflow (evil-set-cursor-color +god-fill-overflow-color))
          ((and is-god-mode overwrite-mode) (evil-set-cursor-color +god-overwrite-mode-color))
          (is-god-mode (+evil-default-cursor-fn))
          ((bound-and-true-p boon-special-state) (evil-set-cursor-color boon-special-cursor-color))
          ((bound-and-true-p boon-command-state) (+evil-default-cursor-fn))
          (t (+evil-emacs-cursor-fn)))
  ))

(after! undo-tree
  ; Terminal accomodation, C-? means backspace
  ; Conveniently currenly mapped to emacs undo-redo in the terminal only.
  ; Confusingly, terminal C-M-_ is actually C-M-/
  (map! :unless (display-graphic-p)
        "C-M-_" #'undo-tree-redo)
  )

; Generated by Claude Code, except for :leader j
;;; Avy - configured but entirely unbound in +emacs-bindings.el.
;;; Evil users get this through gs+evilem; emacs users get zero bindings.
;;; M-g (goto-map) is the Emacs-idiomatic home for navigation commands.
(map! ;(:after avy
       (:leader "j" #'avy-goto-char-timer)
       ; Unsure about this one - some modes will overide this.
       "M-j" #'avy-goto-char-timer
       (:map goto-map                    ; M-g prefix
        "j"   #'avy-goto-char-timer     ; M-g j — type to narrow (= gs SPC / gs   / in evil)
        "J"   #'avy-goto-char-2         ; M-g J — 2-char jump      (= gs s in evil)
        "l"   #'avy-goto-line           ; M-g l
        "w"   #'avy-goto-word-1         ; M-g w
        "W"   #'avy-goto-symbol-1)      ; M-g W — jump to symbol by first char
       ;)

      ;; Jump to an isearch match with avy (pick one from all highlights)
      (:after isearch
       :map isearch-mode-map
       "M-j" #'avy-isearch))

;(after! avy
  ; Interesting, but due to the below issue can't really use it.
  ;(setq avy-style 'de-bruijn)
  ;(when (eq avy-style 'de-bruijn)
  ;  ; Doom sets this to nil, but that causes issues with de-bruijn
  ;  (setq avy-single-candidate-jump t))
;  )


(use-package! ligature
  :load-path "path-to-ligature-repo"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                        '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                          ;; =:= =!=
                          ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                          ;; ;; ;;;
                          (";" (rx (+ ";")))
                          ;; && &&&
                          ("&" (rx (+ "&")))
                          ;; !! !!! !. !: !!. != !== !~
                          ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                          ;; ?? ??? ?:  ?=  ?.
                          ("?" (rx (or ":" "=" "\." (+ "?"))))
                          ;; %% %%%
                          ("%" (rx (+ "%")))
                          ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                          ;; |->>-||-<<-| |- |== ||=||
                          ;; |==>>==<<==<=>==//==/=!==:===>
                          ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                          "-" "=" ))))
                          ;; \\ \\\ \/
                          ("\\" (rx (or "/" (+ "\\"))))
                          ;; ++ +++ ++++ +>
                          ("+" (rx (or ">" (+ "+"))))
                          ;; :: ::: :::: :> :< := :// ::=
                          (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                          ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                          ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                          "="))))
                          ;; .. ... .... .= .- .? ..= ..<
                          ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                          ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                          ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                          ;; *> */ *)  ** *** ****
                          ("*" (rx (or ">" "/" ")" (+ "*"))))
                          ;; www wwww
                          ("w" (rx (+ "w")))
                          ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                          ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                          ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                          ;; << <<< <<<<
                          ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                          "-"  "/" "|" "="))))
                          ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                          ;; >> >>> >>>>
                          (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                          ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                          ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                       (+ "#"))))
                          ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                          ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                          ;; __ ___ ____ _|_ __|____|_
                          ("_" (rx (+ (or "_" "|"))))
                          ;; Fira code: 0xFF 0x12
                          ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                          ;; Fira code:
                          "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                          ;; The few not covered by the regexps.
                          "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; No longer necessary as Doom's Agda module includes this.
;(load! (let ((coding-system-for-read 'utf-8))
;         (shell-command-to-string "agda-mode locate")))

(after! evil
  (setq evil-disable-insert-state-bindings t)
  (setq evil-default-state 'emacs)

  ; This is not recommended compared to disable-insert-state-bindings,
  ; but as an emacs user first, I prefer not to have insert state.
  (defalias 'evil-insert-state 'evil-emacs-state)

  (map! :nm "C-e" #'doom/forward-to-last-non-comment-or-eol)
  (map! :nm "C-m" #'evil-scroll-line-down)

  (setq evil-combined-normal-state-map (make-composed-keymap evil-normal-state-map evil-motion-state-map))
  (map! :e "M-o" #'evil-execute-in-normal-state)
  (map! :leader :desc "vim" "z" evil-combined-normal-state-map)

  (setq evil-lbrace-map (lookup-key evil-combined-normal-state-map (kbd "[")))
  (setq evil-rbrace-map (lookup-key evil-combined-normal-state-map (kbd "[")))

  (map! :leader :desc "[" "[" evil-lbrace-map)
  (map! :leader :desc "]" "]" evil-rbrace-map)

  ; Matches modern editors.
  (setq evil-emacs-state-cursor #'my-evil-emacs-cursor-fn)
  (setq evil-motion-state-cursor evil-normal-state-cursor)
  )

(add-hook! 'overwrite-mode-hook
  (when (eq evil-state 'emacs)
    (+god--configure-modeline-h)
    (evil-set-cursor #'update-evil-emacs-cursor)
    ))

(after! dired (remove-hook 'dired-mode-hook 'dired-omit-mode))

(defun doom/ediff-init-and-example ()
  "ediff the current `init.el' with the example in doom-emacs-dir"
  (interactive)
  (ediff-files (concat doom-user-dir "init.el")
               (concat doom-emacs-dir "static/init.example.el")))

(define-key! help-map
  "di"   #'doom/ediff-init-and-example
  )

(after! company
  (map! :e "C-\t" #'company-complete
        :i "C-\t" #'company-complete
        :e "C-." #'company-complete
        :i "C-." #'company-complete))

(after! corfu

  (defun my/corfu-next-or-down (&optional n)
    "Move to next candidate, or quit if only one candidate exists.
  Falls through to whatever <down> is bound to outside corfu-map."
    (interactive "p")
    (if (= (length corfu--candidates) 1)
        (progn
          (corfu-quit)
          (call-interactively (or (key-binding (kbd "<down>")) #'next-line)))
      (corfu-next n)))

  (defun my/corfu-prev-or-up (&optional n)
    "Move to prev candidate, or quit if only one candidate exists.
  Falls through to whatever <up> is bound to outside corfu-map."
    (interactive "p")
    (if (= (length corfu--candidates) 1)
        (progn
          (corfu-quit)
          (call-interactively (or (key-binding (kbd "<up>")) #'previous-line)))
      (corfu-previous n)))

  ; Solution found by Claude - if we don't do this, Corfu won't
  ; register my modified commands as navigation.
  (add-to-list 'corfu-continue-commands 'my/corfu-next-or-down)
  (add-to-list 'corfu-continue-commands 'my/corfu-prev-or-up)

  ; To be consistent with corfu-next/prev, hide from M-x
  (put #'my/corfu-next-or-down 'completion-predicate #'ignore)
  (put #'my/corfu-prev-or-up 'completion-predicate #'ignore)

  (map! :ei "C-." #'completion-at-point)
  (map! :map 'corfu-map
        "C-TAB" #'corfu-reset
        "C-<tab>" #'corfu-reset
        "<down>" #'my/corfu-next-or-down
        "<up>" #'my/corfu-prev-or-up
        :e "C-\r" #'corfu-quit
        :e "C-<return>" #'corfu-quit
        :e ctl-tap #'corfu-quit
        :e ctl-tap-wsl #'corfu-quit
        :ei "C-." #'corfu-insert-separator)

  ; Make <escape> in corfu popup quit the popup
  ; https://github.com/emacs-evil/evil-collection/issues/676#issuecomment-1604386513
  (defvar my-corfu-override-keymap-alist '())
  (add-to-ordered-list 'emulation-mode-map-alists 'my-corfu-override-keymap-alist 0)
  (add-hook 'my-corfu-override-keymap-alist
            `(completion-in-region-mode . ,(define-keymap "<escape>" #'corfu-quit)))

  (setq corfu-max-width 80)

  (setq corfu-preselect 'valid)
  (setq +corfu-want-ret-to-confirm 't)
  (setq corfu-quit-at-boundary t)
  (setq corfu-quit-no-match t)

  ; The default is a bit too aggressive
  (setq corfu-auto-delay 0.48)

  (setq-hook! 'org-mode-hook corfu-auto-delay 1.5
                             corfu-auto-prefix 4)
  )

(after! lsp-mode
  ; lsp-ui-doc has issues for me on wslg.
  ; Using eldoc-box instead; see below for that config
  (setq lsp-ui-doc-enable nil)

  (setq lsp-ui-doc-include-signature t)

  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-use-webkit nil)

  (setq lsp-ui-doc-alignment 'window)

  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-border (face-foreground 'default))

  (setq lsp-eldoc-enable-hover t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-render-documentation t)
  (setq lsp-eldoc-render-all t)

  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t)

  ; If I were going to keep this menu onscreen always, this is what I would set the width to.
  ; But, I'd rather have a code map I sometimes can call up.
  ; Not sure if lsp-ui-imenu is the best means for that.
  ; (setq lsp-ui-imenu-window-width 25)

  ; Supposedly slow, but sometimes useful. No indicator though.
  (setq lsp-enable-folding nil)

  ;(map! "<f12>" #'lsp-ui-doc-glance)
  (map! "<f12>" #'eldoc-box-help-at-point)
  (map! "S-<f12>" #'lsp-describe-thing-at-point)

  (setq lsp-inlay-hint-enable t)
  (setq lsp-lens-enable t)
  (custom-set-faces!
    '(lsp-inlay-hint-face :foreground "dim gray" :height 0.9 :slant italic))

  (setq lsp-idle-delay 0.6)

  (setq lsp-response-timeout 30)
  (setq lsp-enable-file-watchers nil)

  (setq lsp-log-io nil)

  ; Doubt we still need this
  (setq lsp-debounce-full-sync-notifications-interval 2.0)


  (defvar my-lsp-startup-time nil)

  (add-hook! 'lsp-before-initialize-hook (setq my-lsp-startup-time (current-time)))

  (advice-add 'lsp--error :around
              (lambda (orig-fn msg &rest args)
                (unless (and my-lsp-startup-time
                             (<= (float-time (time-subtract (current-time) my-lsp-startup-time)) 5))

                  (apply orig-fn msg args))))

  ; In this function and its peers, the core message is actually the first element of args,
  ; but in lsp--message it becomes the second element.
  ; Note that info, warn, and error have green, yellow, and red faces respectively.
  (advice-add 'lsp--warn :around
              (lambda (orig-fn format-string &rest args)
                (unless (and (>= (length args) 1)
                              (let ((s (seq-elt args 0)))
                                (and s (stringp s)
                                     (string-match-p "content modified" s))))
                  (apply orig-fn format-string args))))

  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  )

(after! eglot
  (map! "<f12>" #'eldoc-box-help-at-point))

(use-package! eldoc-box
  :after (:any lsp-mode eglot)
  :config
  (global-eldoc-mode +1)

  ;; Eldoc displays in echo area/minibuffer by default
  ;; This gives you the quick, non-intrusive help
  (setq eldoc-echo-area-use-multiline-p 1)  ; Allow multiple lines in echo
  (setq eldoc-echo-area-prefer-doc-buffer nil)  ; Keep it in echo area

  (setq eldoc-documentation-strategy #'eldoc-documentation-compose)

  (setq eldoc-box-clear-with-C-g t)
  (setq eldoc-box-only-multi-line t)

  (custom-set-faces!
    `(eldoc-box-border :background ,(face-foreground 'default)))
)

(setq select-active-regions nil)

;(setq initial-frame-alist
;      (append initial-frame-alist '(
;        (left . ???)
;        (top . ???)
;        (width . 80)
;        (height . 80))))

;; https://old.reddit.com/r/emacs/comments/lwklvl/how_can_i_change_the_initial_window_size_of_emacs/
;; Set initial frame size and position
(defun my/set-initial-frame (base-factor-width base-factor-height)
  (let* (
    (a-width (* (display-pixel-width) base-factor-width))
        (a-height (* (display-pixel-height) base-factor-height))
        (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
    (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))

;; Moved into local files.
;(if (display-graphic-p)
;    (progn
;      (setq frame-resize-pixelwise t)
;      (my/set-initial-frame 0.30 0.35)))

; Updated version
(defun my/default-frame-dims (base-factor-width base-factor-height)
  (let* (
         (a-width (* (display-pixel-width) base-factor-width))
         (a-height (* (display-pixel-height) base-factor-height))
         (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
         (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))
    `(
      (left . ,(truncate a-left))
      (top . ,(truncate a-top))
      (width . (text-pixels . ,(truncate a-width)))
      (height . (text-pixels . ,(truncate a-height))))))

(defun my/set-frame-dims (dims)
  (setq frame-resize-pixelwise t)
  (let ((left (truncate (alist-get 'left dims)))
        (top (truncate (alist-get 'top dims)))
        (width (truncate (cdr (alist-get 'width dims))))
        (height (truncate (cdr (alist-get 'height dims)))))
    (set-frame-position (selected-frame) left top)
    (set-frame-size (selected-frame) width height t)))

(defvar my/base-factor-width 0.30 "Percentage of screen to set default frame width to")
(defvar my/base-factor-height 0.35 "Percentage of screen to set default frame height to")

; Set the base factor vars and call this function in the local config.
(defun my/setup-frame-dims ()
  (defun my/setup-frame-dims--apply ()
    (let ((dims (my/default-frame-dims my/base-factor-width my/base-factor-height)))
      (my/set-frame-dims dims)
      (setf (alist-get (window-system) window-system-default-frame-alist) dims))
    )

  (setq frame-resize-pixelwise t)
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook
                (defun my/setup-frame-dims-late (&optional _)
                  (when (display-graphic-p)
                    (my/setup-frame-dims--apply)
                    (remove-hook 'server-after-make-frame-hook #'my/setup-frame-size-late))))
    (when (display-graphic-p)
      (my/setup-frame-dims--apply)
      ;(add-hook 'window-setup-hook
      ;          (defun my/setup-frame-dims-late (&optional _)
      ;            (when (display-mouse-p)
      ;              (my/setup-frame-dims--apply)
      ;              (remove-hook 'window-setup-hook #'my/setup-frame-size-late))
      ;            ))
      )))

(c-add-style "allman"
             '("k&r"
               (c-basic-offset 4)
               (indent-tabs-mode . nil)))

;(after! cc-mode
;  (setq c-default-style "allman"))

; Disables rebase mode, if desired.
;(use-package! magit
;  :defer t
;  :init
;  (setq auto-mode-alist (rassq-delete-all 'git-rebase-mode auto-mode-alist))
;  :config
;  (setq auto-mode-alist (rassq-delete-all 'git-rebase-mode auto-mode-alist)))

(when (cl-some (lambda (arg)
           (string-match-p "ediff" arg))
         command-line-args)
  ;(remove-hook 'window-setup-hook #'doom-init-ui-h)
  (remove-hook 'doom-init-ui-hook #'+dashboard-init-h)
)

(after! ediff
  (setq ediff-window-setup-function 'my/ediff-setup-windows-plain)
  ;(setq ediff-merge-split-window-function 'split-window-vertically)
)

(defun my/ediff-setup-windows-plain (buf-A buf-B buf-C control-buffer)
  (if (and ediff-show-ancestor (with-current-buffer control-buffer
                                 ediff-merge-with-ancestor-job)) ; with-Ancestor-p)
      (my/ediff-setup-windows-plain-merge-baseleft buf-A buf-B buf-C control-buffer)
      (ediff-setup-windows-plain buf-A buf-B buf-C control-buffer)))

; https://news.ycombinator.com/item?id=34557827
; Does (A|B|C)/D.
(defun my/ediff-setup-windows-plain-merge-basemid (buf-A buf-B buf-C control-buffer)
      ;; skip dedicated and unsplittable frames
      (ediff-destroy-control-frame control-buffer)
      (let ((window-min-height 1)
            (with-Ancestor-p (with-current-buffer control-buffer
                               ediff-merge-with-ancestor-job))
            split-window-function
            merge-window-share merge-window-lines
            (buf-Ancestor (with-current-buffer control-buffer
                            ediff-ancestor-buffer))
            wind-A wind-B wind-C wind-Ancestor)
        (with-current-buffer control-buffer
          (setq merge-window-share ediff-merge-window-share
                ;; this lets us have local versions of ediff-split-window-function
                split-window-function ediff-split-window-function))
        (delete-other-windows)
        (set-window-dedicated-p (selected-window) nil)
        (split-window-vertically)
        (ediff-select-lowest-window)
        (ediff-setup-control-buffer control-buffer)

        ;; go to the upper window and split it betw A, B, and possibly C
        (other-window 1)
        (setq merge-window-lines
              (max 2 (round (* (window-height) merge-window-share))))
        (switch-to-buffer buf-A)
        (setq wind-A (selected-window))

        (split-window-vertically (max 2 (- (window-height) merge-window-lines)))
        (if (eq (selected-window) wind-A)
            (other-window 1))

        (setq wind-C (selected-window))
        (switch-to-buffer buf-C)

        (select-window wind-A)
        (funcall split-window-function)

        (if (eq (selected-window) wind-A)
            (other-window 1))
        (switch-to-buffer buf-B)
        (setq wind-B (selected-window))

        (when (and ediff-show-ancestor with-Ancestor-p)
          (select-window wind-A)
          (split-window-horizontally)
          (when (eq (selected-window) wind-A)
            (other-window 1))

          (switch-to-buffer buf-Ancestor)
          (setq wind-Ancestor (selected-window)))

        (balance-windows-area)

        (with-current-buffer control-buffer
          (setq ediff-window-A wind-A
                ediff-window-B wind-B
                ediff-window-C wind-C
                ediff-window-Ancestor wind-Ancestor))

        (ediff-select-lowest-window)
        (minimize-window)
        (ediff-setup-control-buffer control-buffer)
        ))

(defun my/ediff-setup-windows-plain-merge-baseleft (buf-A buf-B buf-C control-buffer)
      ;; skip dedicated and unsplittable frames
      (ediff-destroy-control-frame control-buffer)
      (let ((window-min-height 1)
            (with-Ancestor-p (with-current-buffer control-buffer
                               ediff-merge-with-ancestor-job))
            split-window-function
            merge-window-share merge-window-lines
            (buf-Ancestor (with-current-buffer control-buffer
                            ediff-ancestor-buffer))
            wind-A wind-B wind-C wind-Ancestor wind-topleft)
        (with-current-buffer control-buffer
          (setq merge-window-share ediff-merge-window-share
                ;; this lets us have local versions of ediff-split-window-function
                split-window-function ediff-split-window-function))
        (delete-other-windows)
        (set-window-dedicated-p (selected-window) nil)
        (split-window-vertically)
        (ediff-select-lowest-window)
        (ediff-setup-control-buffer control-buffer)

        ;; go to the upper window and split it betw A, B, and possibly C
        (other-window 1)
        (setq merge-window-lines
              (max 2 (round (* (window-height) merge-window-share))))
        (setq wind-topleft (selected-window))

        (split-window-vertically (max 2 (- (window-height) merge-window-lines)))

        ; This was originally at the end, but I prefer this on the left.
        ; Some necessary modification.
        (when (and ediff-show-ancestor with-Ancestor-p)
          (switch-to-buffer buf-Ancestor)
          (setq wind-Ancestor wind-topleft)

          ;(select-window wind-A)
          (split-window-horizontally)
          (when (eq (selected-window) wind-Ancestor)
            (other-window 1))
          )

        (switch-to-buffer buf-A)
        (setq wind-A (selected-window))

        (if (eq (selected-window) wind-A)
            (other-window 1))

        (setq wind-C (selected-window))
        (switch-to-buffer buf-C)

        (select-window wind-A)
        (funcall split-window-function)

        (if (eq (selected-window) wind-A)
            (other-window 1))
        (switch-to-buffer buf-B)
        (setq wind-B (selected-window))

        (balance-windows-area)

        (with-current-buffer control-buffer
          (setq ediff-window-A wind-A
                ediff-window-B wind-B
                ediff-window-C wind-C
                ediff-window-Ancestor wind-Ancestor))

        (ediff-select-lowest-window)
        (minimize-window)
        (ediff-setup-control-buffer control-buffer)
        ))

(use-package! google-c-style
  :hook (c-mode-common-hook . google-set-c-style)
  :hook (c-mode-common-hook . google-make-newline-indent)
)

(after! cc-mode
  (set-ligatures! '(c-mode c++-mode c-ts-mode c++-ts-mode) nil)
  (set-ligatures! '(c-mode c++-mode c-ts-mode c++-ts-mode)
;   Copied from cc package, with ones I don't want commented out
    :null "nullptr"
    :true "true" :false "false"
    ; :int "int" :float "float"
    ; :str "std::string"
    ; :bool "bool"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    ; :for "for"
    :return "return"
    :yield "#require"
    )
  )

(after! rustic
  (set-ligatures! '(rust-mode rustic-mode)
;   Copied from cc package, with ones I don't want commented out
    :true "true" :false "false"
    ;; Flow
    :and "&&" :or "||"
    )
  )


(after! rustic
  (setq lsp-rust-analyzer-import-granularity "module")

  (setq lsp-rust-analyzer-lens-references-trait-enable t)
  (setq lsp-rust-analyzer-lens-references-method-enable t)
  (setq lsp-rust-analyzer-lens-references-adt-enable t)
  ; Not entirely decided on this; supposedly overlaps with trait references
  ; and I don't feel I've gotten a lot of use out of this.
  (setq lsp-rust-analyzer-lens-implementations-enable t)
  )

(after! (rustic lsp-mode)
  (defun my-lsp-rust-analyzer-debug (runnable)
    (interactive (list (lsp-rust-analyzer--select-runnable)))

    (use-package! dape :demand t)

    (-let (((&rust-analyzer:Runnable
             :args (&rust-analyzer:RunnableArgs :cargo-args :workspace-root? :executable-args)
             :label) runnable))
      (pcase (aref cargo-args 0)
        ("run" (aset cargo-args 0 "build"))
        ("test" (when (-contains? (append cargo-args ()) "--no-run")
                  (cl-callf append cargo-args (list "--no-run")))))
      (->> (append (list (executable-find "cargo"))
                   cargo-args
                   (list "--message-format=json"))
           (s-join " ")
           (shell-command-to-string)
           (s-lines)
           (-keep (lambda (s)
                    (condition-case nil
                        (-let* ((json-object-type 'plist)
                                ((msg &as &plist :reason :executable) (json-read-from-string s)))
                          (when (and executable (string= "compiler-artifact" reason))
                            executable))
                      (error))))
         (funcall
          (lambda (artifact-spec)
            (pcase artifact-spec
              (`() (user-error "No compilation artifacts or obtaining the runnable artifacts failed"))
              (`(,spec) spec)
              (_ (user-error "Multiple compilation artifacts are not supported")))))
         (let ((config (dape--config-eval 'codelldb-rust `(:args ,executable-args))))
           (dape config)))))

  (advice-add #'lsp-rust-analyzer-debug :around (lambda (_ runnable) (my-lsp-rust-analyzer-debug runnable)))
  )

(setq org-return-follows-link t)

(setq which-key-show-transient-maps t)

(setq which-key-paging-prefixes '(
                                  "C-x"
                                  "C-x r"
                                  "C-x t"
                                  "C-x v"
                                  "C-x w"
                                  "C-h"
                                  "<ESC>"))

(after! which-key
  ; Almost the same as above + which-key-paging-key, but works with leader key.
  ; It does however require which-key-paging-key to already be set, and thus depends
  ; on which-key already being loaded.
  (defun map-which-key-paging-leader-prefixes (keys &optional map)
    (seq-doseq (key keys)
      (let ((chord (if (string-empty-p key) which-key-paging-key (concat key " " which-key-paging-key)))
            (map-or-default (or map 'which-key-mode-map)))
        (map! :leader :map map-or-default chord #'which-key-C-h-dispatch))))

  (defmacro map-which-key-paging-localleader-prefixes (mode-map keys &optional package)
    (let ((package-list (or package '())))
      `(after! ,package-list
         (seq-doseq (key ,keys)
            (let ((chord (if (string-empty-p key) which-key-paging-key (concat key " " which-key-paging-key))))
              (map! :localleader :map ,mode-map chord #'which-key-C-h-dispatch))))))

  (map-which-key-paging-leader-prefixes `("" "h" "p" "w" "b" "z" "[" "]" ,alt-tap))

  (map-which-key-paging-localleader-prefixes org-mode-map '("") org)
  (map-which-key-paging-leader-prefixes '("C-v") 'org-mode-map)
)

(after! '(org evil)
  (advice-add #'org-mode-restart :around
              (lambda (orig-fn)
                (let ((current-evil-state evil-state))
                  (funcall orig-fn)
                  (unless (eq current-evil-state evil-state)
                      (evil-change-state current-evil-state)))))
  )

(after! cc-mode
  ; Resolve tree-sitter bugs
  (add-to-list 'major-mode-remap-alist '(c-mode . nil))
  (add-to-list 'major-mode-remap-alist '(c++-mode . nil))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . nil)))

; Having issues with it hanging.
(set-file-template! 'gitignore-mode :ignore t)


(defun my-suppress-popup (pred)
  (push `(,pred display-buffer-no-window (allow-no-window . t))
        +popup--display-buffer-alist)
  )

(defun add-to-display-buffer-alist (entry)
  (push entry +popup--display-buffer-alist))

(defun my-rust-dap-program ()
  (concat (projectile-project-root) "target/debug/" (projectile-project-name))
  )

(after! dap-mode
  (dap-mode 1)
  (require 'dap-codelldb)
  (require 'dap-lldb)
  (require 'dap-gdb)

  (setq dap-auto-configure-features '(sessions locals breakpoints))

  ; For some reason, the codelldp I downloaded for dape almost always has issues with dap,
  ; with the panes not showing up and always the 'process filter' 'malformed message' error.
  ;(setq dap-codelldb-debug-program "~/.doom.d/debug-adapters/codelldb/extension/adapter/codelldb")
  (setq dap-codelldb-debug-path "~/.doom-emacs.d/.local/etc/dap-extension/vscode/codelldb")
  (dap-codelldb-setup)

  (setq dap-lldb-debug-program '("/usr/sbin/lldb-dap"))

  (dap-register-debug-template "Rust::LLDB::Run"
                               (list :type "lldb"
                                     :request "launch"
                                     :name "Rust::Run"
                                     :console "integratedTerminal" ; "internalConsole"; "externalTerminal"
                                     :program (my-rust-dap-program)
                                     :cwd (projectile-project-root)
                                     :stopOnEntry nil
                                     :args nil))

  (dap-register-debug-template "Rust::LLDB-DAP::Run"
                               (list :type "lldb-vscode"
                                     :request "launch"
                                     :name "Rust::LLDB-DAP::Run"
                                     ;:console "integratedTerminal" ; "internalConsole"; "externalTerminal"
                                     :program (my-rust-dap-program)
                                     :cwd (projectile-project-root)
                                     ;:stopOnEntry nil
                                     :initCommands '("command script import ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/etc/lldb_lookup.py"
                                                     "command source ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/etc/lldb_commands")))

  (dap-register-debug-template "Rust::GDB::Run"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "Rust::GDB::Run"
                                   :dap-server-path '("rust-gdb" "-i" "dap")
                                   :gdbpath "gdb"
                                   :target (my-rust-dap-program)
                                   :cwd (projectile-project-root)))

  (map! :map '+dap-running-session-mode-map
        "<f8>" #'dap-next
        "<f9>" #'dap-step-in
        "<f6>" #'dap-step-out
        "<f5>" #'dap-continue)

  (setq dap-ui-buffer-configurations
        `(("*dap-ui-locals*" (side . right) (slot . 1) (window-width . 0.2))
          ("*dap-ui-expressions*" (side . right) (slot . 2) (window-width . 0.2))
          ("*dap-ui-sessions*" (side . right) (slot . 3) (window-width . 0.2))
          ("*dap-ui-breakpoints*" (side . right) (slot . 4) (window-width . 0.2))
          (,dap-ui--debug-window-buffer (side . bottom) (slot . 5) (window-height . 0.2) )
          ("*dap-ui-repl*" (side . bottom) (slot . 1) (window-height . 0.2)))
        )


  (setq dap-external-terminal '("wt.exe" "--title" "dap-mode"
                                "wsl" "-e" "sh" "-c" "{command}"))

  (setq dap-internal-terminal #'dap-internal-terminal-vterm)

  (my-suppress-popup "\\* Rust::Run log\\*\\'")
  (set-popup-rule! "\\*Rust::Run - Rust::Run\\*\\'"
    :height 0.2
    :actions '(display-buffer-below-selected))
)

(defvar my-dape-active-mode-map (make-sparse-keymap) "My custom keymap while debugging")

(after! dape
  (add-to-list 'minor-mode-map-alist (cons 'dape-active-mode my-dape-active-mode-map))

  (map! :map 'my-dape-active-mode-map
        "<f8>" #'dape-next
        "<f9>" #'dape-step-in
        "<f6>" #'dape-step-out
        "<f5>" #'dape-continue)

  (defun my-dape-add-to-config (existing-config new-prop new-value)
    (let* ((cell (assoc existing-config dape-configs))
           (new (copy-sequence (cdr cell)))
           (new (plist-put new new-prop new-value)))
      (setf (alist-get existing-config dape-configs) new)))

  (defun my-dape-new-config (base-config new-name)
      (let* ((cell (assoc base-config dape-configs))
             (new (cl-copy-seq (cdr cell))))
        (setf (alist-get new-name dape-configs) new)))

  ; Replacing Rust's built in pretty printing with an alternate version.
  (my-dape-add-to-config 'codelldb-rust 'command-args
                         '("--port" :autoport))
  (my-dape-add-to-config 'codelldb-rust :initCommands ["command script import ~/downloads/rust-prettifier-for-lldb/rust_prettifier_for_lldb.py"])

  ;; Save breakpoints on quit
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
  ;; Load breakpoints on startup
  (dape-breakpoint-load)
  )


; If opening a directory that is detected to be a project
; (as all git repos should be),
; I want to automatically open a workspace associated with it.


(setq +workspaces-switch-project-function #'dired)

(defvar my-session-file nil)

(defun my/project-session-key (project-root)
  (secure-hash 'sha1 (file-truename project-root))
  )

(defun my/session-file (project-name project-root)
  (let* ((dir (file-name-directory (doom-session-file))))
    (concat dir project-name "-" (my/project-session-key project-root)))
  )

(add-hook! 'window-setup-hook
  (defun my-init-project-workspace ()
    (when (and (stringp dired-directory) (equal (doom-project-root) dired-directory))
      (setq my-session-file (my/session-file (doom-project-name) (doom-project-root)))
      (when (file-exists-p my-session-file)
        (doom/load-session my-session-file)
        )
      (+workspaces-switch-to-project-h))
    (remove-hook 'window-setup-hook #'my-init-project-workspace)))

(add-hook! '(find-file-hook kill-emacs-hook delete-frame-functions)
  (when my-session-file
    (doom/save-session my-session-file)))

;(add-hook! 'doom-first-file-hook
;  ;(when-let ((name (doom-project-name)))
;  (when (equal (doom-project-root) dired-directory)
;    (let ((dir dired-directory))
;      (add-hook! 'window-setup-hook
;        ;(+workspace-switch (doom-project-name) t)
;        ;(dired dir)))))
;        (defun my-init-workspace ()
;          (+workspaces-switch-to-project-h)
;          (remove-hook 'window-setup-hook #'my-init-workspace))))))
    ; Modified from +workspace-switch
    ;(unless (+workspace-exists-p name)
    ;  (+workspace-new name))
    ;(let ((old-name (+workspace-current-name)))
    ;  (unless (equal old-name name)
    ;    (setq +workspace--last
    ;          (or (and (not (+workspace--protected-p old-name))
    ;                   old-name)
    ;              +workspaces-main))
    ;    ;(persp-frame-switch name)
    ;    )
    ;  (equal (+workspace-current-name) name))))


(setq lsp-haskell-formatting-provider "fourmolu")
; My results were not great with this.
;(setq haskell-ts-use-indent t)

(after! haskell-mode
  (map! :localleader
        :map haskell-mode-map
        "b" #'haskell-compile)
  )

(after! haskell-ts-mode
  (map! :localleader
        :map haskell-ts-mode-map
        ; This is haskell-mode's; haskell-ts-mode doesn't have much of a separate command
        "b" #'haskell-compile
        "B" #'haskell-ts-compile-region-and-go
        "r" #'run-haskell
        "C" #'haskell-cabal-visit-file
        )
  )

(map! :leader :prefix "o"
      (:when (modulep! :term ghostel)
        :desc "Toggle ghostel popup" "t" #'+ghostel/toggle
        :desc "Open ghostel here"    "T" #'+ghostel/here)
      )

(after! ghostel
  (defvar ghostel-boon-insert-map (make-boon-map 'ghostel-mode 'insert))
  (map! :map ghostel-boon-insert-map
        "<escape>" nil
        "C-z" nil)

  (map! :map ghostel-mode-map
        "<prior>" (lambda () (interactive) (ghostel-send-key "p" "ctrl"))
        "<next>" (lambda () (interactive) (ghostel-send-key "n" "ctrl"))
        ctl-tap-wsl #'ghostel-emacs-mode
        ctl-tap #'ghostel-emacs-mode
        ctl-dbl-tap-wsl #'ghostel-copy-mode
        ctl-dbl-tap #'ghostel-copy-mode)
  (map! :map ghostel-readonly-mode-map
        ctl-tap-wsl #'ghostel-semi-char-mode
        ctl-tap #'ghostel-semi-char-mode
        ctl-dbl-tap-wsl #'boon-set-command-state
        ctl-dbl-tap #'boon-set-command-state
        "<escape>" #'ghostel-semi-char-mode
        "C-z" #'boon-set-command-state
        )

  ; <next> and <prev> are normally set by pixel-scroll-precision-mode and the associated map,
  ; which as a a minor mode has higher priority.
  ; We use this map instead, piggy-backing on a mechanism ghostel uses for mouse scrolling.
  (map! :map ghostel--scroll-intercept-map
        ; Goes up and down in history
        "<prior>" (lambda () (interactive)  (if (eq ghostel--input-mode 'semi-char)
                                           (ghostel-send-key "p" "ctrl")
                                         (ghostel--redispatch-scroll-event 'prior)))
        "<next>" (lambda () (interactive) (if (eq ghostel--input-mode 'semi-char)
                                         (ghostel-send-key "n" "ctrl")
                                       (ghostel--redispatch-scroll-event 'next)))
        )
  )

(after! vterm
  (setq vterm-min-window-width 40)

  (defvar vterm-boon-insert-map (make-boon-map 'vterm-mode 'insert))
  (map! :map vterm-boon-insert-map
        "<escape>" nil
        "C-z" nil)

  (map! :map vterm-mode-map
        ctl-tap-wsl #'vterm-copy-mode
        ctl-tap #'vterm-copy-mode
        ctl-dbl-tap-wsl (lambda () (interactive) (vterm-copy-mode) (boon-set-command-state))
        ctl-dbl-tap (lambda () (interactive) (vterm-copy-mode) (boon-set-command-state))
        )
  (map! :map vterm-copy-mode-map
        ctl-tap-wsl #'vterm-copy-mode
        ctl-tap #'vterm-copy-mode
        ctl-dbl-tap-wsl #'boon-set-command-state
        ctl-dbl-tap #'boon-set-command-state
        )
  )

(use-package! claude-code-ide
  ;:bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (map! :leader "M-'" #'claude-code-ide-menu
                "C-'" #'claude-code-ide-menu)
  (claude-code-ide-emacs-tools-setup) ; Optionally enable Emacs MCP tools

  (setq claude-code-ide-terminal-backend 'vterm)

  (setq claude-code-ide-window-width 40)

  ; Unfortunately, Windows steals C-<escape>
  (advice-add #'claude-code-ide--configure-vterm-buffer :after (lambda ()
                (local-set-key (kbd "<escape>") #'claude-code-ide-send-escape)))
  )


(defun +org-capture/open-frame-ms (&optional initial-input key)
  "Opens the org-capture window in the current frame, closing it once
you're done. This can be called from an external script on Windows."
  (interactive)
  (when (and initial-input (string-empty-p initial-input))
    (setq initial-input nil))
  (when (and key (string-empty-p key))
    (setq key nil))
  (let* ((frame-title-format "")
         (frame (selected-frame)))

    (when (not (+org-capture-frame-p))
      (modify-frame-parameters nil +org-capture-frame-parameters))
    (with-selected-frame frame
      (require 'org-capture)
      (doom/reload-font)
      (condition-case ex
          (letf! ((#'pop-to-buffer #'switch-to-buffer))
            (switch-to-buffer (doom-fallback-buffer))
            (let ((org-capture-initial initial-input)
                  org-capture-entry)
              (when (and key (not (string-empty-p key)))
                (setq org-capture-entry (org-capture-select-template key)))
              (funcall +org-capture-fn)))
        ('error
         (message "org-capture: %s" (error-message-string ex))
         (delete-frame frame))))))

;(advice-add #'org-capture-select-template :before (lambda (&rest _) (doom/reload-font)))
;(add-hook! org-capture-mode-hook (lambda (_) (doom/reload-font)))


(defun insert-into-list (list n el)
  "Insert into list LIST an element EL at index N.

If N is 0, EL is inserted before the first element.

The resulting list is returned.  As the list contents is mutated
in-place, the old list reference does not remain valid."
  (let* ((padded-list (cons nil list))
         (c (nthcdr n padded-list)))
    (setcdr c (cons el (cdr c)))
    (cdr padded-list)))

(insert-into-list +dashboard-menu-sections 3
  '("Find org-roam node"
    :icon (nerd-icons-icon-for-mode 'org-mode :face '+dashboard-menu-title)
    :when (modulep! :lang org +roam)
    :face (:inherit (+dashboard-menu-title bold))
    :action org-roam-node-find))

(after! org-protocol
  ;(advice-add #'org-capture-select-template
  ;            :around
  ;            (lambda (fn &rest args)
  ;              (condition-case err
  ;                  (apply fn args)
  ;                (user-error
  ;                 (when (string= (cadr err) "Abort")
  ;                   (server-edit)
  ;                   nil)))))

  (advice-add #'org-protocol-check-filename-for-protocol :before (lambda (&rest _) (doom/reload-font)))
  )

;(add-hook 'org-capture-mode-hook (lambda () (redisplay t)))

(after! org
  (setq org-hide-emphasis-markers t)

  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-latex-create-formula-image-program 'dvisvgm)

  ; Taken from doom org module;
  ; since it uses evil-org-mode package,
  ; my redirect-evil-to-boon doesn't automatically apply.
  (map! :mode org-mode
        ;; more intuitive RET keybinds
        :m "RET"      #'+org/dwim-at-point
        :m "<return>" #'+org/dwim-at-point
        :i "RET"      #'+org/return
        :i "<return>" #'+org/return
        :i [S-return] #'+org/shift-return
        :i "S-RET"    #'+org/shift-return)

  (defun my/org-latex-preview-all (&optional arg)
    (interactive "P")
    (org-latex-preview (my/shift-prefix arg 2)))

  ; Created by Claude
  (defun my/shift-prefix (arg n)
    "Shift raw prefix ARG up by N levels of C-u (each level = x4).
Handles the common raw-prefix forms: nil, a cons like (4), or
a plain integer (from C-u 3 etc.)."
    (let ((base (cond
                 ((null arg) 1)                 ; no prefix => treat as C-u^0
                 ((consp arg) (car arg))         ; (4), (16), ...
                 ((integerp arg) arg)            ; numeric arg, e.g. C-u 3
                 (t 1))))
      (list (* base (expt 4 n)))))

  (map! :localleader :mode org-mode
        :prefix ("z" . "previews/misc")
        :desc "Preview latex" "l" #'org-latex-preview
        :desc "Preview all latex" "L" #'my/org-latex-preview-all
        )

  ; Not sure if this is true for all machines,
  ; but in the environments I have tried this on, the scale is too big.
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 0.5))

  (when (modulep! :lang org +roam)

    (add-to-list 'org-capture-templates
                 '("r" "Org-Roam Node" entry
                   (function org-roam-capture)
                   ""
                   :immediate-finish t))

                                        ; Modified from https://www.d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5

    (defun vulpea-todo-p ()
      "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
      (seq-find                                 ; (3)
       (lambda (type)
         (eq type 'todo))
       (org-element-map                         ; (2)
           (org-element-parse-buffer 'headline) ; (1)
           'headline
         (lambda (h)
           (org-element-property :todo-type h)))))

    (defun my/org-roam-buffer-tags-get ()
      (let ((node (org-roam-node-at-point)))
        (if (org-roam-node-p node)
            (org-roam-node-tags node)
          nil)
        )
      )

    (defun vulpea-todo-update-tag ()
      "Update todo tag in the current buffer."
      (when (and (not (active-minibuffer-window))
                 (vulpea-buffer-p))
        (save-excursion
          (goto-char (point-min))
          (let* ((tags (my/org-roam-buffer-tags-get))
                 (original-tags tags))

            (if (vulpea-todo-p)
                (when (not (seq-contains-p tags "todo"))
                  (org-roam-tag-add '("todo")))
              (when (seq-contains-p tags "todo")
                (org-roam-tag-remove '("todo"))))
            ))))

    (defun vulpea-buffer-p ()
      "Return non-nil if the currently visited buffer is a note."
      (and buffer-file-name
           (string-prefix-p
            (expand-file-name (file-name-as-directory org-roam-directory))
            (file-name-directory buffer-file-name))))

    (defun vulpea-todo-files ()
      "Return a list of note files containing 'todo' tag." ;
      (seq-uniq
       (seq-map
        #'car
        (org-roam-db-query
         [:select [nodes:file]
          :from tags
          :left-join nodes
          :on (= tags:node-id nodes:id)
          :where (like tag (quote "%\"todo\"%"))]))))

    (defun vulpea-agenda-files-update (&rest _)
      "Update the value of `org-agenda-files'."
      (setq org-agenda-files (vulpea-todo-files)))

    (add-hook 'find-file-hook #'vulpea-todo-update-tag)
    (add-hook 'before-save-hook #'vulpea-todo-update-tag)

    (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
    (advice-add 'org-todo-list :before #'vulpea-agenda-files-update)
    )
  )

; If using flyspell, their bindings are overly aggressive
(setq flyspell-mode-map (make-sparse-keymap))

(after! jinx
  (setq jinx-languages "en_US-large")

  ;; Import any ispell LocalWords already in the file so they're honoured by jinx.
  (add-hook! 'jinx-mode-hook
    (defun +jinx-load-ispell-localwords-h ()
      (require 'ispell)
      (when-let* ((words (save-excursion
                           (goto-char (point-min))
                           (cl-loop while (search-forward ispell-words-keyword nil t)
                                    collect (string-trim
                                             (buffer-substring-no-properties
                                              (point) (line-end-position)))))))
        (let ((merged (mapconcat #'identity words " ")))
          (setq jinx-local-words (concat jinx-local-words merged))
          (setq jinx--session-words (append jinx--session-words (split-string merged)))))))

  (when (modulep! :completion vertico)
    (after! vertico-multiform
      (add-to-list 'vertico-multiform-categories
                   '(jinx grid
                     (vertico-grid-annotate . 20)
                     (vertico-count . 4)))))
  )

(use-package! org-fragtog
  :hook (org-mode-hook . org-fragtog-mode))

(setq dtrt-indent-min-relevant-lines 1)




; Must be at end of file
;
(when (file-exists-p "~/.doom.d/config.local.el")
  (load-file "~/.doom.d/config.local.el"))
