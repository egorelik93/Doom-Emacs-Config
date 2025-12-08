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
  :config

  (setq catppuccin-flavor 'macchiato)

  (defun set-catppuccin-flavor-and-reload (flavor)
    (interactive "MOne of latte, frappe, macchiato, or mocha: ")
    (setq catppuccin-flavor (intern flavor))
    (catppuccin-reload)
    (my-configire-catppuccin)
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
           (eq loading-theme (modus-themes-get-current-theme :no-enable)))
      (setq my-ef-themes-solaire-faces-do-not-recurse t)
      (modus-themes-with-colors
        (let ((theme (modus-themes-get-current-theme :no-enable)))
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
(defconst ctl-tap "C-<F15>")
(defconst ctl-tap-term-raw "\e[28;5~")

(defconst ctl-dbl-tap-wsl "C-<Launch9>")
(defconst ctl-dbl-tap "C-<F18>")
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

(defun evil-exit-emacs-state-unless-god ()
  (interactive)
  (if (bound-and-true-p god-local-mode) (god-local-mode 0) (evil-exit-emacs-state)))

; I like the idea, but other than hjkl I just can't get
; used to the commands.
(map! ; :e ctl-tap-wsl #'evil-exit-emacs-state-unless-god
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
      :desc "C-x" "x" ctl-x-map
      "M-x" #'doom/open-scratch-buffer)

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

(defun boon-exit-insert-state-unless-god ()
  (interactive)
  (if (bound-and-true-p god-local-mode) (god-local-mode 0) #'boon-quit))

; Boon is cool, but still needs a lot of configuration to work with anything else.
(use-package! boon
  :config
  (require 'boon-qwerty)

  (require 'boon-powerline)
  (boon-powerline-theme)

  ; The normal implementation applies to any special mode, which includes lots of modes
  ; without custom keybindings.
  (defun boon-special-mode-p ()
  "Should the mode use `boon-special-state'? Less aggressive than original."
  (or ;(and (eq (get major-mode 'mode-class) 'special)
      ;     (not (boon-shell-mode-p)))
      (-some 'eval boon-special-conditions)
      (memq major-mode boon-special-mode-list)))

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
    (let ((map (if (listp merge-with) (make-composed-keymap merge-with) (make-sparse-keymap)))
          (boon-map-property (cdr (assq state boon-map-property-alist)))
          (boon-map (cdr (assq state boon-map-alist))))
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
        (:when (display-graphic-p)
          "C-i" #'pixel-scroll-interpolate-up ; C-i is always equivalent to terminal TAB.
                                              ; Thus, picking a frequent command that already has
                                               ; a less reachable key, namely Page Up.
          "C-k" #'pixel-scroll-interpolate-down)
        (:unless (display-graphic-p)
          "C-k" nil) ; discourage use while in termimal)
        )

  (map! :e ctl-tap #'boon-set-command-state)
  (map! :map 'boon-command-map ctl-tap #'boon-set-insert-like-state)
  (map! :map 'boon-command-map ctl-dbl-tap #'boon-set-insert-like-state)

  (defvar +doom-dashboard-mode-boon-map (make-boon-map '+doom-dashboard-mode 'command +doom-dashboard-mode-map))
  (defvar +doom-dashboard-mode-boon-special-map (make-boon-map '+doom-dashboard-mode 'special +doom-dashboard-mode-map))

  (map! :map (+doom-dashboard-mode-boon-map +doom-dashboard-mode-boon-special-map)
        "i" #'+doom-dashboard/backward-button
        "k" #'+doom-dashboard/forward-button)

  (defvar org-boon-map (make-boon-map 'org-mode 'command))
  (map! :map org-boon-map
        "<RET>" #'org-return
        "<return>" #'org-return)

  (map! :leader :desc "boon" alt-tap boon-command-map)
  )

(use-package! theist-mode
  :config
  (map! :leader :desc "C-c" "M-c" #'theist-C-c)
  )

;(map! :map '+doom-dashboard-mode-map "i" #'+doom-dashboard/backward-button)
;(map! :map '+doom-dashboard-mode-map "k" #'+doom-dashboard/forward-button)

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
        "j" #'evil-next-line
        "k" #'evil-previous-line
        "h" #'evil-backward-char
        "l" #'evil-forward-char
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

(load! (let ((coding-system-for-read 'utf-8))
         (shell-command-to-string "agda-mode locate")))

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
  (map! :e "C-<tab>" #'company-complete
        :i "C-<tab>" #'company-complete
        :e "C-." #'company-complete
        :i "C-." #'company-complete))

(after! corfu
  (map! :ei "C-." #'completion-at-point)
  (map! :map 'corfu-map
        "C-<TAB>" #'corfu-reset
        :e "C-<RET>" #'corfu-quit
        :e "C-<return>" #'corfu-quit
        :e ctl-tap #'corfu-quit
        :e ctl-tap-wsl #'corfu-quit
        :ei "C-." #'corfu-insert-separator)
  (setq corfu-max-width 80)

  (setq corfu-preselect 'valid)
  (setq +corfu-want-ret-to-confirm 't)
  (setq corfu-quit-at-boundary t)
  (setq corfu-quit-no-match t))

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
(defun my/set-initial-frame ()
  (let* ((base-factor-width 0.30) (base-factor-height 0.35)
    (a-width (* (display-pixel-width) base-factor-width))
        (a-height (* (display-pixel-height) base-factor-height))
        (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
    (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))

(if (display-graphic-p)
    (progn
      (setq frame-resize-pixelwise t)
      (my/set-initial-frame)))

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
  (remove-hook 'doom-init-ui-hook #'+doom-dashboard-init-h)
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

  (map-which-key-paging-leader-prefixes `("" "h" "p" "w" "b" "x" "z" "[" "]" ,alt-tap))

  (map-which-key-paging-localleader-prefixes org-mode-map '("") org)
  (map-which-key-paging-leader-prefixes '("C-v") 'org-mode-map)
)

(after! org
  (advice-add #'org-mode-restart :around
              (lambda (orig-fn)
                (let ((current-evil-state evil-state))
                  (funcall orig-fn)
                  (unless (eq current-evil-state evil-state))
                      (evil-change-state current-evil-state))))
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
