;;; themes.el -*- lexical-binding: t; -*-

(require 'use-theme)

;; As I basically never like this color for a cursor,
;; just set this in the theme during :every-enable and it will be
;; taken care of.
;; For this to work, :every-enable must be implemented to run
;; before doom-load-theme-hook which runs +evil-update-cursor-color-h
(defvar my-emacs-cursor-color nil)
(defun my-evil-update-cursor-color-h ()
  (put 'cursor 'evil-emacs-color  (or my-emacs-cursor-color (face-foreground 'warning)))
  (put 'cursor 'evil-normal-color (face-background 'cursor)))
(advice-add '+evil-update-cursor-color-h :override #'my-evil-update-cursor-color-h)

(add-hook 'disable-theme-functions
          (defun my-reset-emacs-cursor-color ()
            (setq my-emacs-cursor-color nil)
            ))

(use-theme-package! catppuccin-theme
  :theme catppuccin
  :defer t
  :init
  (
   (setq catppuccin-flavor 'macchiato)
   )
  :setup
  (
   ;; This must run before the most recent catppuccin reload.
   ;; If we need to reload anyway, can put this in :setup,
   ;; but otherwise must be in :init.

   ;; Resolved on emacs 30.
   ;;
   ;; Inside Windows CLIs, Catpuccin's 256-color quantization
   ;; gets mapped by emacs to something completely unusable.
   ;; We thus map it to 16 colors ourselves.
   ;;
   ;; Important requirements:
   ;; - Default text color must be "white".
   ;; - Background should be "black" (treated by Windows Terminal as transparent).
   ;; - Highlights are used by vertico and must be visible without being unreadable.
   ;; - If Highlights of the current line are visible (which they have to be because of prev point),
   ;;   then it should ideally be the same color as region selection.
   (when (and (featurep :system 'windows)
              (version< emacs-version "30.1")
              (or (not (display-graphic-p)) (daemonp)))
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
     (byte-compile #'my-catppuccin-quantize-to-16)

     (advice-add 'catppuccin-quantize-color :filter-return #'my-catppuccin-quantize-to-16)
     )


  (defun set-catppuccin-flavor-and-reload (flavor)
    (interactive "MOne of latte, frappe, macchiato, or mocha: ")
    (setq catppuccin-flavor (intern flavor))
    (load-theme 'catppuccin t t)
    (my-configure-catppuccin)
    (catppuccin-reload))

  (defun my-catppuccin-evil-update-cursor-color-h ()
    (put 'cursor 'evil-emacs-color ))
  )
  :first-enable
  (
   (setq my-catppuccin-base-color "#272b38")

   ;;(catppuccin-set-color 'base "#282c34" 'macchiato) ; doom-one background
   ;;(catppuccin-set-color 'base "#262938" 'macchiato) ; a bit less intense than macchiato
   ;;(catppuccin-set-color 'base "#272b38" 'macchiato) ; a little less purple, more grey than the above
   ;;(catppuccin-set-color 'base "#282c3a" 'macchiato) ; should be less than the above, but I can't tell.

   (catppuccin-set-color 'base my-catppuccin-base-color 'macchiato)

   (when (eq catppuccin-flavor 'macchiato)
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

   (load-theme 'catppuccin t t)
  )
  :every-enable
  (
   (when (eq catppuccin-flavor 'macchiato)
     (setq my-emacs-cursor-color (catppuccin-color 'yellow 'macchiato)))
   )
  )

(use-theme-package! batppuccin
  :preface
  (
   (defvar my-batppuccin-variants '(
                                    batppuccin-mocha
                                    batppuccin-macchiato
                                    batppuccin-frappe
                                    batppuccin-latte))
   )
  :theme (:eval my-batppuccin-variants)
  :assert-loads-once t
  :defer (not (memq doom-theme my-batppuccin-variants))
  :setup
  (
   (setq my-batppuccin-base-color "#272b38")
   (setq my-batppuccin-warning-color "#cdbe70")
   (setq my-batppuccin-original-yellow (batppuccin-get-color "bat-yellow" 'batppuccin-macchiato))
   (setf (alist-get "bat-base" batppuccin-macchiato-colors-alist) my-batppuccin-base-color)
   (setf (alist-get "bat-yellow" batppuccin-macchiato-colors-alist) my-batppuccin-warning-color)
   )
  :first-enable
  (
   (when (eq theme 'batppuccin-macchiato)
     (let ((bat-original-yellow my-batppuccin-original-yellow))
       (batppuccin-with-colors
         (custom-theme-set-faces! 'batppuccin-macchiato
           `(show-paren-match :foreground "#ee82ee" :background ,bat-surface1 :weight bold)
           ;;`(font-lock-builtin-face :foreground ,bat-pink)
           `(font-lock-preprocessor-face :foreground "#f0c6c6")
           ;; Given that batppuccin doesn't use much :inherit,
           ;; we will take a difference approach from that for catppuccin;
           ;; we have changed the palette for yellow to my preferred warning colors,
           ;; and manually set the remainder to the original color.
           `(ansi-color-bright-yellow :foreground ,bat-original-yellow :background ,bat-original-yellow)
           `(font-lock-type-face :foreground ,bat-original-yellow)
           `(diredfl-number :foreground ,bat-original-yellow)
           `(ruler-mode-current-column :foreground ,bat-original-yellow :weight bold)
           `(shr-mark :foreground ,bat-base :background ,bat-original-yellow)
           `(bookmark-face :foreground ,bat-original-yellow)
           `(diary :foreground ,bat-original-yellow)
           `(eldoc-highlight-function-argument :foreground ,bat-original-yellow :weight bold)
           `(nrepl-message-3-face :foreground ,bat-original-yellow)
           `(consult-bookmark :foreground ,bat-original-yellow)
           `(haskell-type-face :foreground ,bat-original-yellow)
           `(haskell-constructor-face :foreground ,bat-original-yellow)
           `(lsp-ui-sideline-code-action :foreground ,bat-original-yellow)
           `(magit-tag :foreground ,bat-original-yellow)
           `(marginalia-type :foreground ,bat-original-yellow)
           ;; Like other highlight faces, foreground was set to bat-base but
           ;; this is hard to see in certain contexts.
           `(asciidoc-highlight-face :foreground ,bat-surface1 :background ,bat-original-yellow)
           `(rainbow-delimiters-depth-3-face :foreground ,bat-original-yellow)
           `(web-mode-html-attr-name-face :foreground ,bat-original-yellow)
           `(web-mode-html-attr-custom-face :foreground ,bat-original-yellow :slant italic)
           `(web-mode-html-attr-engine-face :foreground ,bat-original-yellow)
           `(web-mode-block-attr-name-face :foreground ,bat-original-yellow)
           `(web-mode-annotation-type-face :foreground ,bat-original-yellow)
           `(web-mode-type-face :foreground ,bat-original-yellow)
           `(nerd-icons-lyellow :foreground ,bat-original-yellow)
           `(nerd-icons-yellow :foreground ,bat-original-yellow)
           `(nerd-icons-dyellow :foreground ,bat-yellow)
           `(ansi-color-names-vector
             [,bat-surface1 ,bat-red ,bat-green ,bat-original-yellow
              ,bat-blue ,bat-pink ,bat-teal ,bat-subtext0])
           ;; I kind of like the burlywood color originally used for latex -
           ;; might make a good alternative to yellow here.
           ;; (currently in catppuccin but not batppuccin)
           ;;`(highlight-quoted-symbol :foreground ,bat-yellow)
           `(highlight-quoted-symbol :foreground "burlywood")
           `(highlight-quoted-quote  :foreground ,bat-sky)
           ;; batppuccin sets doom-modeline-info to teal, which overrides
           ;; doom-modeline's settings for boon and god,
           ;; which batppuccin doesn't know about
           ;; (but it does know about evil, albeit set quite differently).
           ;; I will keep the bold however.
           `(doom-modeline-info :foreground ,bat-green :weight bold)
           ;; In general, batppuccin switches a lot of red/yellow/green families
           ;; to red/yellow/blue.
           `(compilation-info :foreground ,bat-green)
           `(flymake-note :underline (:style wave :color ,bat-green))
           `(cider-repl-result-face :inherit success)
           `(elfeed-log-info-level-face :foreground ,bat-green)
           `(erlang-edoc-verbatim :foreground ,bat-green)
           `(flycheck-info :underline (:style wave :color ,bat-green))
           `(flycheck-error-list-info :foreground ,bat-green)
           `(flycheck-fringe-info :foreground ,bat-green)
           `(asciidoc-code-face :foreground ,bat-green :background ,bat-mantle :extend t)
           `(markdown-code-face :foreground ,bat-green :background ,bat-mantle)
           `(markdown-inline-code-face :foreground ,bat-green)
           `(markdown-pre-face :foreground ,bat-green)
           `(which-key-local-map-description-face :foreground ,bat-green)
           `(font-latex-verbatim-face :foreground ,bat-green :inherit fixed-pitch)
           `(undo-tree-visualizer-unmodified-face :foreground ,bat-green)

           ;; For my preferences I prefer to swap these
           `(org-code :foreground ,bat-green)
           `(org-verbatim :foreground ,bat-teal)
           ;; Undecided on this one: auctex is default is some
           ;; yellow called burlywood; batppuccin changes this to teal.
           ;; burlywood sticks out better - but perhaps too much.
           `(font-latex-math-face :foreground "burlywood")
           ;; Batppuccin sets these to bat-base, but with hl-line
           ;; that is impossible to see.
           `(hi-blue :foreground "black" :background ,bat-blue)
           `(hi-green :foreground "black" :background ,bat-green)
           `(hi-pink :foreground "black" :background ,bat-pink)
           `(hi-yellow :foreground "black" :background ,bat-yellow)
           )))
     )
   )
  :every-enable
  (
   (when (eq theme 'batppuccin-macchiato)
     (setq my-emacs-cursor-color my-batppuccin-original-yellow))
   )
  )

(use-theme-package! tokyo-night
  :preface
  (
   (defvar my-tokyo-night-variants '(
                                    tokyo-night
                                    tokyo-night-storm
                                    tokyo-night-moon
                                    tokyo-night-day))
   )
  :theme (:eval my-tokyo-night-variants)
  :defer (not (memq doom-theme my-tokyo-night-variants))
  :assert-loads-once t
  )

(use-theme-package! ef-themes
  :defer (not (my-modus-theme-p doom-theme))
  :theme #'my-modus-theme-p
  :assert-loads-once t
  :preface
  (
   (defun my-modus-theme-p (theme)
     (or
      (string-prefix-p "ef-" (symbol-name theme))
      (string-prefix-p "modus-" (symbol-name theme))))
  )
  :config
  (
   ;;(ef-themes-take-over-modus-themes-mode 1)
   (modus-themes-include-derivatives-mode 1)
   )
  :setup
  (
   ;; solaire-mode
   (defun my-ef-themes-solaire-faces (loading-theme)
     ;; For whatever reason, loading-theme is not directly available inside modus-themes-with-colors
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

     ;; solaire-mode checks this, so necessary to provide manually
     (put loading-theme 'theme-feature t)
    )
  )
  :every-enable
  (
   ;; This code, via the enable-theme-functions hook,
   ;; is run *by* enable-theme/load-theme - which means
   ;; technically it runs /before/ solaire-mode--prepare-for-theme-a
   ;; which is :after advice on load-theme.
   (my-ef-themes-solaire-faces theme)
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

(when (and (featurep :system 'windows) (or (not (display-graphic-p)) (daemonp)))
  (add-hook! 'tty-setup-hook
    (unless (memq default-terminal-coding-system '(utf-8 cp65001 utf-8-dos utf-8-unix))
        ; On CMD, this will cause the boon modeline icon to at least
        ; show a state name instead of a code point.
        (setq doom-modeline-icon nil))))
