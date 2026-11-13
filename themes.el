;;; themes.el -*- lexical-binding: t; -*-

(require 'use-theme)

;; If I prefer some color besides warning for the normal state cursor,
;; call this in the theme during :every-enable.
(defun my-set-normal-cursor-color (color)
  (put 'cursor 'evil-normal-color color)
  )

;; In keeping with my usage, uses the theme for the emacs state cursor,
;; and the normal state can be set by calling my-set-normal-cursor-color.
(defun my-evil-update-cursor-color-h ()
  (put 'cursor 'evil-normal-color (face-foreground 'warning))
  (put 'cursor 'evil-emacs-color (face-background 'cursor)))
(advice-add '+evil-update-cursor-color-h :override #'my-evil-update-cursor-color-h)

(add-hook 'disable-theme-functions
          (defun my-reset-normal-cursor-color (_theme)
            (put 'cursor 'evil-normal-color (face-foreground 'warning))
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
     (my-set-normal-cursor-color (catppuccin-color 'yellow 'macchiato)))
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
   ;; Copied from catpuccin

   ;; Color operations
   (defun my-batppuccin-hex-to-rgb (color)
     "Convert a hex COLOR string like \"#rrggbb\" to a list of three integers."
     (mapcar (lambda (i) (string-to-number (substring color i (+ i 2)) 16))
             '(1 3 5)))

   (defun my-batppuccin-rgb-to-hex (r g b)
     "Convert R, G, B integers to a hex color string."
     (format "#%02x%02x%02x" r g b))

   (defun my-batppuccin-rnd (n)
     "Round N to the nearest integer."
     (round (+ 0.5 n)))

   (defun my-batppuccin-lighten (color value)
     "Lighten COLOR by VALUE% (0–100)."
     (let* ((factor (/ value 100.0)))
       (apply #'my-batppuccin-rgb-to-hex
              (mapcar (lambda (v)
                        (my-batppuccin-rnd
                         (min 255 (+ (* (- 255 v) factor) v))))
                      (my-batppuccin-hex-to-rgb color)))))

   (defun my-batppuccin-darken (color value)
     "Darken COLOR by VALUE% (0–100)."
     (let* ((factor (/ value 100.0)))
       (apply #'my-batppuccin-rgb-to-hex
              (mapcar (lambda (v)
                        (floor (* (- 1 factor) v)))
                      (my-batppuccin-hex-to-rgb color)))))

   (defun my-batppuccin-recolor (color value theme)
     "Darken or lightens COLOR based on the current flavour."
     (if (eq theme 'batppuccin-latte)
         (my-batppuccin-lighten color value)
       (my-batppuccin-darken color value)))


   (defun my-rgb-diff (rgb1 rgb2)
     (mapcar (lambda (i) (- (nth i rgb1) (nth i rgb2))) '(0 1 2))
     )

   (defun my-rgb-add (rgb diff)
     (mapcar (lambda (i) (+ (nth i rgb) (nth i diff))) '(0 1 2))
     )


   (setq my-batppuccin-base-color "#272b38")
   (setq my-batppuccin-warning-color "#cdbe70")
   (setq my-batppuccin-original-yellow (batppuccin-get-color "bat-yellow" 'batppuccin-macchiato))
   (setf (alist-get "bat-base" batppuccin-macchiato-colors-alist) my-batppuccin-base-color)
   (setf (alist-get "bat-yellow" batppuccin-macchiato-colors-alist) my-batppuccin-warning-color)

   ;; Obtained by taking the hex value difference between bat-base and bat-cursor-line
   ;; and adding to my base.
   ;; We can also get to this same number by noting that
   ;; the current (my-batppuccin-darken bat-cursor-line 12) = bat-base exactly,
   ;; and finding a number that satisfies the same equation with our bat-base.
   ;; That turns out to be this number.
   ;; I do still like the slight blue tint of the original color, however.
   (setq my-batppuccin-cursor-line-color "#2d3140")
   ;; Another idea:
   ;; Batppuccin's cursor-line-color is close to (my-batppuccin-lighten ,bat-base 2.3)
   ;; Lighten my base by the same and apply the rgb difference.
   ;; The two end up extremely close, but the first looks a tad more natural.
   ;;(setq my-batppuccin-cursor-line-color "#2c3140")
   ;;(setq my-batppuccin-cursor-line-color
   ;;      (apply #'my-batppuccin-rgb-to-hex
   ;;             (my-rgb-add
   ;;              (my-batppuccin-hex-to-rgb (my-batppuccin-lighten my-batppuccin-base-color 2.3))
   ;;              (my-rgb-diff
   ;;               (my-batppuccin-hex-to-rgb "#2a2d42")
   ;;               (my-batppuccin-hex-to-rgb (my-batppuccin-lighten "#24273a" 2.3))))))

   (setf (alist-get "bat-cursor-line" batppuccin-macchiato-colors-alist) my-batppuccin-cursor-line-color)

   ;; Unfortunately, no such nice equation for bat-selection.
   ;; We try the catppuccin formula and add the difference in bases.
   ;; A few different methods ended up with a number close to this.
   (setq my-batppuccin-selection-color
         (apply #'my-batppuccin-rgb-to-hex
                (my-rgb-add
                 (my-batppuccin-hex-to-rgb (my-batppuccin-lighten my-batppuccin-base-color 17))
                 (my-rgb-diff
                  (my-batppuccin-hex-to-rgb "#494d64")
                  (my-batppuccin-hex-to-rgb (my-batppuccin-lighten "#24273a" 17))))))

   (setf (alist-get "bat-selection" batppuccin-macchiato-colors-alist) my-batppuccin-selection-color)

   )
  :every-enable
  (
   ;; I hate doing this, but batppuccin sets the current flavor too late.
   (let ((batppuccin--current theme))
     (batppuccin-with-colors
       (let (
             (bat-yellow (if (eq theme 'batppuccin-macchiato) my-batppuccin-original-yellow bat-yellow))
             (bat-warning (if (eq theme 'batppuccin-macchiato) my-batppuccin-warning-color bat-yellow))
             (class '((class color) (min-colors 88)))
             )

         (when (eq theme 'batppuccin-macchiato)
           (custom-theme-set-faces
            'batppuccin-macchiato
            `(show-paren-match ((,class :foreground "#ee82ee" :background ,bat-surface1 :weight bold)))
            ;;`(font-lock-builtin-face ((,class :foreground ,bat-pink)))
            `(font-lock-preprocessor-face ((,class :foreground "#f0c6c6")))
            ;; Undecided on this one: auctex is default is some
            ;; yellow called burlywood; batppuccin changes this to teal.
            ;; burlywood sticks out better - but perhaps too much.
            `(font-latex-math-face ((,class :foreground "burlywood")))
            ;; Batppuccin sets these to bat-base, but with hl-line
            ;; that is impossible to see.
            `(hi-blue ((,class :foreground ,(my-batppuccin-darken bat-mantle 50) :background ,bat-blue)))
            `(hi-green ((,class :foreground ,(my-batppuccin-darken bat-mantle 50) :background ,bat-green)))
            `(hi-pink ((,class :foreground ,(my-batppuccin-darken bat-mantle 50) :background ,bat-pink)))
            `(hi-yellow ((,class :foreground ,(my-batppuccin-darken bat-mantle 50) :background ,bat-yellow)))
            )

           (my-set-normal-cursor-color bat-yellow)

           ;; This must be called after indent-bars loads or it will get reset.
           (after! indent-bars
             ;; The comment face is too visible for this.
             (setq indent-bars-color `(,bat-overlay0 :face-bg nil :blend 0.425)))
           )

         ;; Use this list for any fundamental swaps to batppuccin.
         ;; Since batppuccin itself uses custom-theme-set-faces,
         ;; this makes it easier to copy/paste.
         (custom-theme-set-faces
          theme
          ;; Given that batppuccin doesn't use much :inherit,
          ;; we will take a difference approach from that for catppuccin;
          ;; we have changed the palette for yellow to my preferred warning colors,
          ;; and manually set the remainder to the original color.
          `(ansi-color-bright-yellow ((,class (:foreground ,bat-yellow :background ,bat-yellow))))
          `(font-lock-type-face ((,class :foreground ,bat-yellow)))
          `(diredfl-number ((,class :foreground ,bat-yellow)))
          `(ruler-mode-current-column ((,class :foreground ,bat-yellow :weight bold)))
          `(shr-mark ((,class :foreground ,bat-base :background ,bat-yellow)))
          `(bookmark-face ((,class :foreground ,bat-yellow)))
          `(diary ((,class :foreground ,bat-yellow)))
          `(eldoc-highlight-function-argument ((,class :foreground ,bat-yellow :weight bold)))
          `(nrepl-message-3-face ((,class :foreground ,bat-yellow)))
          `(consult-bookmark ((,class :foreground ,bat-yellow)))
          `(haskell-type-face ((,class :foreground ,bat-yellow)))
          `(haskell-constructor-face ((,class :foreground ,bat-yellow)))
          `(lsp-ui-sideline-code-action ((,class :foreground ,bat-yellow)))
          `(magit-tag ((,class :foreground ,bat-yellow)))
          `(marginalia-type ((,class :foreground ,bat-yellow)))
          ;; Like other highlight faces, foreground was set to bat-base but
          ;; this is hard to see in certain contexts.
          `(asciidoc-highlight-face ((,class :foreground ,bat-surface1 :background ,bat-yellow)))
          `(rainbow-delimiters-depth-3-face ((,class :foreground ,bat-yellow)))
          `(web-mode-html-attr-name-face ((,class :foreground ,bat-yellow)))
          `(web-mode-html-attr-custom-face ((,class :foreground ,bat-yellow :slant italic)))
          `(web-mode-html-attr-engine-face ((,class :foreground ,bat-yellow)))
          `(web-mode-block-attr-name-face ((,class :foreground ,bat-yellow)))
          `(web-mode-annotation-type-face ((,class :foreground ,bat-yellow)))
          `(web-mode-type-face ((,class :foreground ,bat-yellow)))
          `(nerd-icons-lyellow ((,class :foreground ,bat-yellow)))
          `(nerd-icons-yellow ((,class :foreground ,bat-yellow)))
          `(nerd-icons-dyellow ((,class :foreground ,bat-warning)))
          ;; batppuccin sets doom-modeline-info to teal, which overrides
          ;; doom-modeline's settings for boon and god,
          ;; which batppuccin doesn't know about
          ;; (but it does know about evil, albeit set quite differently).
          ;; I will keep the bold however.
          `(doom-modeline-info ((,class :foreground ,bat-green :weight bold)))
          ;; In general, batppuccin switches a lot of red/yellow/green families
          ;; to red/yellow/blue.
          `(compilation-info ((,class :foreground ,bat-green)))
          `(flymake-note ((,class :underline (:style wave :color ,bat-green))))
          `(cider-repl-result-face ((,class :foreground ,bat-green)))
          `(elfeed-log-info-level-face ((,class :foreground ,bat-green)))
          `(erlang-edoc-verbatim ((,class :foreground ,bat-green)))
          `(flycheck-info ((,class :underline (:style wave :color ,bat-green))))
          `(flycheck-error-list-info ((,class :foreground ,bat-green)))
          `(flycheck-fringe-info ((,class :foreground ,bat-green)))
          `(asciidoc-code-face ((,class :foreground ,bat-green :background ,bat-mantle :extend t)))
          `(markdown-code-face ((,class :foreground ,bat-green :background ,bat-mantle)))
          `(markdown-inline-code-face ((,class :foreground ,bat-green)))
          `(markdown-pre-face ((,class :foreground ,bat-green)))
          `(which-key-local-map-description-face ((,class :foreground ,bat-green)))
          `(font-latex-verbatim-face ((,class :foreground ,bat-green :inherit fixed-pitch)))
          `(undo-tree-visualizer-unmodified-face ((,class :foreground ,bat-green)))

          ;; For my preferences I prefer to swap these
          `(org-code ((,class :foreground ,bat-green)))
          `(org-verbatim ((,class :foreground ,bat-teal)))

          ;; catppuccin dynamically sets these. Lets copy what it does
          `(diff-hl-change ((,class :foreground ,(my-batppuccin-recolor bat-blue 50 theme) :background ,bat-diff-chg-bg)))
          `(diff-hl-delete ((,class :foreground ,(my-batppuccin-recolor bat-red 50 theme) :background ,bat-diff-del-bg)))
          `(diff-hl-insert ((,class :foreground ,(my-batppuccin-recolor bat-green 50 theme) :background ,bat-diff-add-bg)))
          `(diff-refine-added ((,class (:foreground ,bat-green :background ,(my-batppuccin-recolor bat-diff-add-bg -20 theme) :weight bold :extend t))))
          `(diff-refine-removed ((,class (:foreground ,bat-red :background ,(my-batppuccin-recolor bat-diff-del-bg -20 theme) :weight bold :extend t))))

          ;; Too visible for my preferences
          `(font-lock-comment-face ((,class (:foreground ,bat-overlay1 :slant italic))))
          ;; Make it better contrast with comments
          `(font-lock-doc-face ((,class (:foreground ,bat-overlay2 :slant normal))))

          ;; Some solaire-mode adjustments based on what catppuccin does.
          `(solaire-fringe-face ((,class (:background ,bat-mantle :foreground ,bat-surface1))))
          ;; This is what controls whether the doom dashboard modeline has the same background color;
          ;; undecided how I feel about it.
          ;;`(solaire-mode-line-face ((,class :background ,bat-crust nil :foreground ,bat-text)))
          `(solaire-mode-line-inactive-face ((,class :inverse-video nil :background ,bat-crust :foreground ,bat-subtext1)))
          `(solaire-header-line-face ((,class :inherit solaire-mode-line-face)))
          )

         (custom-theme-set-variables
          theme
          `(ansi-color-names-vector
            [,bat-surface1 ,bat-red ,bat-green ,bat-yellow
                           ,bat-blue ,bat-pink ,bat-teal ,bat-subtext0]))
         )))
   )
  :every-disable
  (
   ;; Set back to default
   (setq indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.425))
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
