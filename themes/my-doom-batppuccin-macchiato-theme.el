;;; my-doom-catppuccin-macchiato-theme.el --- Catppuccin Macchiato palette for doom-themes -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;;; Commentary:
;;
;; EXPERIMENT (layered approach, simplified).  Self-contained: needs only
;; doom-themes, and no other file from ~/.doom.d.
;;
;; The Catppuccin Macchiato palette, with the author's palette modifications
;; (a less purple base, a separate warning yellow), handed to doom-themes so that
;; everything doom-themes knows how to style gets Catppuccin colours.  It defines
;; no faces of its own.  It is usable alone, but is meant to be the PRIMARY theme
;; with batppuccin-macchiato enabled on top of it: batppuccin wins wherever it
;; defines a face, and the batppuccin stanza's overrides in themes.el win over
;; both.
;;
;; To enable batppuccin on top, something has to load it after this theme is
;; enabled, e.g. from themes.el:
;;
;;   (add-hook 'enable-theme-functions
;;             (lambda (theme)
;;               (when (eq theme 'my-doom-catppuccin-macchiato)
;;                 (load-theme 'batppuccin-macchiato t))))
;;
;; (`enable-theme-functions' runs after this theme is enabled, so the theme loaded
;; from it ends up above it.)
;;
;; Every colour is a literal (GUI 256 16) triple:
;;   GUI  the Catppuccin hex colour (base and warning as modified).
;;   256  catppuccin-theme's `catppuccin-quantize-color' of the GUI colour.
;;   16   the result of the author's `my-catppuccin-quantize-to-16' (themes.el)
;;        on the 256 colour: Windows Terminal (Campbell) colours.
;;
;;; Code:

(require 'doom-themes)

;; Compiler pacifier
(defvar modeline-bg)

(def-doom-theme my-doom-batppuccin-macchiato
  "Catppuccin Macchiato palette for doom-themes, to sit under batppuccin."
  :family 'my-doom-batppuccin-macchiato
  :background-mode 'dark

  (
   ;; name        gui       256       16
   (rosewater  '("#f4dbd6" "#eecccc" "#cccccc"))
   (flamingo   '("#f0c6c6" "#eebbbb" "#cccccc"))
   (pink       '("#f5bde6" "#eebbdd" "#b4009e"))
   (mauve      '("#c6a0f6" "#bb99ee" "#b4009e"))
   (red        '("#ed8796" "#dd7788" "#e74856"))
   (maroon     '("#ee99a0" "#ee9999" "#e74856"))
   (peach      '("#f5a97f" "#ee9977" "#c50f1f"))
   (yellow     '("#eed49f" "#eecc99" "#c50f1f"))
   (green      '("#a6da95" "#99cc88" "#16c60c"))
   (teal       '("#8bd5ca" "#88ccbb" "#16c60c"))
   (sky        '("#91d7e3" "#88ccdd" "#3a96dd"))
   (sapphire   '("#7dc4e4" "#77bbdd" "#3a96dd"))
   (blue       '("#8aadf4" "#88aaee" "#cccccc"))
   (lavender   '("#b7bdf8" "#aabbee" "#881798"))
   (text       '("#cad3f5" "#bbccee" "#cccccc"))
   (subtext1   '("#b8c0e0" "#aabbdd" "#cccccc"))
   (subtext0   '("#a5adcb" "#99aabb" "#cccccc"))
   (overlay2   '("#939ab7" "#8899aa" "#cccccc"))
   (overlay1   '("#8087a2" "#777799" "#767676"))
   (overlay0   '("#6e738d" "#666688" "#767676"))
   (surface2   '("#5b6078" "#555577" "#767676"))
   (surface1   '("#494d64" "#444455" "#0037da"))
   (surface0   '("#363a4f" "#333344" "#0037da"))
   (mantle     '("#1e2030" "#111122" "#0c0c0c"))
   (crust      '("#181926" "#111122" "#0c0c0c"))
   (bg         '("#272b38" "#222233" "#0c0c0c")) ; macchiato's #24273a, made a little less purple

   ;; Derived from bg the way batppuccin's own derived colours are
   (cursor-line '("#2d3140" "#222233" "#0c0c0c"))
   (selection   '("#4b5162" "#444455" "#0037da"))

   ;; The warning colour; `yellow' stays stock for everything that isn't a warning
   (warning     '("#cdbe70" "#ccbb66" "#f9f1a5"))

   ;; doom-themes' palette, mapped from the above
   (bg-alt     mantle)
   (fg         text)
   (fg-alt     subtext0)
   (base0      crust)
   (base1      mantle)
   (base2      '("#2e3243" "#222233" "#0c0c0c")) ; bg halfway to surface0
   (base3      surface0)
   (base4      surface1)
   (base5      surface2)
   (base6      overlay1)
   (base7      subtext0)
   (base8      subtext1)

   (grey       overlay1)
   (orange     peach)
   (dark-blue  sapphire)
   (magenta    pink)
   (violet     mauve)
   (cyan       sky)
   (dark-cyan  sapphire)

   ;; face categories (what doom-themes-base uses for packages batppuccin skips)
   (highlight      blue)
   (vertical-bar   surface1)
   (builtin        red)
   (comments       overlay1)               ; the author's comment colour
   (doc-comments   overlay2)
   (constants      peach)
   (functions      blue)
   (keywords       mauve)
   (methods        blue)
   (operators      sky)
   (type           yellow)
   (strings        green)
   (variables      flamingo)
   (numbers        peach)
   (region         selection)
   (error          red)
   (success        green)
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   (modeline-bg           mantle)
   (modeline-fg           text)
   (modeline-inactive-bg  crust)
   (modeline-inactive-fg  overlay1))

  ;;;; Faces: none of our own; doom-themes-base-faces supplies them.
  ()

  ;;;; Variables: none.
  ())

;;; my-doom-batppuccin-macchiato-theme.el ends here
