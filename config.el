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


(load! "themes.el")


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
    (speedrect-mode))


(load! "modal.el")


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

(load! "ediff-layout.el")

(load! "lang.el")

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


(load! "org.el")

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

(setq dtrt-indent-min-relevant-lines 1)




; Must be at end of file
;
(when (file-exists-p "~/.doom.d/config.local.el")
  (load-file "~/.doom.d/config.local.el"))
