;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

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

(defun translate-to-leader (prompt)
  (if (evil-normal-state-p)
      (kbd doom-leader-key)
      (kbd doom-leader-alt-key)))

(defun translate-to-localleader (prompt)
  (if (evil-normal-state-p)
      (kbd doom-localleader-key)
      (kbd doom-localleader-alt-key)))

;(map! :map 'override "<Launch6>" #'my-localleader-alias)

(map! :map 'key-translation-map "<Tools>" #'translate-to-leader)
;;; In WSL, F13 is getting mapped to <Tools>
(map! :map 'key-translation-map "<f13>" #'translate-to-leader)

(map! :map 'key-translation-map "<Launch5>" (kbd "M-o"))
(map! :map 'key-translation-map "<f14>" (kbd "M-o"))

(map! :map 'key-translation-map "<Launch6>" #'translate-to-localleader)
(map! :map 'key-translation-map "<f15>" #'translate-to-localleader)

; Created by Claude
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
        (t
         (execute-kbd-macro keys)
         ;; If we're still in normal state (didn't enter visual mode),
         ;; return to original state
         (when (eq evil-state 'normal)
           (funcall (intern (format "evil-%s-state" original-state)))
           ; Some versions had this, others didn't
           (setq evil-next-state nil)
           ))))))


(map! :map evil-emacs-state-map "M-o" #'my-evil-execute-in-normal-state)
;(map! :map evil-emacs-state-map "<Launch5>" #'my-evil-execute-in-normal-state)
;(map! :map evil-emacs-state-map "<f14>" #'my-evil-execute-in-normal-state)

(map! "C-RET" #'cua-rectangle-mark-mode)
(map! "C-<return>" #'cua-rectangle-mark-mode)

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

(use-package! speedrect
  :config
    (speedrect-mode)
  )


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
  (setq evil-disable-insert-state-bindings 't)
  (setq evil-default-state 'emacs))

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
  (map! :e "C-<tab>" #'completion-at-point
        :i "C-<tab>" #'completion-at-point
        :e "C-." #'completion-at-point
        :i "C-." #'completion-all-completions)
  (setq corfu-max-width 80)

  (setq corfu-preselect 'directory)
  (setq +corfu-want-ret-to-confirm 'minibuffer))

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
  )

(setq org-return-follows-link t)
