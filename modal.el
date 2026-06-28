;;; modal.el -*- lexical-binding: t; -*-

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
