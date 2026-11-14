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


(after! boon
  (map! :e ctl-tap #'boon-set-command-state)
  (map! :map 'boon-command-map ctl-tap #'boon-set-insert-like-state)
  (map! :map 'boon-command-map ctl-dbl-tap #'boon-set-insert-like-state)

  (map! :leader :desc "boon" alt-tap boon-command-map)
  (unless (modulep! :editor evil)
    (map! :e "M-o" boon-command-map)
    )
  )

(after! god-mode
  (map! :map god-local-mode-map
        ; Doesn't seem to override other custom shortcuts
        ; Make sure to configure taps in other maps to not change the evil state if in god mode.
        :g ctl-dbl-tap-wsl #'god-local-mode
        :g ctl-dbl-tap #'god-local-mode
        :g ctl-tap-wsl #'god-local-mode
        :g ctl-tap #'god-local-mode
        )
  )

(map! :e ctl-dbl-tap-wsl #'god-local-mode
      :e ctl-dbl-tap #'god-local-mode)
