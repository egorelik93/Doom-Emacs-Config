;;; no-evil-bindings.el -*- lexical-binding: t; -*-

; Modified from config/default/+evil-bindings.el.

(unless (modulep! evil)
  (load! "no-evil-windows.elc"))

;;
;;; Global keybindings

;; Smart tab, these will only work in GUI Emacs
(map! :i [tab]
      `(menu-item "Insert smart tab" nil :filter
        (lambda (cmd)
          (cond
           ((or (doom-lookup-key [tab] overriding-terminal-local-map)
                (doom-lookup-key (kbd "TAB") overriding-terminal-local-map))
            cmd)
           ,@(when (modulep! :editor snippets)
               '(((memq (bound-and-true-p yas--active-field-overlay)
                        (overlays-in (1- (point)) (1+ (point))))
                  #'yas-next-field-or-maybe-expand)
                 ((and (bound-and-true-p yas-minor-mode)
                       (yas-maybe-expand-abbrev-key-filter 'yas-expand))
                  #'yas-expand)))
           ,@(when (modulep! :completion company +tng)
               '(((bound-and-true-p company-mode)
                  #'company-indent-or-complete-common)))
           ,@(when (modulep! :completion corfu)
               '(((and (bound-and-true-p corfu-mode)
                       corfu--candidates)
                  (if (derived-mode-p 'eshell-mode 'comint-mode)
                      #'completion-at-point
                    #'indent-for-tab-command)))))))
      :m [tab]
      `(menu-item "Command smart tab" nil :filter
        (lambda (cmd)
          (cond
           ((or (doom-lookup-key [tab] overriding-terminal-local-map)
                (doom-lookup-key (kbd "TAB") overriding-terminal-local-map))
            cmd)
           ,@(when (modulep! :editor snippets)
               '(((and (evil-visual-state-p)
                       (or (eq evil-visual-selection 'line)
                           (not (memq (char-after)
                                      (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                  #'yas-insert-snippet)))
           ,@(when (modulep! :editor fold)
               '(((save-excursion (end-of-line) (invisible-p (point)))
                  #'+fold/toggle)))
           ;; Fixes #4548: without this, this tab keybind overrides
           ;; mode-local ones for modes that don't have an evil
           ;; keybinding scheme or users who don't have :editor (evil
           ;; +everywhere) enabled.
           ((or (doom-lookup-key
                 [tab]
                 (list (get major-mode 'boon-map)
                       (current-local-map))
                (doom-lookup-key
                 (kbd "TAB")
                 (list (get major-mode 'boon-map)))
                (doom-lookup-key (kbd "TAB") (list (current-local-map))))
            cmd)
           ((fboundp 'boon-smarter-forward)
            #'boon-smarter-forward)))))
      ;; Extend smart tab for specific modes. This way, we process the entire
      ;; smart tab logic and only fall back to these commands at the end.
      (:when (modulep! :lang org)
       (:after org :map org-mode-map
        [remap indent-for-tab-command]
        `(menu-item "Go to the next field" org-table-next-field
          :filter ,(lambda (cmd) (when (org-at-table-p) cmd)))))

      (:when (modulep! :editor multiple-cursors)
        ; Don't forget to put :nv on this! Otherwise it breaks the insert g key.
        :nv "gz" nil
        :prefix "gz"

        ;:nv "d" nil
        ;:nv "D" nil
        ;:nv "s" nil
        ;:nv "S" nil
        ;:nv "c" nil
        ;:nv "C" nil
        ;:nv "j" nil
        ;:nv "k" nil
        ;:nv "m" nil
        ;:nv "n" nil
        ;:nv "N" nil
        ;:nv "p" nil
        ;:nv "P" nil
        ;:nv "q" nil
        ;:nv "t" nil
        ;:nv "u" nil
        ;:nv "z" nil
        ;:v  "I" nil
        ;:v  "A" nil

        :desc "Mark next" :nv "l" #'mc/mark-next-like-this
        :desc "Mark previous" :nv "j" #'mc/mark-previous-like-this
        :nv "L" #'mc/skip-to-next-like-this
        :nv "J" #'mc/skip-to-previous-like-this
        :desc "Unmark previous" :nv "C-j" #'mc/unmark-previous-like-this
        :desc "Unmark next" :nv "C-l" #'mc/unmark-next-like-this
        :nv "l" #'mc/mark-next-lines
        :nv "i" #'mc/mark-previous-lines
        :desc "Mark all" :nv "m" #'mc/mark-all-like-this
        :desc "Mark all DWIM" :nv "M" #'mc/mark-all-like-this-dwim
        :nv "q" #'mc/keyboard-quit
        :nv "t" #'mc/toggle-cursor-on-click
        :desc "Edit line starts" :nv "h" #'mc/edit-beginnings-of-lines
        :desc "Edit line endings" :nv ";" #'mc/edit-ends-of-lines
        :desc "Edit lines" :nv "v" #'mc/edit-lines
        :desc "Mark tag" :nv "s" #'mc/mark-sgml-tag-pair
        :nv "a" #'mc/mark-all-like-this-in-defun
        :desc "Add cursor w/mouse" :nv "<mouse-1>" #'mc/add-cursor-on-click
        )
      )


;;
;;; Module keybinds

;;; :editor
(map! (:when (modulep! :editor multiple-cursors)
        ;; evil-multiedit
        :v  "R"     nil
        :n  "M-d"   nil
        :n  "M-D"   nil
        :v  "M-d"   nil
        :v  "M-D"   nil
        :nv "C-M-d" nil

                                        ;Replacing :v  "R"     #'evil-multiedit-match-all
        :n "gzR"   #'iedit-mode
        :n  "C->"   #'mc/mark-next-symbol-like-this
        :n  "C-<"   #'mc/mark-previous-symbol-like-this
                                        ;:v  "M-d"   #'evil-multiedit-match-and-next
                                        ;:v  "M-D"   #'evil-multiedit-match-and-prev
                                        ;:nv "C-M-d" #'evil-multiedit-restore
        (:map iedit-mode-map
                                        ;:nv "M-d" #'evil-multiedit-match-and-next
                                        ;:nv "M-D" #'evil-multiedit-match-and-prev
              [return]  #'iedit-toggle-selection))

      )

;;
;;; <leader>

(map! :leader
      "w" nil
      (:prefix-map ("w" . "window")
                   "j" #'windmove-left
                   "l" #'windmove-right
                   "k" #'windmove-down
                   "i" #'windmove-up

                   "[left]" #'windmove-left
                   "[right]" #'windmove-right
                   "[down]" #'windmove-down
                   "[up]" #'windmove-up

                   "w" #'other-window
                   "W" (defun my-other-window-prev (count) (interactive "p\ni\np") (other-window (- count)))
                   "f" #'ffap-other-window

                   "p" (defun my-window-mru () (interactive) (select-window (get-mru-window t)))

                   "u" #'evil-window-top-left
                   "." #'evil-window-bottom-right

                   "n" 'evil-window-new

        :desc "Split horizontal" "s" #'split-window-below
        :desc "Split vertical" "v" #'split-window-right

                   "d" #'delete-window
                   "q" #'quit-window
                   "o" #'delete-other-windows

                   "+" #'enlarge-window
                   "-" #'shrink-window
                   ">" #'enlarge-window-horizontally
                   "<" #'shrink-window-horizontally
                   "_" #'evil-window-set-height
                   "|" #'evil-window-set-width
                   "=" #'balance-windows

                   "C-j" #'windmove-swap-states-left
                   "C-l" #'windmove-swap-states-right
                   "C-i" #'windmove-swap-states-up
                   "C-k" #'windmove-swap-states-down

                   "J" #'evil-window-move-far-left
                   "L" #'evil-window-move-far-right
                   "I" #'evil-window-move-very-top
                   "K" #'evil-window-move-very-bottom

                   (:when (featurep 'tab-bar)
                     "T" #'tab-window-detach
                     "gt" #'tab-bar-switch-to-next-tab
                     "gT" #'tab-bar-switch-to-prev-tab
                     )

                   "x" #'window-swap-states

                   "r" #'evil-window-rotate-downwards
                   "R" #'evil-window-rotate-upwards

                   "0" #'digit-argument
                   "1" #'digit-argument
                   "2" #'digit-argument
                   "3" #'digit-argument
                   "4" #'digit-argument
                   "5" #'digit-argument
                   "6" #'digit-argument
                   "7" #'digit-argument
                   "8" #'digit-argument
                   "9" #'digit-argument)

      :desc "Switch to last buffer" "`"    #'evil-switch-to-windows-last-buffer

      ;;; <leader> b --- buffer
      (:prefix "b"
       :desc "Switch to last buffer"       "l"   #'evil-switch-to-windows-last-buffer
       :desc "New empty buffer"            "N"   #'evil-buffer-new
       :desc "Save all buffers"            "S"   (defun my-save-all ()
                                                   (interactive)
                                                   (save-some-buffers t
                                                                      #'(lambda ()
                                                                          (and (not buffer-read-only)
                                                                               (buffer-file-name)))))
       )

      ;;; <leader> i --- insert
      (:prefix "i"
       :desc "Evil ex path"                  "p"   nil
       :desc "From evil register"            "r"   nil)

      ;;; <leader> q --- quit/session
      (:prefix "q"
       :desc "Quit Emacs without saving"    "Q" (defun my-quit-all-with-error-code ()
                                                  (interactive)
                                                  (if (bound-and-true-p server-buffer-clients)
                                                      (user-error "Cannot exit client process with error code")
                                                    (kill-emacs (or err-code 1))))
       )

      ;;; <leader> s --- search
      (:prefix "s"
       :desc "Jump list"                    "j" #'consult-global-mark
       :desc "Jump to mark"                 "r" #'consult-mark
       )

      ;;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
       :desc "Goggles"                       "g" #'goggles-mode
       )
      )

(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:C-w\\|%s w\\) m\\'" prefix-re))
                  nil . "maximize")
                which-key-replacement-alist)))
