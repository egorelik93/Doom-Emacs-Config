;;; emacs-bindings.el -*- lexical-binding: t; -*-

; Copied from config/default/+emacs-bindings.el.
; Since I keep evil on as an option, I am not getting those bindings automatically.

(setopt persp-keymap-prefix (kbd "C-c w"))
(after! projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


;;
;;; Autoloads

(autoload 'org-capture-goto-target "org-capture" nil t)


;;
;;; Leader keys

(map! :leader
      :desc "Evaluate line/region"        "e"   #'+eval/line-or-region

      (:prefix ("l" . "<localleader>")) ; bound locally
      (:prefix ("!" . "checkers"))      ; bound by flycheck

      ;;; <leader> p --- project
      ;(:prefix ("p" . "project")
      ; :desc "Search project for symbol"   "." #'+default/search-project-for-symbol-at-point
      ; :desc "Find file in other project"  "F" #'doom/find-file-in-other-project
      ; :desc "Search project"              "s" #'+default/search-project
      ; :desc "Toggle project scratch buffer" "x" #'doom/toggle-project-scratch-buffer
      ; :desc "Switch to project scratch buffer" "X" #'doom/switch-to-project-scratch-buffer
      ; ;; later expanded by projectile
      ; (:prefix ("4" . "in other window"))
      ; (:prefix ("5" . "in other frame")))

      ;;; <leader> & --- snippets
      (:prefix-map ("&" . "snippets")
       :desc "New snippet"           "n" #'yas-new-snippet
       :desc "Insert snippet"        "i" #'yas-insert-snippet
       :desc "Find global snippet"   "/" #'yas-visit-snippet-file
       :desc "Reload snippets"       "r" #'yas-reload-all
       :desc "Create Temp Template"  "c" #'aya-create
       :desc "Use Temp Template"     "e" #'aya-expand)

      ;;; <leader> v --- versioning
      (:prefix-map ("v" . "versioning")
       :desc "Git revert file"             "R"   #'vc-revert
       :desc "Kill link to remote"         "y"   #'+vc/browse-at-remote-kill
       :desc "Kill link to homepage"       "Y"   #'+vc/browse-at-remote-kill-homepage
       (:when (modulep! :ui vc-gutter)
        :desc "Git revert hunk"            "r"   #'+vc-gutter/revert-hunk
        :desc "Git stage hunk"             "s"   #'+vc-gutter/stage-hunk
        :desc "Git time machine"           "t"   #'git-timemachine-toggle
        :desc "Jump to next hunk"          "n"   #'+vc-gutter/next-hunk
        :desc "Jump to previous hunk"      "p"   #'+vc-gutter/previous-hunk)
       (:when (modulep! :tools magit)
        :desc "Magit dispatch"             "/"   #'magit-dispatch
        :desc "Magit file dispatch"        "."   #'magit-file-dispatch
        :desc "Forge dispatch"             "'"   #'forge-dispatch
        :desc "Magit status"               "g"   #'magit-status
        :desc "Magit status here"          "G"   #'magit-status-here
        :desc "Magit file delete"          "x"   #'magit-file-delete
        :desc "Magit blame"                "B"   #'magit-blame-addition
        :desc "Magit clone"                "C"   #'magit-clone
        :desc "Magit fetch"                "F"   #'magit-fetch
        :desc "Magit buffer log"           "L"   #'magit-log-buffer-file
        :desc "Git stage file"             "S"   #'magit-stage-file
        :desc "Git unstage file"           "U"   #'magit-unstage-file
        (:prefix ("f" . "find")
         :desc "Find file"                 "f"   #'magit-find-file
         :desc "Find gitconfig file"       "g"   #'magit-find-git-config-file
         :desc "Find commit"               "c"   #'magit-show-commit
         :desc "Find issue"                "i"   #'forge-visit-issue
         :desc "Find pull request"         "p"   #'forge-visit-pullreq)
        (:prefix ("o" . "open in browser")
         :desc "Browse file or region"     "."   #'+vc/browse-at-remote
         :desc "Browse homepage"           "h"   #'+vc/browse-at-remote-homepage
         :desc "Browse remote"             "r"   #'forge-browse-remote
         :desc "Browse commit"             "c"   #'forge-browse-commit
         :desc "Browse an issue"           "i"   #'forge-browse-issue
         :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
         :desc "Browse issues"             "I"   #'forge-browse-issues
         :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs)
        (:prefix ("l" . "list")
         (:when (modulep! :tools gist)
          :desc "List gists"               "g"   #'gist-list)
         :desc "List repositories"         "r"   #'magit-list-repositories
         :desc "List submodules"           "s"   #'magit-list-submodules
         :desc "List issues"               "i"   #'forge-list-issues
         :desc "List pull requests"        "p"   #'forge-list-pullreqs
         :desc "List notifications"        "n"   #'forge-list-notifications)
        (:prefix ("c" . "create")
         :desc "Initialize repo"           "r"   #'magit-init
         :desc "Clone repo"                "R"   #'magit-clone
         :desc "Commit"                    "c"   #'magit-commit-create
         :desc "Fixup"                     "f"   #'magit-commit-fixup
         :desc "Issue"                     "i"   #'forge-create-issue
         :desc "Pull request"              "p"   #'forge-create-pullreq)))

      ;;; <leader> w --- workspaces/windows
      ;(:prefix-map ("w" . "workspaces/windows")
      ; (:when (modulep! :ui workspaces)
      ;  :desc "Display workspaces"           "d" #'+workspace/display
      ;  :desc "Rename workspace"             "r" #'+workspace/rename
      ;  :desc "Create workspace"             "c" #'+workspace/new
      ;  :desc "Create named workspace"       "C" #'+workspace/new-named
      ;  :desc "Delete workspace"             "k" #'+workspace/kill
      ;  :desc "Delete saved workspace"       "K" #'+workspace/delete
      ;  :desc "Save workspace"               "S" #'+workspace/save
      ;  :desc "Switch to other workspace"    "o" #'+workspace/other
      ;  :desc "Switch to left workspace"     "p" #'+workspace/switch-left
      ;  :desc "Switch to right workspace"    "n" #'+workspace/switch-right
      ;  :desc "Switch to"                    "w" #'+workspace/switch-to
      ;  :desc "Switch to workspace 1"        "1" #'+workspace/switch-to-0
      ;  :desc "Switch to workspace 2"        "2" #'+workspace/switch-to-1
      ;  :desc "Switch to workspace 3"        "3" #'+workspace/switch-to-2
      ;  :desc "Switch to workspace 4"        "4" #'+workspace/switch-to-3
      ;  :desc "Switch to workspace 5"        "5" #'+workspace/switch-to-4
      ;  :desc "Switch to workspace 6"        "6" #'+workspace/switch-to-5
      ;  :desc "Switch to workspace 7"        "7" #'+workspace/switch-to-6
      ;  :desc "Switch to workspace 8"        "8" #'+workspace/switch-to-7
      ;  :desc "Switch to workspace 9"        "9" #'+workspace/switch-to-8
      ;  :desc "Switch to last workspace"     "0" #'+workspace/switch-to-final)
      ; :desc "Autosave session"             "a" #'doom/quicksave-session
      ; :desc "Save session"                 "s" #'doom/save-session
      ; :desc "Load session"                 "l" #'doom/load-session
      ; :desc "Load last autosaved session"  "L" #'doom/quickload-session
      ; :desc "Undo window config"           "u" #'winner-undo
      ; :desc "Redo window config"           "U" #'winner-redo)

      ;;; <leader> m --- multiple cursors
      (:when (modulep! :editor multiple-cursors)
       (:prefix-map ("m" . "multiple-cursors")
        :desc "Edit lines"         "l"         #'mc/edit-lines
        :desc "Mark next"          "n"         #'mc/mark-next-like-this
        :desc "Unmark next"        "N"         #'mc/unmark-next-like-this
        :desc "Mark previous"      "p"         #'mc/mark-previous-like-this
        :desc "Unmark previous"    "P"         #'mc/unmark-previous-like-this
        :desc "Mark all"           "t"         #'mc/mark-all-like-this
        :desc "Mark all DWIM"      "m"         #'mc/mark-all-like-this-dwim
        :desc "Edit line endings"  "e"         #'mc/edit-ends-of-lines
        :desc "Edit line starts"   "a"         #'mc/edit-beginnings-of-lines
        :desc "Mark tag"           "s"         #'mc/mark-sgml-tag-pair
        :desc "Mark in defun"      "d"         #'mc/mark-all-like-this-in-defun
        :desc "Add cursor w/mouse" "<mouse-1>" #'mc/add-cursor-on-click))

      ;; APPs
      ;;; <leader> M --- mu4e
      (:when (modulep! :email mu4e)
       (:prefix-map ("M" . "mu4e")
        :desc "Open email app" "M" #'=mu4e
        :desc "Compose email"  "c" #'+mu4e/compose))

      ;;; <leader> I --- IRC
      (:when (modulep! :app irc)
       (:prefix-map ("I" . "irc")
        :desc "Open irc app"       "I" #'=irc
        :desc "Next unread buffer" "a" #'tracking-next-buffer
        :desc "Quit irc"           "q" #'+irc/quit
        :desc "Reconnect all"      "r" #'circe-reconnect-all
        :desc "Send message"       "s" #'+irc/send-message
        (:when (modulep! :completion ivy)
         :desc "Jump to channel"  "j" #'+irc/ivy-jump-to-channel)
        (:when (modulep! :completion vertico)
         :desc "Jump to channel"  "j" #'+irc/vertico-jump-to-channel))))


;;
;;; Global & plugin keybinds

(map! "C-'" #'imenu

      ;;; search
      (:when (modulep! :completion ivy)
        "C-S-s"        #'swiper
        "C-S-r"        #'ivy-resume)
      (:when (modulep! :completion helm)
        "C-S-s"        #'swiper-helm
        "C-S-r"        #'helm-resume)
      (:when (modulep! :completion vertico)
        "C-S-r"        #'vertico-repeat)

      ;;; objed
      (:when (modulep! :editor objed +manual)
        ; Not valid on windows
        ;"M-SPC"     #'objed-activate
        )

      ;;; buffer management
      "C-x b"       #'switch-to-buffer
      "C-x 4 b"     #'switch-to-buffer-other-window
      (:when (modulep! :ui workspaces)
        "C-x b"       #'persp-switch-to-buffer
        "C-x B"       #'switch-to-buffer
        "C-x 4 B"     #'switch-to-buffer-other-window
        (:when (modulep! :completion ivy)
          "C-x 4 b"   #'+ivy/switch-workspace-buffer-other-window))
      "C-x C-b"     #'ibuffer
      "C-x K"       #'doom/kill-this-buffer-in-all-windows

      ;;; completion (in-buffer)
      (:when (modulep! :completion company)
       "C-;" #'+company/complete
       (:after company
        :map company-active-map
        "C-o"        #'company-search-kill-others
        "C-n"        #'company-select-next
        "C-p"        #'company-select-previous
        "C-h"        #'company-quickhelp-manual-begin
        "C-S-h"      #'company-show-doc-buffer
        "C-s"        #'company-search-candidates
        "M-s"        #'company-filter-candidates
        [C-tab]      #'company-complete-common-or-cycle
        [tab]        #'company-complete-common-or-cycle
        [backtab]    #'company-select-previous
        "C-RET"      (cond ((modulep! :completion vertico)  #'completion-at-point)
                           ((modulep! :completion ivy)      #'counsel-company)
                           ((modulep! :completion helm)     #'helm-company))
        "C-<return>" (cond ((modulep! :completion vertico)  #'completion-at-point)
                           ((modulep! :completion ivy)      #'counsel-company)
                           ((modulep! :completion helm)     #'helm-company))
        :map company-search-map
        "C-n"        #'company-search-repeat-forward
        "C-p"        #'company-search-repeat-backward
        "C-s"        (cmd! (company-search-abort) (company-filter-candidates))))

      ;;; expand-region
      "C-="  #'er/expand-region

      ;;; flycheck
      (:after flycheck
        :map flycheck-error-list-mode-map
        "C-n" #'flycheck-error-list-next-error
        "C-p" #'flycheck-error-list-previous-error
        "RET" #'flycheck-error-list-goto-error)

      ;;; help and info
      (:after help-mode
        :map help-mode-map
        "o" #'link-hint-open-link
        ">" #'help-go-forward
        "<" #'help-go-back
        "n" #'forward-button
        "p" #'backward-button)
      (:after apropos
        :map apropos-mode-map
        "o" #'link-hint-open-link
        "n" #'forward-button
        "p" #'backward-button)
      (:after info
        :map Info-mode-map
        "o" #'link-hint-open-link)

      ;;; ivy & counsel
      (:when (modulep! :completion ivy)
        (:after ivy
          :map ivy-minibuffer-map
          "TAB"   #'ivy-alt-done
          "C-g"   #'keyboard-escape-quit)
        (:after counsel
          :map counsel-ag-map
          "C-SPC" #'ivy-call-and-recenter ; preview
          "M-RET" #'+ivy/git-grep-other-window-action)
        "C-M-y"   #'counsel-yank-pop)

      ;;; neotree
      (:when (modulep! :ui neotree)
        "<f9>"    #'+neotree/open
        "<C-f9>"  #'+neotree/find-this-file
        (:after neotree
          :map neotree-mode-map
          "q"     #'neotree-hide
          "RET"   #'neotree-enter
          "SPC"   #'neotree-quick-look
          "v"     #'neotree-enter-vertical-split
          "s"     #'neotree-enter-horizontal-split
          "c"     #'neotree-create-node
          "D"     #'neotree-delete-node
          "g"     #'neotree-refresh
          "r"     #'neotree-rename-node
          "R"     #'neotree-refresh
          "h"     #'+neotree/collapse-or-up
          "l"     #'+neotree/expand-or-open
          "n"     #'neotree-next-line
          "p"     #'neotree-previous-line
          "N"     #'neotree-select-next-sibling-node
          "P"     #'neotree-select-previous-sibling-node))

      ;;; popups
      (:when (modulep! :ui popup)
        "C-x p"   #'+popup/other
        "C-`"     #'+popup/toggle
        "C-~"     #'+popup/raise)

      ;;; smartparens
      (:after smartparens
        :map smartparens-mode-map
        "C-M-a"           #'sp-beginning-of-sexp
        "C-M-e"           #'sp-end-of-sexp
        "C-M-f"           #'sp-forward-sexp
        "C-M-b"           #'sp-backward-sexp
        "C-M-n"           #'sp-next-sexp
        "C-M-p"           #'sp-previous-sexp
        "C-M-u"           #'sp-up-sexp
        "C-M-d"           #'sp-down-sexp
        "C-M-k"           #'sp-kill-sexp
        "C-M-t"           #'sp-transpose-sexp
        "C-M-<backspace>" #'sp-splice-sexp)

      ;;; treemacs
      (:when (modulep! :ui treemacs)
        "<f9>"   #'+treemacs/toggle
        "<C-f9>" #'treemacs-find-file))

(map! :leader
      (:when (modulep! :editor fold)
       (:prefix ("C-f" . "fold")
        "C-d"     #'vimish-fold-delete
        "C-a C-d" #'vimish-fold-delete-all
        "C-f"     #'+fold/toggle
        "C-a C-f" #'+fold/close-all
        "C-u"     #'+fold/open
        "C-a C-u" #'+fold/open-all)))
