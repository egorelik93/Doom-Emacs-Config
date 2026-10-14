;;; gnus.el -*- lexical-binding: t; -*-

;; Preferences for GNUS.
;; Do not put server configuration details in here;
;; that is for ~/.gnus.el.

(when (featurep :system 'windows)
  (setq nnheader-file-coding-system 'raw-text-dos))

(after! gnus
  (setq gnus-inhibit-startup-message t
        gnus-asynchronous t
        gnus-use-cache t
        gnus-use-header-prefetch t
        gnus-save-newsrc-file nil         ; .newsrc is only for other readers
        gnus-read-newsrc-file nil
        gnus-always-read-dribble-file t
        gnus-check-new-newsgroups nil
        gnus-agent-go-online t
        gnus-fetch-old-headers 'some
        gnus-show-threads t)

  (setq gnus-parameter-large-newsgroup-initial-alist '((".*" . 200)))

  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-most-recent-date
          (not gnus-thread-sort-by-number)))

  (setq gnus-gcc-mark-as-read t
        message-kill-buffer-on-exit t     ; close after sending
        mm-default-directory
        (file-name-as-directory
         (expand-file-name (or (getenv "XDG_DOWNLOAD_DIR") "Downloads") "~"))
        )

  (map! :map (gnus-mode-map gnus-group-mode-map gnus-summary-mode-map gnus-article-mode-map gnus-browse-mode-map)
        "y" #'which-key-show-top-level)

  (map! :localleader :map (gnus-mode-map gnus-group-mode-map gnus-summary-mode-map gnus-article-mode-map gnus-browse-mode-map)
        "h" #'which-key-show-top-level
        "TAB" #'gnus-info-find-node
        "<tab>" #'gnus-info-find-node)

  ;; From https://gist.github.com/BonfaceKilz/726a1f2c06f15372c2df91fd64895bf9
  (defun my-gnus-group-list-subscribed-groups ()
    "List all subscribed groups with or without un-read messages"
    (interactive)
    (gnus-group-list-all-groups 5))

  (map! :map gnus-group-mode-map
        ;; list all the subscribed groups even they contain zero un-read messages
        "o" #'my-gnus-group-list-subscribed-groups)

  ;; summary
  (setq gnus-sum-thread-tree-false-root      "┬▶"
        gnus-sum-thread-tree-indent          "  "
        gnus-sum-thread-tree-root            "┌ "
        gnus-sum-thread-tree-single-indent "◎ "
        gnus-sum-thread-tree-vertical        "|"
        gnus-sum-thread-tree-leaf-with-other "├─► "
        gnus-sum-thread-tree-single-leaf     "╰─► "
        gnus-thread-hide-subtree nil

        gnus-user-date-format-alist '(((gnus-seconds-today) . "Today %H:%M")
                                    ((+ 86400 (gnus-seconds-today)) . "Yday %H:%M")
                                    (604800 . "%a %H:%M")
                                    (t . "%Y-%m-%d"))

        ;; │06-Jan│  Sender Name  │ Email Subject
        gnus-summary-line-format (concat "%0{%U%R%z%}"
                                         "%3{│%}" "%1{%d%}" "%3{│%}"
                                         "  "
                                         "%4{%-20,20f%}"
                                         "  "
                                         "%3{│%}"
                                         " "
                                         "%1{%B%}"
                                         "%s\n"))

  (setq mm-inline-large-images 'resize)

  ;; Created by Claude based on other doom email modules

    ;; Group buffer: topics are the nearest thing to mu4e's maildir/account tree
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  (setq gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)\n")

  ;; Popup rules / real buffers
  (set-popup-rule! "^\\*\\(Group\\|Summary\\|Article\\)" :ignore t)
  (set-popup-rule! "^\\*Gnus \\(log\\|Server\\)" :select nil)
  (add-hook 'gnus-group-mode-hook #'doom-mark-buffer-as-real-h)

  ;; Line numbers/modeline add nothing to read-only views
  (add-hook! '(gnus-group-mode-hook gnus-summary-mode-hook gnus-article-mode-hook)
             #'doom-disable-line-numbers-h)
  (setq-hook! 'gnus-article-mode-hook truncate-lines nil) ; wrap messages

  ;; HTML mail is sometimes better in a browser
  (map! :map gnus-article-mode-map
        :desc "View HTML in browser" "W" #'gnus-article-browse-html-article)

  (map! :localleader
      :map (gnus-group-mode-map gnus-summary-mode-map gnus-article-mode-map)
      :desc "Compose email" "c" #'gnus-msg-mail
      :desc "Fetch new"     "u" #'gnus-group-get-new-news
      :desc "Quit gnus"     "q" #'gnus-group-exit)
  )

(defun my-dashboard/gnus (&optional arg)
  "Wrapper around gnus compatible with doom dashboard"
  (interactive "P")
  (gnus))

(when (boundp '+dashboard-menu-sections)
  (insert-into-list +dashboard-menu-sections 4
                    '("Open email/news"
                      :icon (nerd-icons-mdicon "nf-md-email_newsletter" :face '+dashboard-menu-title)
                      :when (file-exists-p "~/.gnus.el")
                      :action =gnus)))

(after! message
  (defun my-message-mode-setup ()
    (setq fill-column 72)
    (turn-on-auto-fill))

  (add-hook 'message-mode-hook 'my-message-mode-setup)

  (map! :localleader
        :map message-mode-map
        :desc "send and exit" "s" #'message-send-and-exit
        :desc "kill buffer"   "d" #'message-kill-buffer
        :desc "save draft"    "S" #'message-dont-send
        :desc "attach"        "a" #'mml-attach-file)
)

(use-package! ebdb
  :after-call (gnus-before-startup-hook message-mode-hook)
  :config

  (require 'ebdb-gnus)
  (require 'ebdb-message)

  (setq ebdb-message-auto-update-p
      (lambda ()
        (unless (ebdb-mua-message-header "Newsgroups")
          'create)))

  (setq ebdb-complete-mail 'capf)
  (setq ebdb-mua-pop-up nil)
  (setq ebdb-save-on-exit t)
  )

;; Created by Claude

;; Launcher, analogous to =mu4e / =notmuch
(defun =gnus ()
  "Start (or switch to) Gnus in the current window."
  (interactive)
  (if (and (fboundp 'gnus-alive-p) (gnus-alive-p))
      (switch-to-buffer gnus-group-buffer)
    (gnus)))

;; Leader keys, mirroring the mu4e ones in all-bindings.el (SPC o m, SPC M ...)
;; since mu4e isn't enabled the "M" prefix is free.
(unless (or (modulep! :email mu4e) (modulep! :email notmuch)
            (modulep! :email wanderlust))
  (map! :leader
        (:prefix "o"
         :desc "gnus" "m" #'=gnus)
        (:prefix-map ("M" . "gnus")
         :desc "Open email app" "M" #'=gnus
         :desc "Compose email"  "c" #'gnus-msg-mail
         :desc "Fetch new mail" "u" #'gnus-group-get-new-news
         :desc "Quit gnus"      "q" #'gnus-group-exit)))
