;;; tools/vulpea/config.el -*- lexical-binding: t; -*-

(use-package! vulpea
  ;; Load on first input after startup instead of blocking startup.
  ;; Vulpea derives its defaults from `org-directory' at load time, so
  ;; set `org-directory' (or `vulpea-db-sync-directories') in your
  ;; config.el if your notes live somewhere else.
  ;;
  ;; My modified version does something different.
  ;; First, it follows the pattern for org-roam in doom,
  ;; loading the package at config-time, but not starting sync
  ;; until connecting to the database.
  ;; Since vulpea uses file watchers though,
  ;; I have taken the liberty of also starting sync once org-mode begins.
  ;:after-call doom-first-input-hook
  :after org
  :config
  ;; "Setup `vulpea' but don't immediately initialize its database.
  ;; Instead, initialize it when it will be actually needed."
  (require 'vulpea-db-sync)
  ;; (letf! ((#'vulpea-db-sync--start #'ignore))
  ;;   (vulpea-db-autosync-mode +1))

  (defun +vulpea-try-init-db-a (&rest _)
    "Try to initialize vulpea database at the last possible safe moment.
In case of failure, fail gracefully."
    :before #'vulpea-db
    (message "Initializing vulpea database...")
    (advice-remove 'vulpea-db #'+vulpea-try-init-db-a)
    (remove-hook 'org-mode-hook #'+vulpea-try-init-db-a)
    ;;(vulpea-db-sync--start)
    (vulpea-db-autosync-mode +1)
    )

  (advice-add #'vulpea-db :before #'+vulpea-try-init-db-a)
  (add-hook! 'org-mode-hook #'+vulpea-try-init-db-a)
  )

(map! :leader :prefix "n" "v" nil)
(map! :leader
      (:prefix ("n" . "notes")
       (:prefix ("v" . "vulpea")
        :desc "Find note"              "f" #'vulpea-find
        :desc "Find backlink"          "b" #'vulpea-find-backlink
        :desc "Insert link"            "i" #'vulpea-insert
        :desc "Propagate title change" "p" #'vulpea-propagate-title-change
        :desc "Rename tag everywhere"  "r" #'vulpea-tags-batch-rename
        :desc "Full scan"              "s" #'vulpea-db-sync-full-scan
        :desc "Doctor"                 "d" #'vulpea-doctor)))
