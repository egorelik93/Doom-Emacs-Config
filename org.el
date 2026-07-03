;;; org.el -*- lexical-binding: t; -*-

(setq org-return-follows-link t)


(after! '(org evil)
  (advice-add #'org-mode-restart :around
              (lambda (orig-fn)
                (let ((current-evil-state evil-state))
                  (funcall orig-fn)
                  (unless (eq current-evil-state evil-state)
                      (evil-change-state current-evil-state)))))
  )


(defun +org-capture/open-frame-ms (&optional initial-input key)
  "Opens the org-capture window in the current frame, closing it once
you're done. This can be called from an external script on Windows."
  (interactive)
  (when (and initial-input (string-empty-p initial-input))
    (setq initial-input nil))
  (when (and key (string-empty-p key))
    (setq key nil))
  (let* ((frame-title-format "")
         (frame (selected-frame)))

    (when (not (+org-capture-frame-p))
      (modify-frame-parameters nil +org-capture-frame-parameters))
    (with-selected-frame frame
      (require 'org-capture)
      (doom/reload-font)
      (condition-case ex
          (letf! ((#'pop-to-buffer #'switch-to-buffer))
            (switch-to-buffer (doom-fallback-buffer))
            (let ((org-capture-initial initial-input)
                  org-capture-entry)
              (when (and key (not (string-empty-p key)))
                (setq org-capture-entry (org-capture-select-template key)))
              (funcall +org-capture-fn)))
        ('error
         (message "org-capture: %s" (error-message-string ex))
         (delete-frame frame))))))

;(advice-add #'org-capture-select-template :before (lambda (&rest _) (doom/reload-font)))
;(add-hook! org-capture-mode-hook (lambda (_) (doom/reload-font)))


(defun insert-into-list (list n el)
  "Insert into list LIST an element EL at index N.

If N is 0, EL is inserted before the first element.

The resulting list is returned.  As the list contents is mutated
in-place, the old list reference does not remain valid."
  (let* ((padded-list (cons nil list))
         (c (nthcdr n padded-list)))
    (setcdr c (cons el (cdr c)))
    (cdr padded-list)))

(insert-into-list +dashboard-menu-sections 3
  '("Find org-roam node"
    :icon (nerd-icons-icon-for-mode 'org-mode :face '+dashboard-menu-title)
    :when (modulep! :lang org +roam)
    :face (:inherit (+dashboard-menu-title bold))
    :action org-roam-node-find))

(after! org-protocol
  ;(advice-add #'org-capture-select-template
  ;            :around
  ;            (lambda (fn &rest args)
  ;              (condition-case err
  ;                  (apply fn args)
  ;                (user-error
  ;                 (when (string= (cadr err) "Abort")
  ;                   (server-edit)
  ;                   nil)))))

  (advice-add #'org-protocol-check-filename-for-protocol :before (lambda (&rest _) (doom/reload-font)))
  )

;(add-hook 'org-capture-mode-hook (lambda () (redisplay t)))

(after! org
  (setq org-hide-emphasis-markers t)

  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-latex-create-formula-image-program 'dvisvgm)

  ; Taken from doom org module;
  ; since it uses evil-org-mode package,
  ; my redirect-evil-to-boon doesn't automatically apply.
  (map! :mode org-mode
        ;; more intuitive RET keybinds
        :m "RET"      #'+org/dwim-at-point
        :m "<return>" #'+org/dwim-at-point
        :i "RET"      #'+org/return
        :i "<return>" #'+org/return
        :i [S-return] #'+org/shift-return
        :i "S-RET"    #'+org/shift-return)

  ;; Modified from org-cdlatex-math-modify
  (defun my/org-cdlatex-math-modify-word-default (&optional _arg)
  "Execute `my/cdlatex-math-modify-word-default' in LaTeX fragments.
Revert to the normal definition outside of these fragments."
  (interactive "P")
  (if (org-inside-LaTeX-fragment-p)
      (call-interactively 'my/cdlatex-math-modify-word-default)
    (let (org-cdlatex-mode)
      (call-interactively (key-binding (vector last-input-event))))))

  (map! :map org-cdlatex-mode-map
        "C-c {" nil
        "'" #'my/org-cdlatex-math-modify-word-default
        :localleader
        "{" #'org-cdlatex-environment-indent
        )

  (defun my/org-latex-preview-all (&optional arg)
    (interactive "P")
    (org-latex-preview (my/shift-prefix arg 2)))

  ; Created by Claude
  (defun my/shift-prefix (arg n)
    "Shift raw prefix ARG up by N levels of C-u (each level = x4).
Handles the common raw-prefix forms: nil, a cons like (4), or
a plain integer (from C-u 3 etc.)."
    (let ((base (cond
                 ((null arg) 1)                 ; no prefix => treat as C-u^0
                 ((consp arg) (car arg))         ; (4), (16), ...
                 ((integerp arg) arg)            ; numeric arg, e.g. C-u 3
                 (t 1))))
      (list (* base (expt 4 n)))))

  (map! :localleader :mode org-mode
        :prefix ("z" . "previews/misc")
        :desc "Preview latex" "l" #'org-latex-preview
        :desc "Preview all latex" "L" #'my/org-latex-preview-all
        )
  ; Not sure if this is true for all machines,
  ; but in the environments I have tried this on, the scale is too big.
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 0.5))

  (defun my/org-set-created-property ()
    "Set CREATED property with current timestamp if not already set."
    (unless (org-entry-get nil "CREATED")
      (org-set-property "CREATED"
                        (format-time-string "[%Y-%m-%d %a %H:%M]"))))

  (defun my/org-set-created-property-if-chronological ()
    (when (member "chronological" (org-get-tags))
      (my/org-set-created-property)
      ))
  (add-hook 'org-insert-heading-hook #'my/org-set-created-property-if-chronological)

  (when (modulep! :lang org +roam)
    (add-to-list 'org-capture-templates
                 '("r" "Org-Roam Node" entry
                   (function org-roam-capture)
                   ""
                   :immediate-finish t)))

  ;; The following utilities are based on code from vulpea,
  ;; in particular the blog series on scaling agenda.

  (defvar my/org-automate-directories nil
    "Directories that org automation utilities should include.
If using vulpea, probably will want to set this to vulpea-db-sync-directories.")

  (defun my/org-automate-buffer-p ()
    "Return non-nil if the currently visited buffer should accept automation."
    (or (member "my/org-automate" (org-get-tags))
        (when-let* ((file (buffer-file-name))
                    (dirs (bound-and-true-p my/org-automate-directories)))
          (let ((file (expand-file-name file)))
            (seq-some
             (lambda (dir)
               (string-prefix-p
                (file-name-as-directory (expand-file-name dir))
                file))
             dirs
             ))))
    ;;(and buffer-file-name
    ;;     (string-prefix-p
    ;;      (expand-file-name (file-name-as-directory org-roam-directory))
    ;;      (file-name-directory buffer-file-name)))
    )

  (defvar my/org-automate-file-update-hook nil)
  (defun my/org-automate-file-update ()
    (when (and (not (active-minibuffer-window))
               (my/org-automate-buffer-p))
      (run-hooks 'my/org-automate-file-update-hook)
      ))

  (add-hook 'org-mode-hook (defun my/install-org-automate-file-update-hook ()
                             (when my/org-automate-directories
                               (add-hook 'find-file-hook #'my/org-automate-file-update nil t)
                               (add-hook 'before-save-hook #'my/org-automate-file-update nil t)
                               )))

  (defun my/org-chronological-add-dates ()
    "Add dates to headers in chronological buffers."
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (org-get-tags))
             (chronological (member "chronological" tags))
             )
        (when chronological
          (org-map-entries
           (lambda ()
             (unless (org-entry-get nil "CREATED")
               (org-set-property "CREATED"
                                 (format-time-string "[%Y-%m-%d %a %H:%M]"))
               )
             ))))))
  (add-hook 'my/org-automate-file-update-hook #'my/org-chronological-add-dates)

  (when (and (modulep! :lang org +roam) (modulep! :tools vulpea))
    ; Modified from https://www.d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5

    (defun my/vulpea-todo-p ()
      "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
      (seq-find                                 ; (3)
       (lambda (type)
         (eq type 'todo))
       (org-element-map                         ; (2)
           (org-element-parse-buffer 'headline) ; (1)
           'headline
         (lambda (h)
           (org-element-property :todo-type h)))))

    ; May not need this anymore if using vulpea v2
    (defun my/org-roam-buffer-tags-get ()
      (let ((node (org-roam-node-at-point)))
        (if (org-roam-node-p node)
            (org-roam-node-tags node)
          nil)
        )
      )

    (defvar my/vulpea-todo-update-hook)

    (defun my/vulpea-todo-update-tag ()
      "Update todo tag in the current buffer."
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get t))
               ;;(tags (my/org-roam-buffer-tags-get))
               ;;(original-tags tags)
               (tagged (and (member "todo" tags)))
               )

          (if (my/vulpea-todo-p)
              ;;  (when (not (seq-contains-p tags "todo"))
              ;;    (org-roam-tag-add '("todo")))
              ;;(when (seq-contains-p tags "todo")
              ;;  (org-roam-tag-remove '("todo")))
              (when (not tagged)
                (vulpea-buffer-tags-add "todo")
                (run-hooks 'my/vulpea-todo-update-hook)
                )
            (when tagged
              (vulpea-buffer-tags-remove "todo")
              (run-hooks 'my/vulpea-todo-update-hook)
              )
            )
          )))

    (defun my/vulpea-todo-files ()
      "Return a list of note files containing 'todo' tag." ;
      (seq-uniq
       (seq-map
        #'vulpea-note-path
        (vulpea-db-query-by-tags-some '("todo"))
      ;  #'car
      ;  (org-roam-db-query
      ;   [:select [nodes:file]
      ;    :from tags
      ;    :left-join nodes
      ;    :on (= tags:node-id nodes:id)
      ;    :where (like tag (quote "%\"todo\"%"))])
      )))

    (defun my/vulpea-agenda-files-update (&rest _)
      "Update the value of `org-agenda-files'."
      (setq org-agenda-files (my/vulpea-todo-files)))

    (add-hook 'my/org-automate-file-update-hook #'my/vulpea-todo-update-tag)

    (advice-add 'org-agenda :before #'my/vulpea-agenda-files-update)
    (advice-add 'org-todo-list :before #'my/vulpea-agenda-files-update)
    )
  )

(use-package! org-fragtog
  :hook (org-mode-hook . org-fragtog-mode))

; Move this to V, to make room for vulpea
(map! :leader :prefix "n" :desc "View search" "V" #'org-search-view)


;;; Vulpea TODO aggregation
;; Created by Claude Code

(defvar my/vulpea-aggregate-todo-file nil
  "Path where aggregated vulpea TODOs are written. Set in config.local.el.")

(defvar my/vulpea-aggregate-timer nil)

(defvar my/vulpea-aggregate-dirty nil
  "Non-nil when the aggregated TODO file may be out of date.")

(defvar my/vulpea-aggregate-exclude-tag "todos-aggregate-exclude"
  "Vulpea tag that opts a note out of TODO aggregation.")

(defun my/vulpea--entry-planning-line ()
  "Return the raw planning line for the heading at point, or nil."
  (save-excursion
    (forward-line 1)
    (when (looking-at-p org-planning-line-re)
      (string-trim
       (buffer-substring-no-properties (line-beginning-position)
                                       (line-end-position))))))

(defun my/vulpea--entry-properties-drawer ()
  "Return a formatted PROPERTIES drawer string for the heading at point, or nil."
  (let ((props (org-entry-properties nil 'standard)))
    (when props
      (concat "  :PROPERTIES:\n"
              (mapconcat (lambda (p) (format "  :%s: %s" (car p) (cdr p)))
                         props "\n")
              "\n  :END:"))))

(defun my/vulpea--extract-todos (file)
  "Return a list of todo plists for all active TODOs in FILE."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (let (results)
       (org-map-entries
        (lambda ()
          (let ((kw (org-get-todo-state)))
            (when (and kw (not (member kw org-done-keywords)))
              (push (list :keyword kw
                          :title   (org-get-heading t t t t)
                          :tags    (org-get-tags nil t)
                          :plan    (my/vulpea--entry-planning-line)
                          :props   (my/vulpea--entry-properties-drawer))
                    results))))
        t nil)
       (nreverse results)))))

(defun my/vulpea-aggregate-stale-p ()
  "Return non-nil if the aggregate file is missing or older than any todo source file."
  (or (not (file-exists-p my/vulpea-aggregate-todo-file))
      (let ((agg-mtime (file-attribute-modification-time
                        (file-attributes my/vulpea-aggregate-todo-file))))
        (seq-some (lambda (f)
                    (time-less-p agg-mtime
                                 (file-attribute-modification-time
                                  (file-attributes f))))
                  (my/vulpea-todo-files)))))

(defun my/vulpea-aggregate-todos ()
  "Collect active TODOs from vulpea todo files and write to `my/vulpea-aggregate-todo-file'.
Skips the write when called non-interactively and nothing has changed."
  (interactive)
  (cond
   ((not my/vulpea-aggregate-todo-file)
    (when (called-interactively-p 'any)
      (user-error "Set my/vulpea-aggregate-todo-file in config.local.el")))
   ((and (not my/vulpea-aggregate-dirty)
         (not (called-interactively-p 'any))
         (not (my/vulpea-aggregate-stale-p)))
    nil)
   (t
    (condition-case err
        (let ((notes (seq-remove
                      (lambda (n)
                        (member my/vulpea-aggregate-exclude-tag (vulpea-note-tags n)))
                      (vulpea-db-query-by-tags-some '("todo"))))
              (seen  (make-hash-table :test 'equal)))
          (with-temp-file my/vulpea-aggregate-todo-file
            (insert "#+TITLE: Aggregated Desktop TODOs\n"
                    "# Auto-generated. Do not edit manually.\n"
                    (format "# Last updated: %s\n\n"
                            (format-time-string "%Y-%m-%d %a %H:%M")))
            (dolist (note notes)
              (let ((file (vulpea-note-path note)))
                (unless (gethash file seen)
                  (puthash file t seen)
                  (let ((title (vulpea-note-title note))
                        (todos (my/vulpea--extract-todos file)))
                    (when todos
                      (insert (format "* [[file:%s][%s]]\n" file title))
                      (dolist (todo todos)
                        (let* ((kw      (plist-get todo :keyword))
                               (heading (plist-get todo :title))
                               (tags    (plist-get todo :tags))
                               (plan    (plist-get todo :plan))
                               (props   (plist-get todo :props))
                               (tag-str (if tags
                                            (format " :%s:" (string-join tags ":"))
                                          "")))
                          (insert (format "** %s %s%s\n" kw heading tag-str))
                          (when plan  (insert "   " plan  "\n"))
                          (when props (insert props "\n"))))))))))
          (setq my/vulpea-aggregate-dirty nil)
          (message "vulpea-aggregate: wrote %s" my/vulpea-aggregate-todo-file))
      (error (message "vulpea-aggregate: %s" (error-message-string err)))))))

(defun my/vulpea-aggregate-setup (&rest _)
  "Start the aggregation timer and register hooks, if the output file is configured."
  (when my/vulpea-aggregate-todo-file
    (add-hook 'my/vulpea-todo-update-hook
              (lambda () (setq my/vulpea-aggregate-dirty t)))
    (add-hook 'org-mode-hook
              (lambda ()
                (add-hook 'after-save-hook
                          (lambda ()
                            (when (and (my/org-automate-buffer-p)
                                       (let ((tags (vulpea-buffer-tags-get t)))
                                         (and (member "todo" tags)
                                              (not (member my/vulpea-aggregate-exclude-tag tags)))))
                              (setq my/vulpea-aggregate-dirty t)))
                          nil
                          t
                          )))
    (run-with-idle-timer 10 nil #'my/vulpea-aggregate-todos)
    (when my/vulpea-aggregate-timer (cancel-timer my/vulpea-aggregate-timer))
    (setq my/vulpea-aggregate-timer
          (run-with-timer 3600 3600 #'my/vulpea-aggregate-todos))
    (add-hook 'kill-emacs-hook #'my/vulpea-aggregate-todos)))

; Opt in from config.local.el:
;   (setq my/vulpea-aggregate-todo-file "/path/to/todos.org")
;   (add-hook 'emacs-startup-hook #'my/vulpea-aggregate-setup)
;
; Alternate initialization using my vulpea module modifications:
;   (setq my/vulpea-aggregate-todo-file "/path/to/todos.org")
;   (after! vulpea
;     (advice-add #'+vulpea-try-init-db-a :after #'my/vulpea-aggregate-setup))

(after! (laas org)
  ; The standard org--mathp advice on texmathp breaks laas
  (defun my/laaas-org--math-p (orig-texmathp &rest args)
    (apply orig-texmathp args))

  (defun my/laas-org-mathp-texmathp (org-mathp)
    (if org-mathp
        (letf! ((#'org--math-p #'my/laaas-org--math-p))
          (texmathp)
          )
      nil))

  (advice-add #'laas-org-mathp :filter-return #'my/laas-org-mathp-texmathp)
  )

(defun my/org-latex-smart-text-space ()
    "In math mode, wrap the preceding word in \\mathrm{}; otherwise insert a space.
Does not fire on single-letter words or on the argument of a \\command."
    (interactive)
    (my/latex-smart-text-space-common #'org-self-insert-command))

(after! (laas org)
  (map! :map org-cdlatex-mode-map
        "SPC" #'my/org-latex-smart-text-space)
  )
