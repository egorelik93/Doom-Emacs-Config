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

  (when (modulep! :lang org +roam)
    (add-to-list 'org-capture-templates
                 '("r" "Org-Roam Node" entry
                   (function org-roam-capture)
                   ""
                   :immediate-finish t)))

  (when (and (modulep! :lang org +roam) (modulep! vulpea))
    ; Modified from https://www.d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5

    (defun vulpea-todo-p ()
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

    (defun vulpea-todo-update-tag ()
      "Update todo tag in the current buffer."
      (when (and (not (active-minibuffer-window))
                 (vulpea-buffer-p))
        (save-excursion
          (goto-char (point-min))
          (let* ((tags (vulpea-buffer-tags-get t))
                 ;(tags (my/org-roam-buffer-tags-get))
                 ;(original-tags tags)
                 (tagged (and (member "todo" tags)))
                 )

            (if (vulpea-todo-p)
              ;  (when (not (seq-contains-p tags "todo"))
              ;    (org-roam-tag-add '("todo")))
              ;(when (seq-contains-p tags "todo")
              ;  (org-roam-tag-remove '("todo")))
                (when (not tagged)
                  (vulpea-buffer-tags-add "todo"))
              (when tagged
                (vulpea-buffer-tags-remove "todo")
                )
              )
            ))))

    (defun vulpea-buffer-p ()
      "Return non-nil if the currently visited buffer is a note."
      (when-let* ((file (buffer-file-name))
                  (dirs (bound-and-true-p vulpea-db-sync-directories)))
        (let ((file (expand-file-name file)))
          (seq-some
           (lambda (dir)
             (string-prefix-p
              (file-name-as-directory (expand-file-name dir))
              file))
           dirs
           )))
      ;(and buffer-file-name
      ;     (string-prefix-p
      ;      (expand-file-name (file-name-as-directory org-roam-directory))
      ;      (file-name-directory buffer-file-name)))
      )

    (defun vulpea-todo-files ()
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

    (defun vulpea-agenda-files-update (&rest _)
      "Update the value of `org-agenda-files'."
      (setq org-agenda-files (vulpea-todo-files)))

    (add-hook 'find-file-hook #'vulpea-todo-update-tag)
    (add-hook 'before-save-hook #'vulpea-todo-update-tag)

    (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
    (advice-add 'org-todo-list :before #'vulpea-agenda-files-update)
    )
  )

(use-package! org-fragtog
  :hook (org-mode-hook . org-fragtog-mode))

; Move this to V, to make room for vulpea
(map! :leader :prefix "n" :desc "View search" "V" #'org-search-view)
