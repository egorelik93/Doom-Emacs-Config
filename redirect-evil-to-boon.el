;;; redirect-evil-to-boon.el -*- lexical-binding: t; -*-

(defmacro my-after-boon-keymap (&rest body)
  `(after! my-redirect-evil-to-boon ,@body)
  )

(static-when my-enable-evil-like-keymap
  (defvar my-evil-like-keymap (make-sparse-keymap "Extra keymap for unredirected evil bindings to go")))

(defun my/keymap-major-mode-p (keymap-symbol)
  "Return the major mode that uses KEYMAP-SYMBOL, or nil."
  (and (boundp keymap-symbol)
       (keymapp (symbol-value keymap-symbol))
       (string-suffix-p "-map" (symbol-name keymap-symbol))
       (let ((mode (intern-soft (string-remove-suffix "-map"
                                                      (symbol-name keymap-symbol)))))
         (and mode
              (fboundp mode)
              (or (get mode 'derived-mode-parent)
                  (get mode 'mode-class)
                  (rassq mode auto-mode-alist)
                  (rassq mode interpreter-mode-alist))
              mode))))

(defun my/get-boon-mode-map (maps state)
  (cond
   ((null maps) nil)
   ((listp maps) (remq nil (mapcar (lambda (m) (my/get-boon-mode-map m state)) maps)))
   (t
    (if-let ((mode (my/keymap-major-mode-p maps))
             (boon-map-cell (cond
                             ((memq state '(normal operator motion replace))
                              '(boon-map . boon-command-map))
                             (t nil)))
             )
        (let* ((boon-map-property (car boon-map-cell))
               (boon-map (cdr boon-map-cell))
               (boon-map-sym (intern (format "my/boon-%s-%s-map" mode state)))
               (keymap (get mode boon-map-property)))
          (when (null keymap)
            (setq keymap (make-sparse-keymap (format "Boon %s keymap for %s" state mode)))
            (put mode boon-map-property keymap)
            (after! boon (set-keymap-parent keymap (eval boon-map))))
          (unless (and (boundp boon-map-sym) (eq (symbol-value boon-map-sym) keymap))
            (set boon-map-sym keymap))
          boon-map-sym)
      nil))))

(defun my-sequence-starts-with-modifier-p (key-seq)
  "Return t if KEY-SEQ (vector, string, symbol, or list) begins with a modifier."
  (let ((first-event (cond
                      ((vectorp key-seq) (and (> (length key-seq) 0) (elt key-seq 0)))
                      ((stringp key-seq) (and (> (length key-seq) 0) (elt key-seq 0)))
                      ((listp key-seq)   (car key-seq))
                      (t                 key-seq)))) ; Handles single symbol/integer
    (and first-event
         (or
          (not (null (event-modifiers first-event)))
          (eq first-event ?g)
          ))))

(defmacro my-define-doom--map-commit ()
  (let* (
         (evil-case
          (static-if my-enable-evil
              ``(,(or doom--map-fn 'general-define-key)
                 ,@(if state `(:states ',state)) ,@attrs
                 ,@(mapcan #'identity (nreverse defs)))))
         (non-evil-cases
          (list
           (static-when (not my-enable-evil)
             `((or (not state) (eq state 'emacs) (eq state 'global)
                   (and evil-disable-insert-state-bindings (eq state 'insert)))
               `(,(or doom--map-fn 'general-define-key)
                 ,@attrs
                 ,@(mapcan #'identity (nreverse defs))))
             )
           (static-when (or my-redirect-evil-maps my-enable-evil-like-keymap)
             `((and (or (eq state 'normal) (eq state 'motion))
                    (or (null keymaps) ,(static-when my-enable-evil-like-keymap `(eq keymaps 'evil-like-keymap))))
               (let* ((split (
                             ,@(static-if my-redirect-evil-maps
                                   `(cl-loop for def in defs
                                             if (or
                                                 (plist-get attrs :prefix)
                                                 (my-sequence-starts-with-modifier-p (car def)))
                                             ,@(static-if
                                                   (or my-enable-evil
                                                       (and my-redirect-evil-maps my-enable-evil-like-keymap))
                                                   `(collect (copy-sequence def) into with-modifiers)
                                                 `(collect def into with-modifiers))
                                             ,@(static-if my-enable-evil-like-keymap
                                                   `(collect ,(static-if my-enable-evil
                                                                  `(copy-sequence def)
                                                                `def)
                                                     into all-defs
                                                     finally return (cons with-modifiers all-defs))
                                                 `(finally return with-modifiers)))
                                 `(mapcan #'copy-sequence defs))))
                     ,@(static-when (and my-redirect-evil-maps my-enable-evil-like-keymap)
                         `(
                           (with-modifiers (car split))
                           (all-defs (cdr split))))
                     ,@(static-when my-redirect-evil-maps
                         `((with-modifiers split))
                         )
                     ,@(static-when my-enable-evil-like-keymap
                         `((all-defs split))
                         )
                        )
                 ,@(static-when my-log-evil-keybinds
                     `((message "Evil Global Normal Keybind: State %s Attrs %s Defs %s" state attrs defs))
                     )
                 ,(macroexp-progn
                   (remq
                    nil
                    (list
                     (static-when my-redirect-evil-maps
                       `(when with-modifiers
                          ; Must run after boon-qwerty is loaded, which happens in config.el
                          `(my-after-boon-keymap
                            (,(or doom--map-fn 'general-define-key)
                             ,@(plist-put attrs :keymaps ''boon-command-map)
                             ,@(mapcan #'identity (nreverse with-modifiers))
                            ))))
                     (static-when my-enable-evil-like-keymap
                       ``(,(or doom--map-fn 'general-define-key)
                          ,@(plist-put attrs :keymaps ''my-evil-like-keymap)
                          ,@(mapcan #'identity (nreverse all-defs))))
                     ))))))
           `((and keymaps (or (eq state 'normal) (eq state 'motion)))
             ,@(static-when my-log-evil-keybinds
                `((message "Evil Map Normal Keybind: State %s Attrs %s Defs %s" state attrs defs))
                )
             `(when-let* ((boon-maps (my/get-boon-mode-map ',(eval keymaps) ',state)))
                  (apply #',(or doom--map-fn 'general-define-key)
                         (append
                          (plist-put ',attrs :keymaps boon-maps)
                          ',(mapcan ,(static-if my-enable-evil `#'copy-sequence `#'identity) (nreverse defs)))))
             )
           ))
         (non-evil-cases-stripped (remq nil non-evil-cases))
         (collection
          (cond
           ((and evil-case non-evil-cases-stripped)
            `(list ,evil-case (cond ,@non-evil-cases-stripped)))
           (evil-case evil-case)
           (non-evil-cases-stripped `(cond ,@non-evil-cases-stripped))
           (t 'nil)))
          )
    `(when doom--map-batch-forms
       (cl-loop with attrs = (doom--map-state)
                with keymaps = (plist-get attrs :keymaps)
                for (state . defs) in doom--map-batch-forms
                ,@(static-unless my-enable-evil
                    `(if (or (not state) (eq state 'emacs) (eq state 'insert) (eq state 'normal) (eq state 'motion))))
                ,@(static-if my-enable-evil
                      `(nconc ,collection)
                    `(collect ,collection))
                into forms
                finally do
                ,@(static-when my-log-evil-keybinds
                    '((message "%S" forms)))
                (push (macroexp-progn forms) doom--map-forms))
       (setq doom--map-batch-forms nil)
       )
    ))

(defun doom--map-commit ()
  (my-define-doom--map-commit)
)
