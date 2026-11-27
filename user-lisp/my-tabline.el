;;; user-lisp/my-tabline.el -*- lexical-binding: t; no-byte-compile: t; no-native-compile: t; -*-

(require 'doom-lib)
(require 'el-patch)
(require 'centaur-tabs)
(require 'breadcrumb)

(setq breadcrumb-imenu-max-length 0.2)

(defvar my-tabline-splitpoint 0.4)
(defun my-tabline-splitpoint ()
  (floor (* (window-total-width) my-tabline-splitpoint))
  )

(defvar-local my-tabline--splitpoint 0)

;; Changes centaur-tabs to judge visibility based on splitpoint.
(el-patch-defun centaur-tabs-line-format (tabset)
      "Return the `centaur-tabs-display-line-format' value to display TABSET."
  (let* ((sel (centaur-tabs-selected-tab tabset))
         (tabs (centaur-tabs-view tabset))
         (padcolor centaur-tabs-background-color)
         (all-tabs (centaur-tabs-tabs tabset))
         (total-tabs (length all-tabs))
         (sel-index (+ (cl-position (car sel) (mapcar 'car all-tabs)) 1))
         ;; Careful! This needs to be up here because my-tabline--splitpoint is
         ;; buffer-local; once we enter the temp buffer, we lose the intended value.
         (el-patch-add (splitpoint my-tabline--splitpoint))
         atsel elts)
    ;; Track the selected tab to ensure it is always visible.
    (when centaur-tabs--track-selected
      (while (not (memq sel tabs))
        (centaur-tabs-scroll tabset -1)
        (setq tabs (centaur-tabs-view tabset)))
      (while (and tabs (not atsel))
        (setq elts  (cons (centaur-tabs-line-tab (car tabs)) elts)
              atsel (eq (car tabs) sel)
              tabs  (cdr tabs)))
      (setq elts (nreverse elts))
      ;; At this point the selected tab is the last elt in ELTS.
      ;; Scroll TABSET and ELTS until the selected tab becomes
      ;; visible.
      (let (buffer-list-update-hook)
        (with-temp-buffer
          (let ((truncate-partial-width-windows nil)
                (inhibit-modification-hooks t)
                deactivate-mark ;; Prevent deactivation of the mark!
                start)
            (setq truncate-lines nil
                  buffer-undo-list t)
            (setq start (point))
            (while (and (cdr elts) ;; Always show the selected tab!
                        (progn
                          (delete-region start (point-max))
                          (el-patch-add (insert (make-string splitpoint ?\s)))
                          (goto-char (point-max))
                          (apply #'insert elts)
                          (goto-char (point-min))
                          (> (vertical-motion 1) 0)))
              (centaur-tabs-scroll tabset -1)
              (setq elts (cdr elts))))))
      (setq elts (nreverse elts))
      (setq centaur-tabs--track-selected nil))
    ;; Format remaining tabs.
    (while tabs
      (setq elts (cons (centaur-tabs-line-tab (car tabs)) elts)
            tabs (cdr tabs)))
    ;; Cache and return the new tab bar.
    (centaur-tabs-set-template
     tabset
     (list
      (centaur-tabs-count sel-index total-tabs)
      (centaur-tabs-line-format--buttons)
      (nreverse elts)
      (propertize "% "
                  'face (list :background padcolor)
                  'pointer 'arrow)
      (centaur-tabs-line-format--new-button)))))

;; Normally splits the centaur tab bar into two parts, placing
;; imenu breadcrumbs on the left and aligning tabs to the right.
;; When the window becomes too small, it drops the tabs entirely,
;; and adds the buffer name to the breadcrumbs.
;; This makes for a nice intermediate while undecided on the question of tabs.
(setq centaur-tabs-header-line-format '(:eval (my-centaur-tabs-line)))
(defun my-centaur-tabs-line ()
    "Return the header line templates that represent the tab bar.
Inhibit display of the tab bar in current window where
`centaur-tabs-hide-tab-function' return t."
  (cond ((or (centaur-tabs-hide-tab-cached (current-buffer))
             (and centaur-tabs-hide-predicate
                  (funcall centaur-tabs-hide-predicate)))
         ;; Don't show the tab bar.
         (set centaur-tabs-display-line-format nil))
        ((centaur-tabs-current-tabset t)

         (let* ((crumbs (unless header-line-format (breadcrumb-imenu-crumbs)))
                (crumbs-width (if crumbs (string-width crumbs) 0))
                (splitpoint (if header-line-format 0 (my-tabline-splitpoint)))
                tabs tabs-width align)

           (cond
            ((< splitpoint 20))
            ((= my-tabline--splitpoint splitpoint)
             (setq tabs (or (centaur-tabs-template centaur-tabs-current-tabset)
                            (centaur-tabs-line-format centaur-tabs-current-tabset))))
            (t
             (setq-local my-tabline--splitpoint splitpoint)
             (setq tabs (centaur-tabs-line-format centaur-tabs-current-tabset))
             )
            )

           (when tabs
             (setq tabs (format-mode-line tabs))
             (setq tabs-width (string-width tabs))

             (setq align (min (- (window-total-width) splitpoint) tabs-width))
             (setq splitpoint (- (window-total-width) align)))

           (concat
            " "
            (centaur-tabs--icon-for-buffer
             :v-adjust centaur-tabs-icon-v-adjust
             :height centaur-tabs-icon-scale-factor)
            " "
            (if tabs
                (string-limit crumbs (max 0 (- splitpoint 4)))
              (concat (truncate-string-to-width (buffer-name) 12 nil nil t) " > " crumbs))
            (when tabs (propertize " " 'display `((space :align-to (- right ,align)))))
            tabs
            )))))

(provide 'my-tabline)
