;;; checkers/jinx/config.el -*- lexical-binding: t; -*-

(use-package! jinx
  :hook (text-mode . jinx-mode)
  :general ([remap ispell-word] #'jinx-correct)
  :init
  (when (modulep! +everywhere)
    (add-hook! '(yaml-mode-hook
                 conf-mode-hook
                 prog-mode-hook)
               #'jinx-mode))

  :config

  (after! embark
    ; Found in https://github.com/minad/jinx/discussions/213
    (embark-define-overlay-target jinx category (eq %p 'jinx-overlay))
    (add-to-list 'embark-target-finders 'embark-target-jinx-at-point)
    (add-to-list 'embark-keymap-alist '(jinx jinx-repeat-map embark-general-map))
    (add-to-list 'embark-repeat-actions #'jinx-next)
    (add-to-list 'embark-repeat-actions #'jinx-previous)
    (add-to-list 'embark-target-injection-hooks (list #'jinx-correct #'embark--ignore-target)))

    (map! :map embark-identifier-map "$" #'jinx-correct)
  )
