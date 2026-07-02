;;; editor/aas/config.el -*- lexical-binding: t; -*-

(use-package! aas
  :hook (LaTeX-mode . ass-activate-for-major-mode)
  :hook (org-mode . ass-activate-for-major-mode))

(use-package! laas
  :when (modulep! +laas)
  :hook (LaTeX-mode . laas-mode)
  :hook (org-mode . laas-mode)
  )
