;;; doctor.el -*- lexical-binding: t; -*-

;; On Windows, emacs sets $SHELL to its own value.
;; In practice, I will usually want ghostel to use the
;; same shell as explicit-shell-file-name.
;; Set manually where this is not good enough.
(when (and (modulep! :term ghostel)
           (featurep :system 'windows)
           (not (bound-and-true-p ghostel-shell)))
  (warn! "ghostel-shell is not set.
On Windows, you probably want to set ghostel-shell explicitly to your desired shell.
Otherwise it will use the default emacs $SHELL value of cmdproxy.exe.

  ;;; add to $DOOMDIR/local/config.el
  (setq-default ghostel-shell \"C:\\Program Files\\Git\\bin\\bash.exe\")"))
