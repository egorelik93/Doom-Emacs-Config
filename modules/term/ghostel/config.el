;;; term/ghostel/config.el -*- lexical-binding: t; -*-

(use-package! ghostel
  :when (bound-and-true-p module-file-suffix)  ; requires dynamic-modules support
  :commands ghostel-mode
  :hook ((ghostel-mode . mode-line-invisible-mode) ; modeline serves no purpose in vterm
         (ghostel-mode . doom-disable-line-numbers-h))
  :config
  (set-popup-rule! "^\\*ghostel" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)

  (map! :map ghostel-semi-char-mode-map
        "C-q"   #'ghostel-send-next-key)

  (setq-hook! 'ghostel-mode-hook
    ;; Don't prompt about dying processes when killing vterm
    confirm-kill-processes nil
    ;; Prevent premature horizontal scrolling
    hscroll-margin 0))
