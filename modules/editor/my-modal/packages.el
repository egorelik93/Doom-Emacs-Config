;; -*- no-byte-compile: t; -*-
;;; modules/editor/my-modal/packages.el

; The built-in module enables it globally without any opportunity to disable,
; which is not what I want.
(package! god-mode)

(when my-enable-evil
  (package! evil-tutor))

(package! boon)

; Using this to allow an alternate path to C-c menu, without needing god mode.
(package! theist-mode
  :recipe (:host github :repo "leotaku/theist-mode"))
