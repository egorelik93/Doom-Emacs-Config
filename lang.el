;;; lang.el -*- lexical-binding: t; -*-

; C

(c-add-style "allman"
             '("k&r"
               (c-basic-offset 4)
               (indent-tabs-mode . nil)))

;(after! cc-mode
;  (setq c-default-style "allman"))

(use-package! google-c-style
  :hook (c-mode-common-hook . google-set-c-style)
  :hook (c-mode-common-hook . google-make-newline-indent)
)

(after! cc-mode
  (set-ligatures! '(c-mode c++-mode c-ts-mode c++-ts-mode) nil)
  (set-ligatures! '(c-mode c++-mode c-ts-mode c++-ts-mode)
;   Copied from cc package, with ones I don't want commented out
    :null "nullptr"
    :true "true" :false "false"
    ; :int "int" :float "float"
    ; :str "std::string"
    ; :bool "bool"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    ; :for "for"
    :return "return"
    :yield "#require"
    )
  )

(after! cc-mode
  ; Resolve tree-sitter bugs
  (add-to-list 'major-mode-remap-alist '(c-mode . nil))
  (add-to-list 'major-mode-remap-alist '(c++-mode . nil))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . nil)))

; Rust

(after! rustic
  (set-ligatures! '(rust-mode rustic-mode)
;   Copied from cc package, with ones I don't want commented out
    :true "true" :false "false"
    ;; Flow
    :and "&&" :or "||"
    )
  )


(after! rustic
  (setq lsp-rust-analyzer-import-granularity "module")

  (setq lsp-rust-analyzer-lens-references-trait-enable t)
  (setq lsp-rust-analyzer-lens-references-method-enable t)
  (setq lsp-rust-analyzer-lens-references-adt-enable t)
  ; Not entirely decided on this; supposedly overlaps with trait references
  ; and I don't feel I've gotten a lot of use out of this.
  (setq lsp-rust-analyzer-lens-implementations-enable t)
  )

(after! (rustic lsp-mode)
  (defun my-lsp-rust-analyzer-debug (runnable)
    (interactive (list (lsp-rust-analyzer--select-runnable)))

    (use-package! dape :demand t)

    (-let (((&rust-analyzer:Runnable
             :args (&rust-analyzer:RunnableArgs :cargo-args :workspace-root? :executable-args)
             :label) runnable))
      (pcase (aref cargo-args 0)
        ("run" (aset cargo-args 0 "build"))
        ("test" (when (-contains? (append cargo-args ()) "--no-run")
                  (cl-callf append cargo-args (list "--no-run")))))
      (->> (append (list (executable-find "cargo"))
                   cargo-args
                   (list "--message-format=json"))
           (s-join " ")
           (shell-command-to-string)
           (s-lines)
           (-keep (lambda (s)
                    (condition-case nil
                        (-let* ((json-object-type 'plist)
                                ((msg &as &plist :reason :executable) (json-read-from-string s)))
                          (when (and executable (string= "compiler-artifact" reason))
                            executable))
                      (error))))
         (funcall
          (lambda (artifact-spec)
            (pcase artifact-spec
              (`() (user-error "No compilation artifacts or obtaining the runnable artifacts failed"))
              (`(,spec) spec)
              (_ (user-error "Multiple compilation artifacts are not supported")))))
         (let ((config (dape--config-eval 'codelldb-rust `(:args ,executable-args))))
           (dape config)))))

  (advice-add #'lsp-rust-analyzer-debug :around (lambda (_ runnable) (my-lsp-rust-analyzer-debug runnable)))
  )

; Haskell

(setq lsp-haskell-formatting-provider "fourmolu")
; My results were not great with this.
;(setq haskell-ts-use-indent t)

(after! haskell-mode
  (map! :localleader
        :map haskell-mode-map
        "b" #'haskell-compile)
  )

(after! haskell-ts-mode
  (map! :localleader
        :map haskell-ts-mode-map
        ; This is haskell-mode's; haskell-ts-mode doesn't have much of a separate command
        "b" #'haskell-compile
        "B" #'haskell-ts-compile-region-and-go
        "r" #'run-haskell
        "C" #'haskell-cabal-visit-file
        )
  )

; Latex

;; This is almost a clone of laas-mathp, but we're using it outside of laas too,
;; to try to avoid taking an unnecessary dependency.
;; In org.el, we advise laas-mathp to have the same behavior as this,
;; so we can use laas-mathp freely wherever laas is loaded.
(defun my/mathp ()
  "Determine whether point is within a LaTeX maths block."
    (cond
     ((derived-mode-p 'latex-mode) (texmathp))
     ((derived-mode-p 'org-mode) (my/org-mathp))
     (t nil)))

;; Created by Claude
(defun my/cdlatex-math-modify-word-default (arg)
  "Like `cdlatex-math-modify', but with 0- and 1-prefix scope swapped.
No prefix arg (nil) acts on the whole word before point (cdlatex's
`backward-word' branch, normally reached with `C-u 1'). A literal
prefix of 1 (`C-u 1') falls back to cdlatex's normal default of
modifying just the last character/group/macro. Any other prefix
arg is passed through unchanged to `cdlatex-math-modify'."
  (interactive "P")
  (cdlatex-math-modify
   (cond
    ((null arg) 1)     ; 0 -> 1: no arg now means "whole word"
    ((eql arg 1) nil)  ; 1 -> 0: C-u 1 now means "last char"
    (t arg))))          ; anything else: untouched

;; Created by Claude
(defun my/latex-smart-text-space-common (orig-fn)
  (if (my/mathp)
      (let* ((end (point))
             (start (save-excursion (skip-chars-backward "a-zA-Z") (point)))
             (word-length (- end start))
             (preceded-by-backslash
              (eq (char-before start) ?\\)))
        (if (and (> word-length 1) (not preceded-by-backslash))
            (let* ((word (buffer-substring-no-properties start end))
                   (gap-start (save-excursion (goto-char start)
                                              (skip-chars-backward " ")
                                              (point)))
                   (prev-is-mathrm
                    (save-excursion
                      (goto-char gap-start)
                      (looking-back "\\\\math[a-zA-Z][a-zA-Z]{[^{}]*}"
                                    (max (point-min) (- (point) 100))))))
              (delete-region start end)          ; only delete the word itself
              (when prev-is-mathrm
                (delete-region gap-start start)  ; only touch the gap if changing it
                (insert "\\ "))
              (progn
                (insert "\\mathrm{" word "}")
                (call-interactively orig-fn)))
          (call-interactively orig-fn)))
        (call-interactively orig-fn)))

(defun my/latex-smart-text-space ()
    "In math mode, wrap the preceding word in \\mathrm{}; otherwise insert a space.
Does not fire on single-letter words or on the argument of a \\command."
    (interactive)
    (my/latex-smart-text-space-common #'self-insert-command))

(after! cdlatex
  (map! :map cdlatex-mode-map
        "C-c ?" nil
        ;; Use "{ TAB" snippet instead
        "C-c {" nil
        "'" #'my/cdlatex-math-modify-word-default
        "SPC" #'my/latex-smart-text-space
        :localleader
        "h" #'cdlatex-command-help
        "{" #'cdlatex-environment
        )

  (push '(?\C-i ("`")) cdlatex-math-symbol-alist)
  )

(after! laas
  (aas-set-snippets 'laas-mode
    :cond #'laas-mathp
    "<-" "\\leftarrow"
    )

  ;; This just doesn't really work for me
  (setq laas-enable-auto-space nil)
  )

; Org

(load! "org.el")
