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

; Org

(load! "org.el")

; Latex

(after! cdlatex
  (map! :map cdlatex-mode-map
        "C-c ?" nil
        ;; Use "{ TAB" snippet instead
        "C-c {" nil
        :localleader
        "h" #'cdlatex-command-help
        "{" #'cdlatex-environment
        )
  )
