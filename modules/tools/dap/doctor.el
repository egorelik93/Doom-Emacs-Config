;;; tools/debugger/doctor.el -*- lexical-binding: t; -*-

(when (and (modulep! :tools lsp +eglot))
  (warn! "dap is not compatible with :tools (lsp +eglot). Choose only one of (eglot or dap-mode) please"))
