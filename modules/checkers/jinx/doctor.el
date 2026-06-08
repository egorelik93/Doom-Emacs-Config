;;; checkers/jinx/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "gcc")
  (warn! "Couldn't find gcc; jinx won't be able to compile its native module"))

(unless (or (executable-find "pkg-config")
            (executable-find "pkgconf"))
  (warn! "Couldn't find pkg-config; jinx may fail to locate enchant-2 headers"))

(unless (or (zerop (call-process "pkg-config" nil nil nil "--exists" "enchant-2"))
            (executable-find "enchant-2"))
  (warn! "Couldn't find enchant-2; install it (e.g. libenchant-2-dev) for jinx to work"))
