;;; lang/org-pretty/doctor.el -*- lexical-binding: t; -*-

(unless (modulep! :lang org)
  (error! "This module requires (:lang org)"))

(unless (file-exists-p (expand-file-name "upstream-hash.eld" (dir!)))
  (warn! "upstream-hash.eld is missing; run 'doom sync' to generate it"))
