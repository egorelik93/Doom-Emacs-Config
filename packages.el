;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


(defun my/dir-fingerprint (dir)
  "Newest mtime among DIR's files, as a float, ignoring .git."
  (let ((files (directory-files-recursively
                dir "\\.el\\'" nil
                (lambda (d) (not (string-match-p "/\\.git\\'" d))))))
    (if files
        (apply #'max (mapcar (lambda (f)
                               (float-time (file-attribute-modification-time
                                            (file-attributes f))))
                             files))
      0)))

(cl-defmacro local-package! (package &key (local-repo nil) (files nil))
  `(let ((fingerprint (my/dir-fingerprint (expand-file-name ,local-repo (dir!)))))
     (package! ,package
       :type 'local
       :recipe (
                :local-repo ,local-repo
                ,@(when files `((:files ,files)))
                :build t)
       :env (list (cons 'my-fingerprint fingerprint)))
    )
  )

; Must be before any package declarations in this file.
;
(when (file-exists-p (concat doom-user-dir "local/packages.el"))
  (load! (concat doom-user-dir "local/packages.el")))


;; Me: Packages can only be disabled once, and that invalidates
;; any package! declarations otherwise.
;; On the other hand, use-package! and after! break on any packages
;; that are neither recognized nor disabled.
;; My solution is to conditionally disable the package using this macro.
;; Calls must be after any local package! declarations.
(defmacro my-package-default-disable! (package)
  `(package! ,package :disable (not (doom-package-get ',package))))




(package! ligature
  :recipe (:host github :repo "mickeynp/ligature.el"))

;(package! straight :pin "3eca39d")
;(package! transient :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440")
;(package! with-editor :pin "391e76a256aeec6b9e4cbd733088f30c677d965b")

(package! google-c-style)

(package! speedrect)

; Seems like a cool package to keep in mind for creating one-off keymaps
(package! hercules)

(package! eldoc-box)

(package! ef-themes)

(package! catppuccin-theme)

(package! batppuccin)

(package! tokyo-night)

(package! kaolin-themes)

(package! claude-code-ide
  :recipe (:host github :repo "manzaltu/claude-code-ide.el"))

(unless (featurep :system 'windows)
  (package! msgpack)
  (package! tramp-rpc :recipe (:host github :repo "ArthurHeymans/emacs-tramp-rpc" :files ("lisp/*.el"))))

;; Get a basic git commit experience even without magit
(unless (modulep! :tools magit)
  ;; Unfortunately, git-commit cannot be installed without the full magit,
  ;; but at least we don't have to load all of it.
  (package! magit)
  )

(package! benchmark-init)

(package! el-patch)

(package! mixed-pitch)


;; Enable these where using email.
(my-package-default-disable! ebdb)
(my-package-default-disable! gnu-alias)

;; Remove this once doom updates to latest org.
(package! org :pin "146e8a6bd5c0f6c2965456c0eb882c6d673b0465")
