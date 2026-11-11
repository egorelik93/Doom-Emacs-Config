;;; user-lisp/use-theme.el -*- lexical-binding: t; -*-

(defvar use-theme-enable-theme-alist nil)
(defun use-theme-enable-theme-h (theme)
  (dolist (f (alist-get theme use-theme-enable-theme-alist))
    (funcall f)))
(add-hook 'enable-theme-functions #'use-theme-enable-theme-h -50)

(defun use-theme-eval-after-enable (theme form)
  (push (if (functionp form) form (lambda () (eval form))) (alist-get theme use-theme-enable-theme-alist))
  )

(defvar use-theme-disable-theme-alist nil)
(defun use-theme-disable-theme-h (theme)
  (dolist (f (alist-get theme use-theme-disable-theme-alist))
    (funcall f)))
(add-hook 'disable-theme-functions #'use-theme-disable-theme-h -50)

(defun use-theme-eval-after-disable (theme form)
  (message "%S" form)
  (push (if (functionp form) form (lambda () (eval form))) (alist-get theme use-theme-disable-theme-alist))
  )


(defmacro use-theme-package! (package &rest args)
  "Adds detection of doom disabled packages to use-theme-package"
  (declare (indent 1))
  (unless (memq package doom-disabled-packages)
    `(use-theme-package ,package ,@args)))

(cl-defmacro use-theme-package (package
                                &key
                                (theme nil)
                                (theme-eval nil)
                                (preface nil)
                                (defer nil)
                                (init nil)
                                (setup nil)
                                (config nil)
                                (first-load nil)
                                (every-load nil)
                                (first-enable nil)
                                (every-enable nil)
                                (every-disable nil))
  "Single-file themes and use-package :defer do not play as expected together;
because themes are loaded, not required, and use-package defers using eval-after-load,
:config will get evaluated on every load, but only when deferred.

This macro wraps use-package in ways that make sense for themes that
use a single file or feature for both theme and underlying machinery,
leaving us no distinct :config to use.
For other themes, this is usable but consider the ordinary use-package.

:preface        A list of sexps.
                Evaluated at expansion time.
                Can be used to define functions for use by :defer.
:theme          Either a theme name, quoted list of theme names, quoted function, or an sexp.
                Must be provided if the theme name differs from package.
                If an sexp starting with :eval, it will evaluate the second item of the sexp immediately,
                expecting a theme name or quoted list of theme names as the result.
                Otherwise an sexp is interpreted as a test of the provided variable \"theme\".
:theme-eval     A form.
                Alternative to :theme, specifies an expression that will be evaluated at runtime
                to a theme name or list of theme names.
                The primary purpose is if the package itself provides a list of themes it provides;
                as such, it is evaluated after loading.
                Not recommended for long lists, however.
                This forces :first-load, so consider very carefully whether that is desirable.
                Often, it is better to just list out the themes manually.
:defer          Any boolean expression.
                Unlike use-package, the expression will be evaluated before getting passed
                to use-package.
                We suggest enabling this, or possibly conditioning it on
                whether the theme is unused if appropriate.
                Doom Emacs startup behavior seems to be that if the theme is already loaded,
                it will just enable it. Combined with :defer t, that can create
                slight differences in load behavior and timing.
:init           A list of sexps.
                Same behavior as use-package.
:setup          A list of sexps.
                Run once earliest of any :first-load and :first-enable, depending on what is given.
                Theme permitting, we recommend doing typical package config here.
                Setup that has effects on the rest of emacs, e.g. defuns and hooks, should definitely go here.
:config         A list of sexps.
                A synonym for :first-load, to match the look of use-package.
                For clarity of behavior, we recommend using :first-load instead for themes
                not designed for use with use-package.
                Do not use both.
:first-load     A list of sexps.
                As the name suggests, runs only the first time the package is loaded,
                which is usually the expected and desired behavior of :config.
                As with use-package, if not :defer this runs immediately,
                so this is a good location when that is desirable.
                If a package explicitly says to use :config, that goes here (possibly use the :config synonym).
                Otherwise superceded by :setup if one does not strictly need the :first-load timing.
:every-load     A list of sexps.
                Runs every time the package is loaded.
                Unless running at enable-time is insufficient, we recommend not using this.
:first-enable   A list of sexps.
                Runs only the first time the theme is enabled.
                May be a reasonable alternative to config if that is too early;
                from a use-package perspective it is always \"deferred\", and is a good location
                where that is desirable.
                Note that it is run only once per package, even if multiple themes are provided.
                The variable \"theme\" is available to determine which is being enabled.
:every-enable   A list of sexps.
                Runs every time the theme is enabled.
                We recommend setting any final face modifications here.
                The variable \"theme\" is available to determine which is being enabled.
:every-disable  A list of sexps.
                Runs every time the theme is disabled.
                The variable \"theme\" is available to determine which is being enabled.
"
  (declare (indent 1))
  (when preface
    (eval (macroexp-progn preface))
    )
  (when config
    (setq first-load config))
  (let* ((themes (cond
                  ((symbolp theme) (list theme))
                  ((listp theme)
                   (cond
                    ((eq (car theme) 'quote) (ensure-list (cadr theme)))
                    ((eq (car theme) 'function) nil)
                    ((eq (car theme) ':eval) (ensure-list (eval (cadr theme))))
                    (t nil)))
                  (theme-eval nil)
                  (t package)))
         (theme-eval (when theme-eval `(ensure-list ,theme-eval)))
         (theme-test (cond
                      (themes nil)
                      ((and (listp theme) (eq (car theme) 'function)) `(funcall ,theme theme))
                      (theme-eval `(memq theme ,theme-eval))
                      (t theme)))
         (defer (when defer (eval defer)))
         ;; If using theme-eval, we must configure hooks during :first-load.
         (configure-hooks-in-load (and theme-eval (or setup first-enable every-enable every-disable)))
         (setup-in-load (or (not defer) first-load every-load configure-hooks-in-load))
         configure-hooks theme-enabled-sym package-configured-sym
         )
    (if setup-in-load
        (setq first-load (or (append setup first-load) '(nil)))
      (setq first-enable (append setup first-enable))
      )

    (setq theme-enabled-sym (when first-enable (intern (concat "use-theme--" (symbol-name package) "--enabled"))))
    (setq package-configured-sym (when first-load (intern (concat "use-theme--" (symbol-name package) "--configured"))))
    (setq configure-hooks `(
                            ,@(when (or first-enable every-enable)
                                (let ((enable-h
                                       (append
                                        (when first-enable
                                          `((unless ,theme-enabled-sym
                                              (setq ,theme-enabled-sym t)
                                              ,@first-enable
                                              ))
                                          )
                                        every-enable
                                        )))
                                  (cond
                                   (theme-eval
                                    `((mapcar
                                       (lambda (theme)
                                         (use-theme-eval-after-enable theme `(let ((theme ',theme)) ,@',enable-h))
                                         )
                                       ,theme-eval
                                       ))
                                    )
                                   (theme-test
                                    `((add-hook 'enable-theme-functions
                                       (defun ,(intern (concat "use-theme--" (symbol-name package) "--enable-h")) (theme)
                                         (when ,theme-test
                                           ,@enable-h))
                                       -50))
                                    )
                                   (t
                                    (mapcar
                                     (lambda (theme)
                                       `(use-theme-eval-after-enable ',theme `(let ((theme ',',theme)) ,@',enable-h)))
                                     themes)
                                    )
                                   )))
                            ,@(when every-disable
                                (cond
                                 (theme-eval
                                  `((mapcar
                                       (lambda (theme)
                                         (use-theme-eval-after-disable theme `(let ((theme ',theme)) ,@',every-disable))
                                         )
                                       ,theme-eval
                                       ))
                                  )
                                (theme-test
                                 `((add-hook 'disable-theme-functions
                                    (defun ,(intern (concat "use-theme--" (symbol-name package) "--disable-h")) (theme)
                                      (when ,theme-test
                                        @,every-disable))
                                    -50))
                                 )
                                (t
                                 (mapcar
                                  (lambda (theme)
                                    `(use-theme-eval-after-disable ',theme `(let ((theme ',',theme)) ,@',every-disable)))
                                  themes)
                                 )
                                ))))
    (if setup-in-load
        (setq first-load (append first-load configure-hooks))
      (setq init (append init configure-hooks)))

    `(use-package ,package
       ,@(when defer `(:defer ,defer))
       ,@(when (or init first-load first-enable)
           (remq
            nil
            `(:init
              ,(when (and first-load defer) `(setq ,package-configured-sym nil))
              ,(when first-enable `(setq ,theme-enabled-sym nil))
              ,@init
              )))
       ,@(when (or first-load every-load)
           (remq
            nil
            (if defer
                `(:config
                  ,(when first-load
                     `(unless ,package-configured-sym
                        (setq ,package-configured-sym t)
                        ,@first-load
                        ))
                  ,@every-load)
              `(:config
                ,@first-load
                ,@every-load
                ,(when every-load
                   `(eval-after-load ,package ,every-load)
                   )
                ))))
       )))

(provide 'use-theme)
