;;; gnus.el -*- lexical-binding: t; -*-

;; Preferences for GNUS.
;; Do not put server configuration details in here;
;; that is for ~/.gnus.el.

(when (featurep :system 'windows)
  (setq nnheader-file-coding-system 'raw-text-dos))

(after! gnus
  (setq gnus-inhibit-startup-message t
        gnus-asynchronous t
        gnus-use-cache t
        gnus-use-header-prefetch t
        gnus-save-newsrc-file nil         ; .newsrc is only for other readers
        gnus-read-newsrc-file nil
        gnus-always-read-dribble-file t
        gnus-check-new-newsgroups nil
        gnus-agent-go-online t
        gnus-fetch-old-headers 'some
        gnus-show-threads t)

  (setq gnus-parameter-large-newsgroup-initial-alist '((".*" . 200)))

  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-most-recent-date
          (not gnus-thread-sort-by-number)))

  (setq gnus-gcc-mark-as-read t
        message-kill-buffer-on-exit t     ; close after sending
        mm-default-directory
        (file-name-as-directory
         (expand-file-name (or (getenv "XDG_DOWNLOAD_DIR") "Downloads") "~"))
        )

  (map! :map (gnus-mode-map gnus-group-mode-map gnus-summary-mode-map gnus-article-mode-map gnus-browse-mode-map)
        "y" #'which-key-show-top-level)

  (map! :localleader :map (gnus-mode-map gnus-group-mode-map gnus-summary-mode-map gnus-article-mode-map gnus-browse-mode-map)
        "h" #'which-key-show-top-level
        "TAB" #'gnus-info-find-node
        "<tab>" #'gnus-info-find-node)

  ;; From https://gist.github.com/BonfaceKilz/726a1f2c06f15372c2df91fd64895bf9
  (defun my-gnus-group-list-subscribed-groups ()
    "List all subscribed groups with or without un-read messages"
    (interactive)
    (gnus-group-list-all-groups 5))

  (map! :map gnus-group-mode-map
        ;; list all the subscribed groups even they contain zero un-read messages
        "o" #'my-gnus-group-list-subscribed-groups)

  ;; summary
  (setq gnus-sum-thread-tree-false-root      "┬▶"
        gnus-sum-thread-tree-indent          "  "
        gnus-sum-thread-tree-root            "┌ "
        gnus-sum-thread-tree-single-indent "◎ "
        gnus-sum-thread-tree-vertical        "|"
        gnus-sum-thread-tree-leaf-with-other "├─► "
        gnus-sum-thread-tree-single-leaf     "╰─► "
        gnus-thread-hide-subtree nil

        gnus-user-date-format-alist '(((gnus-seconds-today) . "Today %H:%M")
                                    ((+ 86400 (gnus-seconds-today)) . "Yday %H:%M")
                                    (604800 . "%a %H:%M")
                                    (t . "%Y-%m-%d"))

        ;; │06-Jan│  Sender Name  │ Email Subject
        gnus-summary-line-format (concat "%0{%U%R%z%}"
                                         "%3{│%}" "%1{%d%}" "%3{│%}"
                                         "  "
                                         "%4{%-20,20f%}"
                                         "  "
                                         "%3{│%}"
                                         " "
                                         "%1{%B%}"
                                         "%s\n"))

  (setq mm-inline-large-images 'resize)

  ;; Created by Claude based on other doom email modules

    ;; Group buffer: topics are the nearest thing to mu4e's maildir/account tree
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  (setq gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)\n")

  ;; Popup rules / real buffers
  (set-popup-rule! "^\\*\\(Group\\|Summary\\|Article\\)" :ignore t)
  (set-popup-rule! "^\\*Gnus \\(log\\|Server\\)" :select nil)
  (add-hook 'gnus-group-mode-hook #'doom-mark-buffer-as-real-h)

  ;; Line numbers/modeline add nothing to read-only views
  (add-hook! '(gnus-group-mode-hook gnus-summary-mode-hook gnus-article-mode-hook)
             #'doom-disable-line-numbers-h)
  (setq-hook! 'gnus-article-mode-hook truncate-lines nil) ; wrap messages

  ;; HTML mail is sometimes better in a browser
  (map! :map gnus-article-mode-map
        :desc "View HTML in browser" "W" #'gnus-article-browse-html-article)

  (map! :localleader
      :map (gnus-group-mode-map gnus-summary-mode-map gnus-article-mode-map)
      :desc "Compose email" "c" #'gnus-msg-mail
      :desc "Fetch new"     "u" #'gnus-group-get-new-news
      :desc "Quit gnus"     "q" #'gnus-group-exit)
  )

(defun my-dashboard/gnus (&optional arg)
  "Wrapper around gnus compatible with doom dashboard"
  (interactive "P")
  (gnus))

(when (boundp '+dashboard-menu-sections)
  (insert-into-list +dashboard-menu-sections 4
                    '("Open email/news"
                      :icon (nerd-icons-mdicon "nf-md-email_newsletter" :face '+dashboard-menu-title)
                      :when (file-exists-p "~/.gnus.el")
                      :action =gnus)))

(after! message
  (defun my-message-mode-setup ()
    (setq fill-column 72)
    (turn-on-auto-fill))

  (add-hook 'message-mode-hook 'my-message-mode-setup)

  (map! :localleader
        :map message-mode-map
        :desc "send and exit" "s" #'message-send-and-exit
        :desc "kill buffer"   "d" #'message-kill-buffer
        :desc "save draft"    "S" #'message-dont-send
        :desc "attach"        "a" #'mml-attach-file)
)

(use-package! ebdb
  :after-call (gnus-before-startup-hook message-mode-hook)
  :config

  (require 'ebdb-gnus)
  (require 'ebdb-message)

  (setq ebdb-message-auto-update-p
      (lambda ()
        (unless (ebdb-mua-message-header "Newsgroups")
          'create)))

  (setq ebdb-complete-mail 'capf)
  (setq ebdb-mua-pop-up nil)
  (setq ebdb-save-on-exit t)
  )


;; Created by Claude

;; Launcher, analogous to =mu4e / =notmuch
(defun =gnus ()
  "Start (or switch to) Gnus in the current window."
  (interactive)
  (if (and (fboundp 'gnus-alive-p) (gnus-alive-p))
      (switch-to-buffer gnus-group-buffer)
    (gnus)))

;; Leader keys, mirroring the mu4e ones in all-bindings.el (SPC o m, SPC M ...)
;; since mu4e isn't enabled the "M" prefix is free.
(unless (or (modulep! :email mu4e) (modulep! :email notmuch)
            (modulep! :email wanderlust))
  (map! :leader
        (:prefix "o"
         :desc "gnus" "m" #'=gnus)
        (:prefix-map ("M" . "gnus")
         :desc "Open email app" "M" #'=gnus
         :desc "Compose email"  "c" #'gnus-msg-mail
         :desc "Fetch new mail" "u" #'gnus-group-get-new-news
         :desc "Quit gnus"      "q" #'gnus-group-exit)))


(defvar-local my-gnus-configuring-posting-styles nil)
(defun my-gnus-mark-configuring-posting-styles (&optional group)
  (setq my-gnus-configuring-posting-styles group))

(defun my-gnus-match-group (group)
  (if (stringp my-gnus-configuring-posting-styles)
      (string-match group my-gnus-configuring-posting-styles)
  (and (boundp 'gnus-newsgroup-name) (stringp gnus-newsgroup-name) (string-match group gnus-newsgroup-name))))

(use-package! gnus-alias
  :hook (message-setup . my-gnus-alias-determine-identity-if-configured)
  :init

  (defun my-gnus-alias-determine-identity-if-configured ()
    (when (and (bound-and-true-p gnus-alias-identity-alist) (bound-and-true-p my-gnus-alias-identity-rules))
      (require 'gnus-alias)
      (my-gnus-alias-determine-identity)
      (setq my-gnus-configuring-posting-styles nil)
      )
    )

  (advice-add 'gnus-configure-posting-styles :before #'my-gnus-mark-configuring-posting-styles)

  :config

  (map! :localleader
        :map message-mode-map
        :desc "select identity" "f" #'gnus-alias-select-identity)

  (defcustom my-gnus-alias-identity-rules nil
    "Rules used to choose Identity whenever a new message is created.

My version of gnus-alias-identity-rules, to avoid conflict
with the original.
In addition to the below:

o IDENTITY can be a function, passing in matches from the regexp as arguments.
o There is an additional rule method, in which you provide just a string,
  and this represents a regexp for a gnus group name.
  In theory, this allows fully replacing gnus-posting-styles.
  Has no effect without gnus.

Original:

The rules are evaluated to determine which Identity to use when
creating a new message (new email or article, a reply to an email or
article or follow-up to an article or a forwarded email or article).
Rules are individually evaluated by `gnus-alias-determine-identity' in
the order that they are defined in, until one of them results in an
Identity being chosen.  If this results in an invalid Identity being
chosen, what happens next is determined by
`gnus-alias-unknown-identity-rule' \(which see). If no valid Identites
are found,`gnus-alias-default-identity' is used instead.

Each rule has a NAME/DESCRIPTION, which is used mainly as
documentation for yourself, and is referenced when debugging is turned
on.  Then, one of two RULE METHODs are used:

o Header Match - matches a REGEXP (regular expression) to the value in
  the header field identified by HEADER SYMBOL (which is either a key
  into a list of headers defined by `gnus-alias-lookup-abbrev-alist',
  or the name of an actual header).  When replying to an email or
  following up to a post, both the original set of headers and the new
  message's headers are available to be matched against (with a new
  message, only the current set is available).  HEADER SET determines
  which set of headers are matched against.  Options are: current,
  previous or both (previous, and if that's empty, current).

o Function - this method is simply a function that returns either nil
  or non-nil.  Non-nil indicates that a match of whatever kind was
  achieved, and the specified Identity should be used.  This function
  can literally be anything you want from Message's `message-news-p'
  to a custom function (e.g. `my-identity-fn').

If the regexp matches or the function returns non-nil, the Identity
specified by IDENTITY is validated.  This can either be a key from
`gnus-alias-identity-alist' or a substitution scheme that results in
such a key (Header Match only).  Substitution is done just like in
normal regular expression processing, using \\\\D (where D is a number
corresponding to a sub-expression from the last match).

For example, given the following rule:

  NAME: Domain Match
  RULE METHOD: Header Match
    HEADER SYMBOL: to
    REGEXP: <\\\\(.+\\\\)@northbound-train.com
    HEADER SET: both
  IDENTITY: nt-\\\\1

Matching on \"CC: <emacs@northbound-train.com>\" would result in
the \"nt-emacs\" Identity being used.

See the Regular Expressions info node for more details on regexp
patterns and substitution:

    M-: (Info-goto-node \"(emacs)regexps\") RET"
    :type '(repeat
            (list :format "%v"
                  (string :tag "Name/Description")
                  (choice :tag "Rule Method"
                          (string :tag "Newsgroup Match")
                          (list :tag "Header Match"
                                (string :tag "Header Symbol")
                                (regexp :tag "Regexp")
                                (choice :tag "Header set"
                                        (const :tag "Current Headers only" current)
                                        (const :tag "Previous Headers only" previous)
                                        (const :tag "Previous Headers, then Current" both)))
                          (function :tag "Function"))
                  (choice :tag "Identity"
                          (string :tag "Identity Name")
                          (function :tag "Function"))
                  ))
    :group 'gnus-alias)

  ;; Lift out from gnus-alias-determine-identity for independent usage.
  (defun my-gnus-alias-match-header (rule &optional potential-identity)
    (let (class regexp which-headers
                header header-list header-value identity)

      (setq class (nth 0 rule)
            regexp (nth 1 rule)
            which-headers (nth 2 rule))

      ;; lookup header class
      (setq header-list (gnus-alias-resolve-alist-abbrev class))
      (when (not header-list)
        ;; assume that it itself is a header
        (setq header-list (list class)))

      ;; loop thru all header pieces
      (while (and (not identity) header-list)
        (setq header (car header-list))

        ;; which headers wanted?
        (cond
         ;; .........................
         ;; PREVIOUS
         ((equal which-headers 'previous)
          (setq header-value (message-fetch-reply-field header)))

         ;; .........................
         ;; BOTH
         ((equal which-headers 'both)
          (setq header-value (message-fetch-reply-field header))
          (when (not header-value)
            (setq header-value (message-fetch-field header))))

         ;; .........................
         ;; CURRENT (default)
         (t (setq header-value (message-fetch-field header)))
         )

        ;; does it match??
        (save-match-data
          (when (and header-value
                     (string-match regexp header-value))
            (cond
             ((stringp potential-identity)
              ;; check for & process substitutions
              (let ((orig-match-data (match-data))
                    potential-match-data match-num newtext)
                (while (string-match "\\\\\\([0-9]\\)" potential-identity)
                  (setq potential-match-data (match-data))
                  (setq match-num (string-to-number (match-string 1 potential-identity)))

                  ;; could use better (any) fault tolerance here
                  ;; (w/r/t looking up a match that doesn't exit)
                  (set-match-data orig-match-data)
                  (setq newtext (match-string match-num header-value))

                  (set-match-data potential-match-data)
                  (setq potential-identity
                        (replace-match newtext nil nil
                                       potential-identity))
                  ))
              ;; we got the real deal, now
              (setq identity potential-identity)
              )
             ((functionp potential-identity)
              (let ((num-matches (cdr (func-arity potential-identity)))
                    (match-num 1)
                    potential-match-data args)
                (while (<= match-num num-matches)
                  ;; could use better (any) fault tolerance here
                  ;; (w/r/t looking up a match that doesn't exit)
                  (add-to-list 'args (match-string match-num header-value) t)
                  (incf match-num))

                (setq identity (apply potential-identity args)))
              )
             (t (setq identity t)))
            ))

        (setq header-list (cdr header-list))
        )

      identity
      )
    )

  ;; Expands gnus-alias with additional functionality.
  (defun my-gnus-alias-determine-identity (&optional lookup-only)
    "Function that chooses a Identity based on message headers.

My version of gnus-alias-determine-identity.
Uses my-gnus-alias-identity-rules, to avoid conflict.

See `my-gnus-alias-identity-rules' for more information.  Optional
LOOKUP-ONLY is a boolean that, when non-nil, says to determine the
Identity, but don't actually use it (just return it)"

    (let ((rules-list my-gnus-alias-identity-rules)
          (message-reply-buffer (or gnus-alias-reply-buffer
                                    message-reply-buffer))
          current-choice first-elem class regexp which-headers
          header header-list header-value potential-identity identity
          rule-name)

      ;; debugging
      (gnus-alias-dump-headers "[GADI] ")

      ;; loop thru all electric headers until one matches
      (while (and (not identity) rules-list)
        ;; get next potential
        (setq current-choice (car rules-list))
        (setq rule-name (car current-choice))
        (setq current-choice (cdr current-choice))
        (gnus-alias-debug 2 "[GADI] Evaluating <%s>\n" rule-name)

        ;; get first elem
        (setq first-elem (car current-choice))

        ;; what is it, list or function?
        (cond
         ;; .........................
         ;; a function
         ((functionp first-elem)
          ;; call function; if it returns non-nil, use the identity
          (when (funcall first-elem)
            (setq identity (cadr current-choice))
            (when (functionp identity)
              (setq identity (funcall identity))
              )
            ))

         ;; .........................
         ;; a string - a gnus group
         ((stringp first-elem)
          (when (my-gnus-match-group first-elem)
            (setq identity (cadr current-choice))
            (when (functionp identity)
              (setq identity (funcall identity))
              )
            ))

         ;; .........................
         ;; a list - class regexp orig-headers
         ((listp first-elem)
          (setq identity (my-gnus-alias-match-header first-elem (cadr current-choice))))

         ;; .........................
         ;; unknown - ignore
         (t))

        ;; if we found anything, find out if it's valid
        (when (and identity
                   (not (assoc-string
                         identity gnus-alias-identity-alist t)))

          (gnus-alias-debug 2 "[GADI] Unknown Identity found:\n")
          (gnus-alias-debug 2 "       Rule: <%s>\n" rule-name)
          (gnus-alias-debug 2 "       Identity: <%s>\n" identity)
          (gnus-alias-debug 2 "       Action: <%s>\n"
                            (if (symbolp gnus-alias-unknown-identity-rule)
                                (symbol-name gnus-alias-unknown-identity-rule)
                              gnus-alias-unknown-identity-rule))

          (cond
           ;; .........................
           ;; ERROR
           ((equal gnus-alias-unknown-identity-rule 'error)
            (gnus-alias-error "Unknown Identity: <%s>" identity))

           ;; .........................
           ;; DEFAULT
           ((equal gnus-alias-unknown-identity-rule 'default)
            (setq identity gnus-alias-default-identity))

           ;; .........................
           ;; IDENTITY
           ((stringp gnus-alias-unknown-identity-rule)
            (setq identity gnus-alias-unknown-identity-rule))

           ;; .........................
           ;; FUNCTION
           ((functionp gnus-alias-unknown-identity-rule)
            (setq identity (funcall gnus-alias-unknown-identity-rule
                                    identity)))

           ;; .........................
           ;; CONTINUE (assumed)
           (t (setq identity nil))
           ))

        ;; get next element
        (setq rules-list (cdr rules-list))
        )

      ;; if no identity selected, try default
      (when (and (not identity) gnus-alias-default-identity)
        (setq identity gnus-alias-default-identity))

      ;; use it (unless asked not to)
      (unless (or lookup-only (not identity))
        (gnus-alias-debug 1 "Using the <%s> Identity" identity)
        (gnus-alias-use-identity identity))

      ;; return it
      identity))

  ;; Slight modification to gnus-alias to handle empty headers.
  (defun my-gnus-alias-use-identity-1 (identity &optional suppress-error)
    "Use an Identity defined in `gnus-alias-identity-alist'.

IDENTITY must be a valid entry in `gnus-alias-identity-alist',
otherwise an error will occur (NOTE: this behavior has changed
significantly from that found in 'gnus-pers').

SUPPRESS-ERROR will cause the function to silently fail under the
above circumstances rather then generate an error."
    ;; lookup Identity
    (let ((ID (assoc-string identity gnus-alias-identity-alist t))
          reference from org extras body sig extras-list current-extra
          extra-hdr extra-val afters)
      ;; is IDENTITY valid?
      (if (not ID)
          (when (not suppress-error)
            (gnus-alias-error "Unknown Identity: <%s>" identity))

        ;; remove old identity unless we're overlaying
        (unless gnus-alias-overlay-identities
          (gnus-alias-remove-identity gnus-alias-current-identity)
          (setq gnus-alias-current-identity nil))

        ;; get ID elements
        (setq reference (gnus-alias-get-reference ID)
              from (gnus-alias-get-from ID)
              org (gnus-alias-get-org ID)
              extras (gnus-alias-get-extras ID)
              body (gnus-alias-get-body ID)
              sig (gnus-alias-get-sig ID))

        ;; lay down reference Identity maybe
        (when reference
          (gnus-alias-use-identity-1 (gnus-alias-get-value reference)))

        (save-restriction
          (goto-char (point-min))
          (save-match-data
            (when (re-search-forward "<#\\(mml\\|part\\)" nil t)
              (narrow-to-region (point-min) (match-beginning 0))))

          ;; add From maybe
          (when from
            (gnus-alias-remove-header "From")

            (message-position-on-field "From")
            (insert (gnus-alias-get-value from))

            (when gnus-alias-use-buttonized-from
              ;; do something with widgets here
              ;;        (gnus-alias-buttonize-from)
              ))

          ;; add Organization maybe
          (when org
            (gnus-alias-remove-header "Organization")

            (gnus-alias-position-on-field "Organization")
            (insert (gnus-alias-get-value org)))

          ;; add extra headers maybe
          (when extras
            (setq extras-list extras)
            (while extras-list
              (setq current-extra (car extras-list))
              (setq extra-hdr (gnus-alias-get-value (car current-extra)))
              (setq extra-val (gnus-alias-get-value (cdr current-extra)))

              (gnus-alias-remove-header extra-hdr)

              (when extra-val
                (gnus-alias-position-on-field extra-hdr)
                (insert extra-val))

              (setq extras-list (cdr extras-list))
              ))

          ;; add body maybe
          (when body
            (gnus-alias-remove-current-body)
            (gnus-alias-goto-sig)
            (insert (gnus-alias-get-value body))
            (unless (bolp) (insert "\n")))

          ;; remove old signature
          (gnus-alias-remove-sig)

          ;; add signature maybe
          (when sig
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "-- \n")
            (insert (gnus-alias-get-value sig))))

        ;; remember last Identity used
        (setq gnus-alias-current-identity identity)))

    ;; position at start of message
    (message-goto-body))

  (advice-add 'gnus-alias-use-identity-1 :override #'my-gnus-alias-use-identity-1)
  )
