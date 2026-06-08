;;; checkers/jinx/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +spell/correct ()
  "Correct spelling of word at point."
  (interactive)
  (call-interactively #'jinx-correct))

;;;###autoload
(defun +spell/add-word (word &optional scope)
  "Add WORD to your personal dictionary, within SCOPE.

SCOPE can be `buffer' or `session' to exclude words only from the current
buffer or session. Otherwise, the addition is permanent.

Interactively, C-u uses buffer scope, C-u C-u uses session scope."
  (interactive
   (list (thing-at-point 'word t)
         (cond ((equal current-prefix-arg '(16)) 'session)
               ((equal current-prefix-arg '(4))  'buffer))))
  (unless word (user-error "No word at point"))
  (pcase scope
    ('session
     (add-to-list 'jinx--session-words word)
     (jinx--recheck-overlays))
    ('buffer
     ;; jinx--add-local-word sets the buffer-local word list and also adds to
     ;; jinx--session-words so the word is ignored immediately.
     (jinx--add-local-word 'jinx-local-words word))
    (_
     (if (and (boundp 'jinx--dicts) jinx--dicts)
         (progn
           (jinx--mod-add (car jinx--dicts) word)
           (jinx--recheck-overlays))
       (user-error "Jinx is not active in this buffer")))))

;;;###autoload
(defun +spell/remove-word (_word &optional _scope)
  "Remove WORD from your personal dictionary."
  (interactive)
  (user-error "Word removal is not supported with jinx"))

;;;###autoload
(defun +spell/next-error ()
  "Jump to next spelling error."
  (interactive)
  (jinx-next 1))

;;;###autoload
(defun +spell/previous-error ()
  "Jump to previous spelling error."
  (interactive)
  (jinx-previous 1))
