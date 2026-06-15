;;; no-evil-windows.el -*- lexical-binding: t; -*-

; Copied from evil-commands.el

;;; Window navigation

(defvar evil-window-map (make-sparse-keymap)
  "Keymap for window-related commands.")

(defcustom evil-auto-balance-windows t
  "If non-nil window creation and deletion trigger rebalancing."
  :type 'boolean
  :group 'evil)

(defcustom evil-split-window-below nil
  "If non-nil split windows are created below."
  :type 'boolean
  :group 'evil)

(defcustom evil-vsplit-window-right nil
  "If non-nil vertically split windows with are created to the right."
  :type 'boolean
  :group 'evil)

(defvar evil--window-digit nil)

(defun evil--window-keep-pred ()
  (eq 'evil-window-digit-argument this-command))

(defun evil--window-reset-digit ()
  (setq evil--window-digit nil)
  (remove-hook 'post-command-hook #'evil--window-reset-digit))

(defun evil--window-on-exit ()
  (add-hook 'post-command-hook #'evil--window-reset-digit))

(defun evil-window-digit-argument ()
  "Like `digit-argument' but maintains the window map."
  (interactive)
  (unless (eq 'evil-window-digit-argument last-command)
    (set-transient-map evil-window-map #'evil--window-keep-pred #'evil--window-on-exit))
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0)))
    (setq evil--window-digit (+ (* 10 (or evil--window-digit 0)) digit))))

(defmacro evil-save-side-windows (&rest body)
  "Toggle side windows, evaluate BODY, restore side windows."
  (declare (indent defun) (debug (&rest form)))
  (let ((sides (make-symbol "sidesvar")))
    `(let ((,sides (and (fboundp 'window-toggle-side-windows)
                        (window-with-parameter 'window-side))))
       ;; The compiler doesn't understand that all uses are protected
       ;; by `fboundp' :-(
       (declare-function window-toggle-side-windows "window")
       (when ,sides
         (window-toggle-side-windows))
       (unwind-protect
           (progn ,@body)
         (when ,sides
           (window-toggle-side-windows))))))

(defun evil-resize-window (new-size &optional horizontal)
  "Set the current window's width or height to NEW-SIZE.
If HORIZONTAL is non-nil the width of the window is changed,
otherwise its height is changed."
  (let ((count (- new-size (if horizontal (window-width) (window-height)))))
    (enlarge-window count horizontal)))

(defun evil-move-window (side)
  "Move the `selected-window' to SIDE.
The state of the `selected-window' is saved along with the state
of the window tree consisting of all the other windows. Then, all
windows are deleted, the remaining window is split according to
SIDE, the state of the window at SIDE is replaced with the saved
state of the `selected-window', and, finally, the state of the
saved window tree is reconstructed on the opposite side.

SIDE has the same meaning as in `split-window'.

Note, this function only operates on the window tree rooted in
the frame's main window and effectively preserves any side
windows (i.e. windows with a valid window-side window
parameter)."
  (evil-save-side-windows
    (unless (one-window-p)
      (save-excursion
        (let ((w (window-state-get (selected-window))))
          (delete-window)
          (let ((wtree (window-state-get)))
            (delete-other-windows)
            (let ((subwin (selected-window))
                  ;; NOTE: SIDE is new in Emacs 24
                  (newwin (split-window nil nil side)))
              (window-state-put wtree subwin)
              (window-state-put w newwin)
              (select-window newwin)))))
      (balance-windows))))

(defun evil-alternate-buffer (&optional window)
  "Return the last buffer WINDOW has displayed other than the current one.
This is equivalent to Vim's alternate buffer."
  ;; If the last buffer visited has been killed, then `window-prev-buffers'
  ;; returns a list with `window-buffer' at the head.
  (let* ((prev-buffers (window-prev-buffers))
         (head (car prev-buffers)))
    (if (eq (car head) (window-buffer window))
        (cadr prev-buffers)
      head)))

(defun evil-switch-to-windows-last-buffer ()
  "Switch to the last open buffer of the current window."
  (interactive)
  (let ((previous-place (evil-alternate-buffer)))
    (when previous-place
      (switch-to-buffer (car previous-place)))))

(defun evil-window-delete ()
  "Delete the current window or tab.
If `evil-auto-balance-windows' is non-nil then all children of
the deleted window's parent window are rebalanced."
  (interactive)
  (let ((p (window-parent)))
    ;; If tabs are enabled and this is the only visible window, then attempt to
    ;; close this tab.
    (if (and (bound-and-true-p tab-bar-mode)
             (null p))
        (tab-close)
      (delete-window)
      (when evil-auto-balance-windows
        ;; balance-windows raises an error if the parent does not have
        ;; any further children (then rebalancing is not necessary anyway)
        (ignore-errors (balance-windows p))))))

(defun evil-window-split (&optional count file read-only)
  "Split the current window horizontally, COUNT lines height,
editing a certain FILE. The new window will be created below
when `evil-split-window-below' is non-nil. If COUNT and
`evil-auto-balance-windows' are both non-nil then all children
of the parent of the splitted window are rebalanced."
  (interactive
   (list (and current-prefix-arg
              (prefix-numeric-value
               current-prefix-arg))
         nil))
  (select-window
   (split-window (selected-window) (when count (- count))
                 (if evil-split-window-below 'below 'above)))
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (when file
    (funcall (if read-only #'evil-view #'evil-edit) file)))

(defun evil-window-split-view (&optional count file)
  "As with `evil-window-split' but the file is opened read-only."
  (interactive
   (list (and current-prefix-arg
              (prefix-numeric-value
               current-prefix-arg))
         nil))
  (evil-window-split count file t))

(defun evil-window-vsplit (&optional count file)
  "Split the current window vertically, COUNT columns width,
editing a certain FILE. The new window will be created to the
right when `evil-vsplit-window-right' is non-nil. If COUNT and
`evil-auto-balance-windows'are both non-nil then all children
of the parent of the splitted window are rebalanced."
  (interactive
   (list (and current-prefix-arg
              (prefix-numeric-value
               current-prefix-arg))
         nil))
  (select-window
   (split-window (selected-window) (when count (- count))
                 (if evil-vsplit-window-right 'right 'left)))
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (when file
    (evil-edit file)))

(defun evil-split-buffer (buffer)
  "Split window and switch to another buffer."
  (evil-window-split)
  (evil-buffer buffer))

(defun evil-split-next-buffer (&optional count)
  "Split the window and go to the COUNT-th next buffer in the buffer list."
  (interactive "p")
  (evil-window-split)
  (next-buffer count))

(defun evil-split-prev-buffer (&optional count)
  "Split window and go to the COUNT-th prev buffer in the buffer list."
  (interactive "p")
  (evil-window-split)
  (previous-buffer count))

(defun evil-window-left (count)
  "Move the cursor to new COUNT-th window left of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-left)))

(defun evil-window-right (count)
  "Move the cursor to new COUNT-th window right of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-right)))

(defun evil-window-up (count)
  "Move the cursor to new COUNT-th window above the current one."
  (interactive "p")
  (dotimes (_ (or count 1))
    (windmove-up)))

(defun evil-window-down (count)
  "Move the cursor to new COUNT-th window below the current one."
  (interactive "p")
  (dotimes (_ (or count 1))
    (windmove-down)))

(defun evil-window-bottom-right ()
  "Move the cursor to bottom-right window."
  (interactive)
  (let ((last-sibling (frame-root-window)))
    (while (and last-sibling (not (window-live-p last-sibling)))
      (setq last-sibling (window-last-child last-sibling)))
    (when last-sibling
      (select-window last-sibling))))

(defun evil-window-top-left ()
  "Move the cursor to top-left window."
  (interactive)
  (let ((first-child (window-child (frame-root-window))))
    (while (and first-child (not (window-live-p first-child)))
      (setq first-child (window-child first-child)))
    (when first-child
      (select-window
       first-child))))

(defun evil-window-mru ()
  "Move the cursor to the previous (last accessed) buffer in another window.
More precisely, it selects the most recently used buffer that is
shown in some other window, preferably of the current frame, and
is different from the current one."
  (interactive)
  (catch 'done
    (dolist (buf (buffer-list (selected-frame)))
      (let ((win (get-buffer-window buf)))
        (when (and (not (eq buf (current-buffer)))
                   win
                   (not (eq win (selected-window))))
          (select-window win)
          (throw 'done nil))))))

(defun evil-window-next (count)
  "Move the cursor to the next window in the cyclic order.
With COUNT go to the count-th window in the order starting from
top-left."
  (interactive (list (and
                      current-prefix-arg
                      (prefix-numeric-value
                       current-prefix-arg))))
  (if (not count)
      (other-window +1)
    (evil-window-top-left)
    (other-window (1- (min count (length (window-list)))))))

(defun evil-window-prev (count)
  "Move the cursor to the previous window in the cyclic order.
With COUNT go to the count-th window in the order starting from
top-left."
  (interactive (list (and
                      current-prefix-arg
                      (prefix-numeric-value
                       current-prefix-arg))))
  (if (not count)
      (other-window -1)
    (evil-window-top-left)
    (other-window (1- (min count (length (window-list)))))))

(defun evil-window-new (count file)
  "Split the current window horizontally
and open a new buffer or edit a certain FILE."
  (interactive
   (list (and current-prefix-arg
              (prefix-numeric-value
               current-prefix-arg))
         nil))
  (let ((new-window (split-window (selected-window) (when count (- count))
                                  (if evil-split-window-below 'below 'above))))
    (when (and (not count) evil-auto-balance-windows)
      (balance-windows (window-parent)))
    (select-window new-window)
    (evil-buffer-new file)))

(defun evil-window-vnew (count file)
  "Split the current window vertically
and open a new buffer name or edit a certain FILE."
  (interactive
   (list (and current-prefix-arg
              (prefix-numeric-value
               current-prefix-arg))
         nil))
  (let ((new-window (split-window (selected-window) (when count (- count))
                                  (if evil-vsplit-window-right 'right 'left))))
    (when (and (not count) evil-auto-balance-windows)
      (balance-windows (window-parent)))
    (select-window new-window)
    (evil-buffer-new file)))

(defun evil-buffer-new (&optional file)
  "Edit a new unnamed buffer or FILE."
  (interactive
   (let ((prefix current-prefix-arg))
     (list (when (numberp prefix)
             (prefix-numeric-value prefix))
           (when (consp prefix)
             (read-file-name "File: " nil nil
                             t)))))
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-buffer-major-mode buffer)
      (set-window-buffer nil buffer))))

(defun evil-window-increase-height (count)
  "Increase current window height by COUNT."
  (interactive "p")
  (enlarge-window count))

(defun evil-window-decrease-height (count)
  "Decrease current window height by COUNT."
  (interactive "p")
  (enlarge-window (- count)))

(defun evil-window-increase-width (count)
  "Increase current window width by COUNT."
  (interactive "p")
  (enlarge-window count t))

(defun evil-window-decrease-width (count)
  "Decrease current window width by COUNT."
  (interactive "p")
  (enlarge-window (- count) t))

(defun evil-window-set-height (count)
  "Set the height of the current window to COUNT."
  (interactive (list (and
                      current-prefix-arg
                      (prefix-numeric-value
                       current-prefix-arg))))
  (evil-resize-window (or count (frame-height)) nil))

(defun evil-window-set-width (count)
  "Set the width of the current window to COUNT."
  (interactive (list (and
                      current-prefix-arg
                      (prefix-numeric-value
                       current-prefix-arg))))
  (evil-resize-window (or count (frame-width)) t))

(defun evil-ex-resize (arg)
  "The ex :resize command.

If ARG is a signed positive integer, increase the current window
height by ARG.

If ARG is a signed negative integer, decrease the current window
height by ARG.

If ARG is a positive integer without explicit sign, set the current
window height to ARG.

If ARG is empty, maximize the current window height."
  (interactive "sArg: ")
  (if (or (not arg) (= 0 (length arg)))
      (evil-window-set-height nil)
    (let ((n (string-to-number arg)))
      (if (> n 0)
          (if (= ?+ (aref arg 0))
              (evil-window-increase-height n)
            (evil-window-set-height n))
        (evil-window-decrease-height (- n))))))

(defun evil-window-rotate-upwards ()
  "Rotate the windows according to the current cyclic ordering."
  (interactive)
  (evil-save-side-windows
    (let ((wlist (window-list))
          (slist (mapcar #'window-state-get (window-list))))
      (setq slist (append (cdr slist) (list (car slist))))
      (while (and wlist slist)
        (window-state-put (car slist) (car wlist))
        (setq wlist (cdr wlist)
              slist (cdr slist)))
      (select-window (car (window-list))))))

(defun evil-window-rotate-downwards ()
  "Rotate the windows according to the current cyclic ordering."
  (interactive)
  (evil-save-side-windows
    (let ((wlist (window-list))
          (slist (mapcar #'window-state-get (window-list))))
      (setq slist (append (last slist) slist))
      (while (and wlist slist)
        (window-state-put (car slist) (car wlist))
        (setq wlist (cdr wlist)
              slist (cdr slist)))
      (select-window (car (window-list))))))

(defun evil-window-exchange (&optional count)
  "Exchange the current window with the next, or the COUNT-th, one."
  (interactive (list (and
                      current-prefix-arg
                      (prefix-numeric-value
                       current-prefix-arg))))
  (let ((original-window (selected-window)))
    (evil-window-next count)
    (if (fboundp 'window-swap-states)
        (window-swap-states nil original-window t)
      (let* ((other-window (selected-window))
             (original-state (window-state-get original-window))
             (other-state (window-state-get other-window)))
        (window-state-put other-state original-window t)
        (window-state-put original-state other-window t)))))

(defun evil-window-move-very-top ()
  "Close the current window, split the upper-left one horizontally
and redisplay the current buffer there."
  (interactive)
  (evil-move-window 'above))

(defun evil-window-move-far-left ()
  "Close the current window, split the upper-left one vertically
and redisplay the current buffer there."
  (interactive)
  (evil-move-window 'left))

(defun evil-window-move-far-right ()
  "Close the current window, split the lower-right one vertically
and redisplay the current buffer there."
  (interactive)
  (evil-move-window 'right))

(defun evil-window-move-very-bottom ()
  "Close the current window, split the lower-right one horizontally
and redisplay the current buffer there."
  (evil-move-window 'below))

;;; Tab commands

(defun evil-tab-next (arg)
  "Switch to the next tab.
If ARG is non-nil, parse ARG as an index and go to the tab at that
index."
    (interactive (list (and
                        current-prefix-arg
                        (prefix-numeric-value
                         current-prefix-arg))))
  (if arg
      (tab-bar-select-tab arg)
    (tab-bar-switch-to-next-tab)))
