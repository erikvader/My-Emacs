;;; dwm.el --- tiled window manager for emacs        -*- lexical-binding: t; -*-

;; Copyright (C) 2016  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defgroup dwm-mode nil
  "Tiled window manager"
  :prefix "dwm-"
  :group 'convenience)

(defcustom dwm-ignore-buffers-regexp '("helm" "ivy")
  "Ignore buffers name regexp")

(defun dwm-window-edges-alist ()
  "returns a list with each window and each windows edge-distances"
  (mapcar #'(lambda (win) (cons (window-edges win) win))
          (window-list-1)))

(defun dwm-win-left-pos (win-edge)
  (car win-edge))

(defun dwm-win-top-pos (win-edge)
  (cadr win-edge))

(defun dwm-win-right-pos (win-edge)
  (caddr win-edge))

(defun dwm-win-bottom-pos (win-edge)
  (cadddr win-edge))

(cl-defun dwm-find-window (&key left-pos top-pos right-pos bottom-pos)
  "finds and returns the first window that satisfies the position given in the aguments"
  (cdr
   (cl-find-if
    #'(lambda (edge-win)
        (let ((edge (car edge-win)))
          (and (if left-pos (eq left-pos (dwm-win-left-pos edge)) t)
               (if top-pos (eq top-pos (dwm-win-top-pos edge)) t)
               (if right-pos (eq right-pos (dwm-win-right-pos edge)) t)
               (if bottom-pos (eq bottom-pos (dwm-win-bottom-pos edge)) t))))
    (dwm-window-edges-alist))))

(defun dwm-find-next-window (cur-win)
  (let ((main-win (dwm-main-window)))
    (if (equal cur-win main-win)
        (dwm-first-sub-window)
      (let ((next-win
             (cdr (cl-find-if #'(lambda (edge-win)
                                  (< (dwm-win-top-pos (window-edges cur-win))
                                     (dwm-win-top-pos (car edge-win))))
                              (dwm-sub-windows)))))
        (if next-win
            next-win
          main-win)))))

(defun dwm-find-prev-window (cur-win)
  (let ((main-win (dwm-main-window)))
    (if (equal cur-win main-win)
        (dwm-last-sub-window)
      (let ((prev-win
             (cdr (cl-find-if #'(lambda (edge-win)
                                  (> (dwm-win-bottom-pos
                                      (window-edges cur-win))
                                     (dwm-win-bottom-pos (car edge-win))))
                              (reverse (dwm-sub-windows))))))
        (if prev-win
            prev-win
          main-win)))))

(defun dwm-main-window ()
  (dwm-find-window :left-pos 0 :top-pos 0))

(defun dwm-sub-windows (&optional windows-only)
  "returns all sub-windows in edge-win pairs. windows-only makes this
return just the windows"
  (let ((edge-win (cl-sort
                   (remove-if #'(lambda (edge-win) (equal (cdr edge-win) (dwm-main-window)))
                              (dwm-window-edges-alist))
                   #'< :key #'(lambda (edge-win) (dwm-win-top-pos (car edge-win))))))
    (if windows-only
        (mapcar #'cdr edge-win)
      edge-win)))

(defun dwm-first-sub-window ()
  (let ((main-win-edges (window-edges (dwm-main-window))))
    (dwm-find-window :left-pos (dwm-win-right-pos main-win-edges)
                     :top-pos (dwm-win-top-pos main-win-edges))))

(defun dwm-last-sub-window ()
  (cdr (car (last (dwm-sub-windows)))))

(defun dwm-create-sub-buffer (buffer)
  "creates the initial sub buffer"
  (let ((sub-window (split-window (dwm-main-window) nil 'right)))
    (set-window-buffer sub-window buffer)))

(defun dwm-rotate-buffers (arg)
  (interactive "p")
  (save-selected-window
    (let* ((wins (window-list-1))
           (bufs (mapcar #'window-buffer wins)))
      (when (< 1 (length wins))
        (if (< 0 arg)
            (setq bufs (append (last bufs) (butlast bufs)))
          (setq bufs (append (cdr bufs) (list (car bufs)))))
        (mapcar* #'set-window-buffer wins bufs)))))

(defun dwm-rotate-buffers-backwards (arg)
  (interactive "p")
  (dwm-rotate-buffers (- arg)))

(defun dwm-load-sub-buffer (buffer)
  "creates a new sub buffer and returns the window created.
this doesn't care if duplicates are created"
  (let ((sub-window (dwm-first-sub-window)))
    (if sub-window
        (progn
          (condition-case _e
              (split-window sub-window window-min-height 'above)
            (error (let ((last-sub-win (dwm-last-sub-window)))
                     (delete-window last-sub-win)
                     (balance-windows)
                     (split-window (dwm-first-sub-window) window-min-height 'above))))
          (set-window-buffer (dwm-first-sub-window) buffer))
      (dwm-create-sub-buffer buffer))
    (dwm-first-sub-window)))

;; (defun dwm-delete-all-buffers (buf &optional preserve-win)
;;   "deletes all windows that has buf open. This preserves the main
;; window by switching places of buffers if it has to.

;; If preserve-win is a window, then do not delete that window. This will
;; cause duplicated buffers though."
;;   (let ((main (dwm-main-window))
;;         (subs (dwm-sub-windows t)))
;;     ;; check if main is going to be removed
;;     (when (and
;;            subs
;;            (equal (window-buffer main) buf))
;;       (let ((random-window (find-if (lambda (sub)
;;                                      (not (equal (window-buffer sub) (window-buffer main))))
;;                                     subs)))
;;         (when random-window
;;           (set-window-buffer main (window-buffer random-window))
;;           ;; if random-window should be preserved, don't do anything with it
;;           (unless (and
;;                    preserve-win
;;                    (equal preserve-win random-window))
;;             (set-window-buffer random-window buf))))))
;;   (delete-windows-on buf)
;;   (balance-windows))

(defun dwm-delete-duplicated-sub-buffers ()
  "removes all sub windows that show duplicated buffers"
  (interactive)
  (when (> (length (window-list-1)) 1)
    (let* ((sub-buffers (mapcar #'(lambda (edge-win)
                                   (cons (window-buffer (cdr edge-win))
                                         (cdr edge-win)))
                               (dwm-sub-windows)))
           (main (dwm-main-window))
           (buffers (cons (cons (window-buffer main) main) sub-buffers)))
      (maplist #'(lambda (buf-wins)
                   (let ((current (caar buf-wins)))
                     (mapcar #'(lambda (buf-win)
                                 (when (and
                                        (window-live-p (cdr buf-win))
                                        (equal current (car buf-win)))
                                   (delete-window (cdr buf-win))))
                             (cdr buf-wins))))
              buffers))))

(defun dwm-match-ignore-p (buffer-or-name)
  "returns non-nil if the name of buf matches a regexp in
`dwm-ignore-buffers-regexp'"
  (let ((buf (get-buffer-create buffer-or-name)))
    (if (cl-find-if #'(lambda (regex)
                        (string-match regex (buffer-name buf)))
                    dwm-ignore-buffers-regexp)
        t)))

(defun dwm-swap (win1 win2)
  (let ((buf1 (window-buffer win1))
        (buf2 (window-buffer win2)))
    (set-window-buffer win1 buf2)
    (set-window-buffer win2 buf1)))

(defun dwm-load-buffer (win buffer-or-name &optional keep-old-buffer)
  "makes sure that win is displaying buffer-or-name and that it is selected.
If keep-old-buffer is non-nil, then make sure that the previous buffer is still open"
  (let ((loading-buf (get-buffer-create buffer-or-name))
        (old-buf (window-buffer win)))
    (select-window win)
    (unless (equal loading-buf old-buf)
      (save-selected-window
        (let ((other-win (get-buffer-window loading-buf)))
          (if other-win
              (dwm-swap other-win win)
            (set-window-buffer win loading-buf)
            (when keep-old-buffer
              (dwm-load-sub-buffer old-buf)
              (balance-windows))))))
    ;; (set-buffer loading-buf)
    ))

(defun dwm-focus-buffer ()
  "focus the current window into master"
  (interactive)
  (let* ((cur-buf (window-buffer (selected-window)))
         (main-win (dwm-main-window)))
    ;; (unless (equal main-win (selected-window))
    ;;   (dwm--load-buffer main-win buf win-buf))
    (dwm-load-buffer main-win cur-buf t)))

(defun dwm-switch-to-buffer (org-func buffer-or-name &rest args)
  "load buffer-or-name in main and move previous main to a sub window"
  (if (dwm-match-ignore-p buffer-or-name)
      (apply org-func buffer-or-name args)
    (dwm-load-buffer (dwm-main-window) buffer-or-name t)))

(defun dwm-switch-to-buffer-display (org-func buffer-or-name &optional ACTION FRAME)
  "same as `dwm-switch-to-buffer-sub' except that this return the window
instead of the buffer and that it doesn't select to window. Used with `display-buffer'"
  ;; (message "debug display-buffer %s" ACTION)
  ;; (if ACTION
  ;;     (funcall org-func buffer-or-name ACTION FRAME))
  (save-selected-window
    (let ((b (dwm-switch-to-buffer-sub org-func buffer-or-name ACTION FRAME)))
      (if (buffer-live-p b)
          (get-buffer-window b)
        b))))

(defun dwm-switch-to-buffer-sub (org-func buffer-or-name &rest args)
  "load buffer-or-name in first subwindow and select it"
  (unless (dwm-switch-if-open buffer-or-name)
    (let* ((loading-buf (get-buffer-create buffer-or-name)))
      (if (dwm-match-ignore-p loading-buf)
          (apply org-func buffer-or-name args)
        ;; (dwm-delete-duplicated-buffer loading-buf)
        (select-window (dwm-load-sub-buffer loading-buf))
        (balance-windows)
        loading-buf))))

(defun dwm-next-buffer ()
  "goto next"
  (interactive)
  (select-window (dwm-find-next-window (selected-window))))

(defun dwm-prev-buffer ()
  "goto previous"
  (interactive)
  (select-window (dwm-find-prev-window (selected-window))))

(defun dwm-goto-main ()
  "goto main"
  (interactive)
  (select-window (dwm-main-window)))

(defun dwm-continue-main-window (org-func &optional window)
  "Makes sure that the main window always exist in a deletion of a window"
  (let ((win (or window (selected-window)))
        (first-sub (dwm-first-sub-window)))
    (when (equal win (dwm-main-window))
      ;; select main and swap main and first sub
      (dwm-load-buffer win (window-buffer first-sub))
      (setq win first-sub))
    (funcall org-func win)
    (balance-windows)))

(defun dwm-quit-window-always-close (org-func &optional kill window)
  "makes `quit-window' always close instead of replacing the buffer and 
ending up having two windows with the same buffer"
  (delete-window (or window (selected-window))))

(defun dwm-switch-if-open (buf-or-name &optional window)
  "switches to buf-or-name if it is visible in a live window. returns
non-nil if it switched or nil if there wasn't anything to switch to

With optional argument window, check only if window contains
buf-or-name and maybe switch to it."
  (let* ((suc (get-buffer-window-list buf-or-name)))
    (when window
      (setq suc (member window suc)))
    (when suc
      (select-window (car suc)))
    suc))

(defun dwm-set-buffer (buf-or-name)
  "set the current window to buf-or-name
?, but if bur-or-name is
already open in a window, switch to that window instead."
  (interactive "B")
  (dwm-load-buffer (selected-window) buf-or-name nil))

(defun dwm-maximize-main ()
  "deletes all other windows than the main one"
  (interactive)
  (select-window (dwm-main-window))
  (delete-other-windows))

(defvar dwm-mode-key-map (make-sparse-keymap))
(let ((keys '(("C-x B"      . dwm-set-buffer)
              ("C-x b"      . switch-to-buffer)
              ("S-<up>"     . dwm-next-buffer)
              ("S-<down>"   . dwm-prev-buffer)
              ("S-<left>"   . dwm-goto-main)
              ("S-<right>"  . dwm-maximize-main)
              ("M-<return>" . dwm-focus-buffer)
              ("M-<up>"     . dwm-rotate-buffers)
              ("M-<down>"   . dwm-rotate-buffers-backwards)
              ("M-<left>"   . dwm-focus-buffer)
              ("M-<right>"  . dwm-delete-duplicated-sub-buffers))))
  (dolist (key-data keys)
    (define-key dwm-mode-key-map (kbd (car key-data)) (cdr key-data))))

(define-minor-mode dwm-mode
  "Enable tiled window manage"
  :keymap dwm-mode-key-map
  :global t
  (if dwm-mode
      (progn
        (advice-add 'delete-window :around 'dwm-continue-main-window)
        (advice-add 'switch-to-buffer-other-window :around 'dwm-switch-to-buffer-sub)
        (advice-add 'switch-to-buffer :around 'dwm-switch-to-buffer)
        (advice-add 'pop-to-buffer :around 'dwm-switch-to-buffer)
        (advice-add 'display-buffer :around 'dwm-switch-to-buffer-display)
        (advice-add 'quit-window :around 'dwm-quit-window-always-close) ;; help
        ;; (advice-add 'top-level :after 'dwm-delete-duplicated-sub-buffers)
        )
    (advice-remove 'delete-window 'dwm-continue-main-window)
    (advice-remove 'switch-to-buffer-other-window 'dwm-switch-to-buffer-sub)
    (advice-remove 'switch-to-buffer 'dwm-switch-to-buffer)
    (advice-remove 'pop-to-buffer 'dwm-switch-to-buffer)
    (advice-remove 'display-buffer 'dwm-switch-to-buffer-display)
    (advice-remove 'quit-window 'dwm-quit-window-always-close)
    ;; (advice-remove 'top-level 'dwm-delete-duplicated-sub-buffers)
    ))


(defmacro dwm-add-debug-advice (cmd)
  (let ((name (intern (concat "dwm-print-debug-" (symbol-name cmd)))))
    `(mapcar #'eval '((defun ,name (&rest args)
                        (message "%s (%s): %s" ',cmd real-this-command args))
                      (advice-add ',cmd :before ',name)))))

(defmacro dwm-remove-debug-advice (cmd)
  (let ((name (intern (concat "dwm-print-debug-" (symbol-name cmd)))))
    `(advice-remove ',cmd ',name)))

(defun dwm-add-debug ()
  (interactive)
  (dwm-add-debug-advice delete-window)
  (dwm-add-debug-advice switch-to-buffer-other-window)
  (dwm-add-debug-advice switch-to-buffer)
  (dwm-add-debug-advice pop-to-buffer)
  (dwm-add-debug-advice display-buffer)
  (dwm-add-debug-advice quit-window))

(defun dwm-remove-debug ()
  (interactive)
  (dwm-remove-debug-advice delete-window)
  (dwm-remove-debug-advice switch-to-buffer-other-window)
  (dwm-remove-debug-advice switch-to-buffer)
  (dwm-remove-debug-advice pop-to-buffer)
  (dwm-remove-debug-advice display-buffer)
  (dwm-remove-debug-advice quit-window))

(provide 'dwm)
;;; dwm.el ends here
