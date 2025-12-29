;;; info-layout.el --- Browse info docs with a 2 pane layout  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;  Usage:  M-x info-layout RET elisp RET
;;
;;  The info-layout system is my attempt to make it easier to navigate
;;  info documents by using a 2-window layout.
;;  - The windows are arranged side-by-side.
;;  - The left window contains the table of contents
;;    that is always visible.
;;  - The right window contains the actual contents.
;;
;;  Furthermore, actions that select a node on the table of contents
;;  side will drive navigation on the content side.
;;
;;; Code:
(require 'info)
(require 'subr-x)

(defvar-local info-layout--manual nil
  "Name of the current manual.")

(defvar-local info-layout--is-toc nil
  "Is the current buffer the table of contents?")

(defvar-local info-layout--is-content nil
  "Is the current buffer the contents?")

(defvar info-layout--buffer-name-template "*info-layout*<%s> (%s)"
  "Template for generating buffer names for info-layout buffers.")

(defconst info-layout--rx-toc-link
  (rx "*Note " (group (*? not-newline)) "::")
  "A regexp to match an info node link in the table of contents.
The first match should be the info node name.")

;;;###autoload
(defun info-layout (manual)
  "Open an info document for MANUAL in a 2-pane layout."
  (interactive
   (list
    (progn
      (info-initialize)
      (completing-read "Manual name: "
                       (info--filter-manual-names
                        (info--manual-names current-prefix-arg))
                       nil t))))
  (message "imagine %s in 2 panes" manual)
  (delete-other-windows)
  (let* ((display-buffer-alist nil)     ; ignore display-buffer-alist
         (total-width (frame-width))
         (toc-width   (round (- total-width (/ total-width 1.618)))))
    (let* ((toc-buffer     (info-layout--get-buffer manual 'toc))
           (content-buffer (info-layout--get-buffer manual 'content)))
      (switch-to-buffer toc-buffer)
      (unless (equal (info-layout--current-manual) info-layout--manual)
        (Info-goto-node (format "(%s)" info-layout--manual)))
      (Info-toc)
      (split-window-right toc-width)
      ;; setup content
      (windmove-right)
      (switch-to-buffer content-buffer)
      (unless (equal (info-layout--current-manual) info-layout--manual)
        (Info-goto-node (format "(%s)" info-layout--manual))))))

(defun info-layout--current-manual ()
  "Return the name of the current manual."
  (thread-first
    Info-current-file
    file-name-nondirectory
    file-name-sans-extension))

;; *info-layout*<toc> (%s)
;; *info-layout*<content> (%s)
(defun info-layout--get-buffer (manual role)
  "Find or create the info buffer for the given MANUAL and ROLE.
The given role should be either `toc or `content."
  (unless (memq role '(toc content))
    (error "The given role (%s) is not 'toc or 'content" role))
  (let* ((buffer-name (format info-layout--buffer-name-template role manual))
         (buffer      (get-buffer buffer-name)))
    (if buffer
        (progn
          (message "buffer for %s %s exists" manual role)
          buffer)
      ;; initialize a new info-layout buffer
      (message "creating new buffer for %s %s" manual role)
      (let* ((new-info-buffer (get-buffer-create buffer-name)))
        (with-current-buffer new-info-buffer
          (rename-buffer buffer-name)
          (Info-mode)
          (Info-goto-node (format "(%s)" manual))
          (pcase role
            ('toc
             (setq-local
              info-layout--manual manual
              info-layout--is-toc t)
             (Info-toc))
            ('content
             (setq-local
              info-layout--manual     manual
              info-layout--is-content t))))
        new-info-buffer))))

(defun info-layout--get-node-name-at-point ()
  "Get the info node name that the TOC is referencing at the current point."
  (when-let* ((line (thing-at-point 'line))
              (match (string-match info-layout--rx-toc-link line))
              (node  (match-string 1 line)))
    (format "(%s) %s" info-layout--manual node)))

(defun info-layout--go ()
  "Go to the info node at point using the info-layout content buffer."
  ;; TODO: Ensure that we're in the 2-pane layout.
  (when-let* ((node (info-layout--get-node-name-at-point)))
    (windmove-right)
    (Info-goto-node node)))

(defun info-layout--mouse-advice (orig &rest args)
  "Advise `Info-mouse-follow-nearest-node' taking ORIG and ARGS.
Intercept mouse clicks and navigate using `info-layout--go' instead
when appropriate."
  (interactive "e" Info-mode)
  ;; click while focused on toc
  (cond
   (info-layout--is-toc     (info-layout--go))
   (info-layout--is-content (let* ((b  (current-buffer))
                                  (w  (caadar args))
                                  (wb (window-buffer w)))
                             ;; (message "b:%s conbuf:%s wb:%s"
                             ;;          b info-layout-content-buffer wb)
                             (if (equal b wb)
                                 (apply orig args)
                               (windmove-left)
                               (info-layout--go))))
   ;; default
   (t (apply orig args))))

(defun info-layout--keyboard-advice (orig &rest args)
  "Advise `Info-follow-nearest-node' using ORIG and ARGS.
Intercept RET and use `info-layout--go' insetad when appropriate."
  (interactive "P" Info-mode)
  (if info-layout--is-toc
      (info-layout--go)
    (apply orig args)))

(defun info-layout--consult-advice (orig &rest args)
  "Advise `consult-info--position' taking ORIG and ARGS.
Make `consult-info' behave correctly in an `info-layout' context."
  (let* ((candidate (car args)))
    (pcase (consult-info--position candidate)
      (`( ,_matches ,pos ,node ,_bol ,_buf)
       (message "node %s" node)
       (cond
        (info-layout--is-toc
         (progn
           (windmove-right)
           (Info-goto-node node)))
        (info-layout--is-content
         (progn
           (Info-goto-node node)))
        (t
         (progn
           (apply orig args))))))))

(advice-add
 #'Info-mouse-follow-nearest-node
 :around
 #'info-layout--mouse-advice)

(advice-add
 #'Info-follow-nearest-node
 :around
 #'info-layout--keyboard-advice)

(advice-add
 #'consult-info--action
 :around
 #'info-layout--consult-advice)

(when nil
  ;;;; This is the keybinding I use.
  ;;;; I think of it like `C-x 3' but for info.
  (keymap-global-set "C-h 3" #'info-layout)
  )

(provide 'info-layout)
;;; info-layout.el ends here.
