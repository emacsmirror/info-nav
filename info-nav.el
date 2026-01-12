;;; info-nav.el --- Browse info docs with a 2 pane layout  -*- lexical-binding: t; -*-

;; Author: ggxx <https://codeberg.org/ggxx>
;; URL: https://codeberg.org/ggxx/info-nav
;; Keywords: docs, hypermedia
;; Package-Version: 1.0.3
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Usage:  M-x info-nav RET elisp RET
;;
;;  The info-nav system is my attempt to make it easier to navigate
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

(defvar-local info-nav--manual nil
  "Name of the current manual.")

(defvar-local info-nav--is-toc nil
  "Is the current buffer the table of contents?")

(defvar-local info-nav--is-content nil
  "Is the current buffer the contents?")

(defvar info-nav--buffer-name-template "*info-nav*<%s> (%s)"
  "Template for generating buffer names for info-nav buffers.")

(defconst info-nav--rx-toc-link
  (rx "*Note " (group (*? not-newline)) "::")
  "A regexp to match an info node link in the table of contents.
The first match should be the info node name.")

;;;###autoload
(defun info-nav (manual)
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
    (let* ((toc-buffer     (info-nav--get-buffer manual 'toc))
           (content-buffer (info-nav--get-buffer manual 'content)))
      (switch-to-buffer toc-buffer)
      (unless (equal (info-nav--current-manual) info-nav--manual)
        (Info-goto-node (format "(%s)" info-nav--manual)))
      (Info-toc)
      (split-window-right toc-width)
      ;; setup content
      (windmove-right)
      (switch-to-buffer content-buffer)
      (unless (equal (info-nav--current-manual) info-nav--manual)
        (Info-goto-node (format "(%s)" info-nav--manual))))))

(defun info-nav--current-manual ()
  "Return the name of the current manual."
  (thread-first
    Info-current-file
    file-name-nondirectory
    file-name-sans-extension))

;; *info-nav*<toc> (%s)
;; *info-nav*<content> (%s)
(defun info-nav--get-buffer (manual role)
  "Find or create the info buffer for the given MANUAL and ROLE.
The given role should be either `toc or `content."
  (unless (memq role '(toc content))
    (error "The given role (%s) is not 'toc or 'content" role))
  (let* ((buffer-name (format info-nav--buffer-name-template role manual))
         (buffer      (get-buffer buffer-name)))
    (if buffer
        (progn
          (message "buffer for %s %s exists" manual role)
          buffer)
      ;; initialize a new info-nav buffer
      (message "creating new buffer for %s %s" manual role)
      (let* ((new-info-buffer (get-buffer-create buffer-name)))
        (with-current-buffer new-info-buffer
          (rename-buffer buffer-name)
          (Info-mode)
          (Info-goto-node (format "(%s)" manual))
          (pcase role
            ('toc
             (setq-local
              info-nav--manual manual
              info-nav--is-toc t)
             (Info-toc))
            ('content
             (setq-local
              info-nav--manual     manual
              info-nav--is-content t))))
        new-info-buffer))))

(defun info-nav--get-node-name-at-point ()
  "Get the info node name that the TOC is referencing at the current point."
  (when-let* ((line (thing-at-point 'line))
              (match (string-match info-nav--rx-toc-link line))
              (node  (match-string 1 line)))
    (format "(%s) %s" info-nav--manual node)))

(defun info-nav--go ()
  "Go to the info node at point using the info-nav content buffer."
  ;; TODO: Ensure that we're in the 2-pane layout.
  (when-let* ((node (info-nav--get-node-name-at-point)))
    (windmove-right)
    (Info-goto-node node)))

(defun info-nav--mouse-advice (orig &rest args)
  "Advise `Info-mouse-follow-nearest-node' taking ORIG and ARGS.
Intercept mouse clicks and navigate using `info-nav--go' instead
when appropriate."
  (interactive "e" Info-mode)
  ;; click while focused on toc
  (cond
   (info-nav--is-toc     (info-nav--go))
   (info-nav--is-content (let* ((b  (current-buffer))
                                  (w  (caadar args))
                                  (wb (window-buffer w)))
                             ;; (message "b:%s conbuf:%s wb:%s"
                             ;;          b info-nav-content-buffer wb)
                             (if (equal b wb)
                                 (apply orig args)
                               (windmove-left)
                               (info-nav--go))))
   ;; default
   (t (apply orig args))))

(defun info-nav--keyboard-advice (orig &rest args)
  "Advise `Info-follow-nearest-node' using ORIG and ARGS.
Intercept RET and use `info-nav--go' insetad when appropriate."
  (interactive "P" Info-mode)
  (if info-nav--is-toc
      (info-nav--go)
    (apply orig args)))

(defun info-nav--consult-advice (orig &rest args)
  "Advise `consult-info--position' taking ORIG and ARGS.
Make `consult-info' behave correctly in an `info-nav' context."
  (let* ((candidate (car args)))
    (when (functionp 'consult-info--position)
      (pcase (consult-info--position candidate)
        (`( ,_matches ,_pos ,node ,_bol ,_buf)
         (message "node %s" node)
         (cond
          (info-nav--is-toc (progn (windmove-right) (Info-goto-node node)))
          (info-nav--is-content (progn (Info-goto-node node)))
          (t (progn (apply orig args)))))))))

(advice-add
 #'Info-mouse-follow-nearest-node
 :around
 #'info-nav--mouse-advice)

(advice-add
 #'Info-follow-nearest-node
 :around
 #'info-nav--keyboard-advice)

;; I'm only applying this advice when the function exists.
(when (fboundp 'consult-info--action)
  (advice-add
   #'consult-info--action
   :around
   #'info-nav--consult-advice))

(provide 'info-nav)
;;; info-nav.el ends here.
