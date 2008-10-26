;;; gpicker.el --- gpicker integration for Emacs

;; Copyright (C) 2008 Aliaksey Kandratsenka

;; Author: Aliaksey Kandratsenka <alk@tut.by>
;; Version: 1.0
;; Keywords: autocompletion

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; `http://www.gnu.org/licenses/'.

;;; Commentary
;;
;; Arrange loading of this file. One way is to add
;;
;;   (load "/usr/local/share/gpicker/gpicker.el")
;;
;; to your .emacs
;;
;; Run `gpicker-visit-project' to specify project directory.
;;
;; Open files by running `gpicker-find-file' or
;; `gpicker-find-file-other-window' or
;; `gpicker-find-file-other-frame'.
;;
;; I prefer to globally bind them to Super-f, C-x 4 Super-f and
;; C-x 5 Super-f correspondingly. You can do same by adding the following
;; elisp snippet to your .emacs
;;
;;   (global-set-key [8388710] 'gpicker-find-file)
;;   (global-set-key [?\C-x ?4 8388710] 'gpicker-find-file-other-window)
;;   (global-set-key [?\C-x ?5 8388710] 'gpicker-find-file-other-frame)
;;
;; On most keyboards Super is a key with flag between Ctrl and Alt. On
;; mac keyboards it's Command key.

;;; Code

(defvar *gpicker-path* "gpicker")
(defvar *gpicker-project-dir* nil)
(defvar *gpicker-project-type* nil)
(defvar *gpicker-errors-log* (expand-file-name "~/.gpicker-errors.log"))

(defun gpicker-pick (dir)
  (unless *gpicker-project-dir*
    (error "visit gpicker project via 'gpicker-visit-project first!"))
  (let ((chooser-buffer (generate-new-buffer "*gpicker*")))
    (unwind-protect (let ((status (call-process *gpicker-path*
                                                nil ;; input
                                                (list chooser-buffer *gpicker-errors-log*)
                                                nil ;; dont redisplay
                                                "-t"
                                                (or *gpicker-project-type* "default")
                                                dir)))
                      (if (eql status 0)
                          (save-excursion
                            (set-buffer chooser-buffer)
                            (read (buffer-string)))
                        (with-current-buffer (get-buffer-create "*Messages*")
                          (message "gpicker exited with status %d" status))
                        (save-excursion
                          (set-buffer "*Messages*")
                          (goto-char (point-max))
                          (insert-file-contents *gpicker-errors-log*))
                        nil))
      (with-current-buffer chooser-buffer
	(set-buffer-modified-p nil))
      (kill-buffer chooser-buffer)
      (discard-input))))

(defun gpicker-guess-project-type (dir)
  (cond ((file-accessible-directory-p (expand-file-name ".git" dir))
         "git")))

(defun gpicker-set-project-type (type)
  "Sets type of current gpicker project"
  (interactive (list (completing-read "Choose gpicker project type: " '("git" "default")
                                      nil t nil nil "default")))
  (setq *gpicker-project-type* type))

(defun gpicker-visit-project (dir)
  "Selects current project directory for gpicker"
  (interactive "DSelect directory:")
  (cd dir)
  (setq *gpicker-project-type* (gpicker-guess-project-type dir))
  (setq *gpicker-project-dir* (expand-file-name dir)))

(defun gpicker-do-find (find-function)
  (let ((file (gpicker-pick *gpicker-project-dir*)))
    (when file
      (setq file (expand-file-name file *gpicker-project-dir*))
      (let ((revert-without-query (list (regexp-quote (abbreviate-file-name file)))))
        (funcall find-function file)))))

(defun gpicker-find-file ()
  (interactive)
  (gpicker-do-find #'find-file))

(defun gpicker-find-file-other-window ()
  (interactive)
  (gpicker-do-find #'find-file-other-window))

(defun gpicker-find-file-other-frame ()
  (interactive)
  (gpicker-do-find #'find-file-other-frame))

(provide 'gpicker)
