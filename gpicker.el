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

(require 'ffap)

(defvar *gpicker-path* "gpicker")
(defvar *gpicker-extra-args* '("--disable-bzr" ;; bzr builtin file listing is just too slow
                               "--multiselect"))
(defvar *gpicker-project-dir* nil)
(defvar *gpicker-project-type* "guess")
(defvar *gpicker-errors-log* (expand-file-name "~/gpicker-errors.log"))

(defun gpicker-grab-stdout (&rest call-process-args)
  (with-temp-buffer
    (unwind-protect (let ((status (apply #'call-process
                                         (car call-process-args)
                                         nil
                                         (list (current-buffer) *gpicker-errors-log*)
                                         nil
                                         (cdr call-process-args))))
                      (if (eql status 0)
                          (buffer-string)
                        (message "%s exited with status %d" (car call-process-args) status)
                        (save-excursion
                          (set-buffer "*Messages*")
                          (goto-char (point-max))
                          (insert-file-contents *gpicker-errors-log*))
                        nil))
      (delete-file *gpicker-errors-log*))))

(defun gpicker-pick (dir)
  (unless *gpicker-project-dir*
    (error "visit gpicker project via 'gpicker-visit-project first!"))
  (let ((gpicker-args (append *gpicker-extra-args*
                               (list "-t"
                                     (or *gpicker-project-type* "default")
                                     dir)))
        (at-point (ffap-string-at-point)))
    (when (and at-point
               (> (string-bytes at-point) 0))
      (setq gpicker-args (list* "--init-filter" at-point gpicker-args)))
    (unwind-protect (let ((rv (apply #'gpicker-grab-stdout
                                     *gpicker-path*
                                     gpicker-args)))
                      (and rv
                           (split-string rv "\0" t)))
      (discard-input))))

(defun gpicker-set-project-type (type)
  "Sets type of current gpicker project"
  (interactive (list (completing-read "Choose gpicker project type: "
                                      '("guess" "git" "hg"
                                        "bzr" "default" "mlocate")
                                      nil t nil nil "guess")))
  (setq *gpicker-project-type* type))

(defun gpicker-visit-project (dir)
  "Selects current project directory for gpicker"
  (interactive "DSelect directory:")
  (cd dir)
  (setq *gpicker-project-type* "guess")
  (setq *gpicker-project-dir* (expand-file-name dir)))

(defun gpicker-call-find-function (find-function file)
  (setq file (expand-file-name file *gpicker-project-dir*))
  (let ((revert-without-query (list (regexp-quote (abbreviate-file-name file)))))
    (funcall find-function file)))

(defun gpicker-do-find (find-function)
  (let ((files (gpicker-pick *gpicker-project-dir*)))
    (if (and (eql find-function #'find-file)
             (eql (length files) 2))
        (progn
          (gpicker-call-find-function #'find-file (car files))
          (gpicker-call-find-function #'find-file-other-window (cadr files)))
      (dolist (file files)
        (gpicker-call-find-function find-function file)))))

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
