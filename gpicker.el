;;; gpicker.el --- gpicker integration for Emacs -- lexical-binding: t; --

;; Copyright (C) 2008, 2026 Aliaksei Kandratsenka

;; Author: Aliaksei Kandratsenka <alkondratenko@gmail.com>
;; Version: 2.0
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
;;   (global-set-key (kbd "s-f") 'gpicker-find-file)
;;   (global-set-key (kbd "C-x 4 s-f") 'gpicker-find-file-other-window)
;;   (global-set-key (kbd "C-x 5 s-f") 'gpicker-find-file-other-frame)
;;
;; On most keyboards Super is a key with flag between Ctrl and Alt. On
;; mac keyboards it's Command key.
;;
;; Also check out gpicker-imenu and gpicker-isearch. As well
;; https://github.com/alk/supermegadoc/blob/master/supermegadoc.el

;;; Code

(require 'imenu)
(require 'ffap)

(defvar gpicker-program "gpicker"
  "The absolute directory path of the currently tracked project.")

(defvar gpicker-extra-args '("--disable-bzr" ;; bzr builtin file listing is just too slow
                             "--load-stdin-too"
                             "--multiselect"))
(defvar gpicker-project-dir nil
  "The absolute directory path of the currently tracked project.")

(defvar gpicker-project-type "guess"
  "The system backend used by gpicker to determine indexing rules (e.g., git, guess).")

(defvar gpicker--saved-completing-read-function #'completing-read-default
  "Internal reference used to restore the standard Emacs completion framework.")

(defun gpicker--invoke (args &optional stdin-setter)
  "Invoke `gpicker-program' with ARGS.
If STDIN-SETTER is provided, invoke it to populate the stdin buffer.
Returns stdout as a string on success, or nil on failure while appending stderr to *Messages*."
  (let ((err-file (make-temp-file "gpicker-err-")))
    (unwind-protect
        (with-temp-buffer
          (let ((stdout-buffer (current-buffer)))
            (with-temp-buffer
              (when stdin-setter
                (funcall stdin-setter))
              (let* ((destination (list stdout-buffer err-file))
                     (status (apply #'call-process-region
                                    (point-min) (point-max)
                                    gpicker-program nil destination nil args)))
                (if (eql status 0)
                    (with-current-buffer stdout-buffer
                      (buffer-string))
                  (when (file-exists-p err-file)
                    (let ((err-msg (with-temp-buffer
                                     (insert-file-contents err-file)
                                     (buffer-string))))
                      (message "[gpicker] Process failed with status %S:\n%s" status err-msg)))
                  nil)))))
      (when (file-exists-p err-file)
        (delete-file err-file)))))

(defun gpicker-pick (dir)
  "Run the interactive file selector on directory DIR."
  (let* ((gpicker-args (append (list "--init-filter" (ffap-string-at-point))
                               gpicker-extra-args
                               (and gpicker-project-type (list "-t" gpicker-project-type))
                               (list dir)))
         (result (gpicker--invoke gpicker-args
                                  (lambda ()
                                    (dolist (b (buffer-list))
                                      (let ((name (buffer-name b)))
                                        (unless (or (eq (current-buffer) b)
                                                    (string= (substring name 0 1) " ")
                                                    (buffer-file-name b))
                                          (insert name "\0"))))))))
    (discard-input)
    (and result (split-string result "\0" t))))


(defun gpicker--select-index (list &optional init-filter)
  "Run gpicker to select among the LIST of items. Returns index of selected item or nil"
  (let* ((args (list "-l" "-d" "\01" "--output-index"
                      "--init-filter" (or init-filter (ffap-string-at-point))
                      "-"))
         (result (gpicker--invoke args
                                  (lambda ()
                                    (dolist (e list)
                                      (insert e "\0"))))))
    (discard-input)
    (and result (not (string-empty-p result)) (string-to-number result))))

(defun gpicker-complete-list (list &optional init-filter)
  "Run gpicker to select among the LIST of items. Returns selected item or nil"
    (let ((idx (gpicker--select-index list init-filter)))
    (and idx (nth idx list))))

(defun gpicker-set-project-type (type)
  "Sets type of current gpicker project"
  (interactive (list (completing-read "Choose gpicker project type: "
                                      '("guess" "script" "git" "hg"
                                        "bzr" "default" "mlocate")
                                      nil t nil nil "guess")))
  (setq gpicker-project-type type))

(defun gpicker-visit-project (dir)
  "Selects current project directory for gpicker"
  (interactive "DSelect directory:")
  (cd dir)
  (setq gpicker-project-type "guess")
  (setq gpicker-project-dir (expand-file-name dir)))

(defun gpicker-call-find-function (find-function original-file)
  (let ((file (expand-file-name original-file gpicker-project-dir)))
    (if (file-exists-p file)
        (let ((revert-without-query (list (regexp-quote (abbreviate-file-name file)))))
          (funcall find-function file))
    (let ((b (get-buffer original-file)))
      (and b (display-buffer b))))))

(defun gpicker-do-find (find-function)
  "Query current project directory and invoke FIND-FUNCTION on selected results."
  (unless gpicker-project-dir
    (error "visit gpicker project via 'gpicker-visit-project first!"))
  (let ((files (gpicker-pick gpicker-project-dir)))
    (if (and (eql find-function #'find-file)
             (eql (length files) 2))
        (progn
          (gpicker-call-find-function #'find-file (car files))
          (gpicker-call-find-function #'find-file-other-window (cadr files)))
      (dolist (file files)
        (gpicker-call-find-function find-function file)))))

(defun gpicker-find-file ()
  "Interactively look up project files and open in the current window."
  (interactive)
  (gpicker-do-find #'find-file))

(defun gpicker-find-file-other-window ()
  "Interactively look up project files and open in a new split."
  (interactive)
  (gpicker-do-find #'find-file-other-window))

(defun gpicker-find-file-other-frame ()
  "Interactively look up project files and open in a new desktop frame."
  (interactive)
  (gpicker-do-find #'find-file-other-frame))

(defun gpicker-flatten-imenu (imenu-ilist &optional prefix)
  (let* ((prefix (or prefix ""))
         (list-of-lists (mapcar (lambda (pair)
                                  (let* ((snd (cdr pair))
                                         (item (car pair))
                                         (name (concat prefix item)))
                                    (if (listp snd)
                                        ;; if pair's cdr is list then it's sublist
                                        (gpicker-flatten-imenu snd (concat name "/"))
                                      ;; else it's marker
                                      (list (cons name snd)))))
                                imenu-ilist)))
    (apply #'append list-of-lists)))

(defun gpicker-imenu (&optional init-filter)
  (interactive)
  (let ((result t))
    (while (eq result t)
      (let* ((raw-imenu-alist (imenu--make-index-alist))
             (imenu-alist (gpicker-flatten-imenu raw-imenu-alist))
             (selected  (gpicker-complete-list (mapcar #'car imenu-alist) init-filter)))
        (setq result (assoc selected imenu-alist))
        (when (equal result imenu--rescan-item)
          (imenu--cleanup)
          (setq result t))))
    (when result
      (imenu result))))

(defun gpicker-isearch ()
  (interactive)
  (let* ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
         (args (list "-I" "-l" "-P" "-d" "\n" "-n" "\n" "-"))
         (result (gpicker--invoke args
                                  (lambda ()
                                    (insert buffer-content))))
         (line-num (and result (> (length result) 0) (string-to-number result 10))))
    (when line-num
      (goto-char (point-min))
      (forward-line line-num))))

(defun gpicker-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Drop-in high-performance backend replacement for Emacs' completing-read-function'. Delegates options directly to gpicker', seamlessly resolving indices safely."
  (let ((use-gpicker (and (eq def nil)
                          (eq require-match t))))
    (if use-gpicker
        (let* ((candidates (all-completions "" collection predicate))
               (init-str (cond ((stringp initial-input) initial-input)
                               ((consp initial-input) (car initial-input))
                               (t "")))
               (idx (gpicker--select-index candidates init-str)))
          (if idx
              (nth idx candidates)
            (keyboard-quit))) ; Gracefully abort Emacs action if user cancelled selection
      ;; Fallback to standard Emacs completions
      (let ((completing-read-function gpicker--saved-completing-read-function))
        (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method)))))

(define-minor-mode gpicker-complete-mode
  "Global minor mode to route Emacs completion dialogs through gpicker."
  :global t
  :group 'gpicker
  (if gpicker-complete-mode
      (progn
        (setq gpicker--saved-completing-read-function completing-read-function)
        (setq completing-read-function #'gpicker-completing-read)
        (message "[GPicker] Completion wrapper initialized successfully"))
    (setq completing-read-function gpicker--saved-completing-read-function)
    (message "[GPicker] Restored original Emacs completion engine")))

(provide 'gpicker)
