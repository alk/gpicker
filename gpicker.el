;;
;; To setup this add lines like this to your .emacs
;;
;; (load (expand-file-name "/usr/local/share/gpicker/gpicker.el"))
;; (global-set-key [8388710] 'gpicker-find-file)
;; (global-set-key [?\C-x ?4 8388710] 'gpicker-find-file-other-window)
;; (global-set-key [?\C-x ?5 8388710] 'gpicker-find-file-other-frame)
;;
;; this will bind super-f to gpicker-find-file

(defvar *gpicker-path* "gpicker")
(defvar *gpicker-project-dir* nil)
(defvar *gpicker-project-type* nil)

(defun gpicker-pick (dir)
  (unless *gpicker-project-dir*
    (error "visit gpicker project via 'gpicker-visit-project first!"))
  (let ((chooser-buffer (generate-new-buffer "*gpicker*")))
    (unwind-protect (let ((status (call-process *gpicker-path*
                                                nil ;; input
                                                (list chooser-buffer "/tmp/gpicker-errors.log")
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
                          (insert-file-contents "/tmp/gpicker-errors.log"))
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
