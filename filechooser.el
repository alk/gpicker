
(defvar *filechooser-path* (expand-file-name "~/src/altoros/filechooser-c/filechooser"))
(defvar *filechooser-project-dir* (expand-file-name "~/src/altoros/phase1"))

(defun filechooser-pick (dir)
  (let ((chooser-buffer (generate-new-buffer "*filechooser*")))
    (unwind-protect (let ((status (call-process *filechooser-path*
                                                nil ;; input
                                                (list chooser-buffer "/tmp/fc-errors.log")
                                                nil ;; dont redisplay
                                                dir)))
                      (if (eql status 0)
                          (save-excursion
                            (set-buffer chooser-buffer)
                            (read (buffer-string)))
                        (with-current-buffer (get-buffer-create "*Messages*")
                          (message "filechooser exited with status %d" status))
                        (save-excursion
                          (set-buffer "*Messages*")
                          (goto-char (point-max))
                          (insert-file-contents "/tmp/fc-errors.log"))
                        nil))
      (with-current-buffer chooser-buffer
	(set-buffer-modified-p nil))
      (kill-buffer chooser-buffer))))

(defun filechooser-visit-project (dir)
  "Selects current project directory for filechooser"
  (interactive "DSelect directory:")
  (cd dir)
  (setq *filechooser-project-dir* (expand-file-name dir)))

(defun filechooser-do-find (find-function)
  (let ((file (filechooser-pick *filechooser-project-dir*)))
    (when file
      (setq file (expand-file-name file *filechooser-project-dir*))
      (funcall find-function file))))

(defun filechooser-find-file ()
  (interactive)
  (filechooser-do-find #'find-file))

(defun filechooser-find-file-other-window ()
  (interactive)
  (filechooser-do-find #'find-file-other-window))

(defun filechooser-find-file-other-frame ()
  (interactive)
  (filechooser-do-find #'find-file-other-frame))
