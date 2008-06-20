
(defvar *filechooser-path* (expand-file-name "~/src/altoros/filechooser-c/run-chooser.rb"))
(defvar *filechooser-project-dir* (expand-file-name "~/src/altoros/phase1"))

(defun filechooser-pick (dir)
  (let ((chooser-buffer (generate-new-buffer "*filechooser*")))
    (unwind-protect (progn
		      (call-process *filechooser-path*
				    nil
				    (list chooser-buffer (expand-file-name "~/fc-errors.log"))
				    nil
				    dir)
		      (with-current-buffer chooser-buffer
			(read (buffer-string))))
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
