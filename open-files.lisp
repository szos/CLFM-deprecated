
(in-package :clfm)

(defmacro define-file-extension-shell-command (extension command)
  `(let ((curcmd (assoc ,extension *file-associations*)))
     (if curcmd
	 (setf (cdr curcmd) ,command)
	 (setf *file-associations* (cons '(,extension . ,command)
					 *file-associations*)))))

(defmacro define-extension-commands (&body extensions-and-commands)
  `(loop for (ext cmd) in ',extensions-and-commands
	 do (let ((c (assoc ext *file-associations*)))
	      (if c
		  (setf (cdr c) cmd)
		  (setf *file-associations* (cons (cons ext cmd)
						  *file-associations*))))))

(defparameter *file-associations*
  '(;; ("ogg" . "mpv '~a'")
    )
  "alist of file associations, with the car being the file extension and the 
cdr being the shell command to run. These shell commands MUST have a format 
directive (such as ~a) for the filename to be spliced in")

(define-extension-commands
  ("pdf" "xpdf '~a'")
  ("org" "emacs '~a'"))

(defun determine-file-type (file)
  (with-output-to-string (s)
    (uiop:launch-program (format nil "file -b --mime-type '~a'" file))))

(defun file-extension (file)
  (let ((r (reverse (car (cl-ppcre:split "\\." (reverse (if (pathnamep file)
							    (format nil "~a" file)
							    file)))))))
    ;; (if (and (pathnamep file) (not return-string))
    ;; 	(uiop:parse-unix-namestring r)
    ;; 	r)
    r))

(defun run-file-associated-program (file)
  (let* ((adjusted-file (namestring file))
	 (shell-command (cdr (assoc (file-extension adjusted-file)
				    *file-associations*
				    :test #'string=))))
    (cond ((prefer-xdg-open?)
	   (uiop:launch-program (format nil "xdg-open '~a'" adjusted-file)))
	  (t
	   (let ((command (when shell-command (format nil shell-command file))))
	     (cond (command
		    (uiop:launch-program command))
		   ((allow-xdg-open?)
		    (uiop:launch-program
		     (format nil "xdg-open '~a'" adjusted-file)))
		   (t
		    (print command))))))
    ;; (when shell-command
    ;;   (let ((command (format nil shell-command file)))
    ;; 	(print command)
    ;; 	(uiop:launch-program command)))
    ;; (handler-case (progn
    ;; 		    (format t "running prog: ~%")
    ;; 		    (uiop:run-program (format nil shell-command file)))
    ;;   (sb-format:format-error ()
    ;; 	(format t "running prog after erroring~%")
    ;; 	(uiop:run-program (concatenate 'string shell-command file))))
    ))

;;; We want to make a new frame that prompts the user for yes no questions

;; (define-application-frame clfm-prompter () ()
;;   (:panes ))
