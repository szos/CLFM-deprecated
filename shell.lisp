
(in-package :clfm)

(defun run-shell-command (command)
  (let ((stream (uiop:process-info-output
		 (uiop:launch-program command :output :stream)))
	(ostr "")
	(whiler t))
    (setf ostr (handler-case (read-line stream)
		 (end-of-file ()
		   (setf whiler nil)
		   "")))
    (when whiler
      (loop while whiler
	    do (setf ostr (handler-case
			      (concatenate 'string ostr '(#\newline)
					   (read-line stream))
			    (end-of-file ()
			      (setf whiler nil)
			      ostr))))
      ostr)))

