
(in-package :clfm)

(defclass clfm-object ()
  ((pathname :initarg :path
	     :accessor path
	     :initform nil
	     :documentation "The path to the object")
   (modified-date :initarg :last-modified
		  :accessor last-modified
		  :initform nil
		  :documentation
		  "The last time this file or directory was changed")
   (user :initarg :user
	 :accessor user
	 :initform nil
	 :documentation "The user who owns this, changed via chown")
   (group :initarg :group
	  :accessor group
	  :initform nil
	  :documentation "the group who owns this, changed via chgrp")
   (permissions :initarg :permissions
		:accessor permissions
		:initform nil
		:documentation "the permissions of this thing")))

(defclass clfm-directory (clfm-object)
  ((subdirs :initarg :subdirectories
	    :accessor subdirectories
	    :initform nil
	    :documentation "a list of clfm-directory objects from this directory")))

(defclass clfm-file (clfm-object)
  ((size :initarg :size
	 :accessor size
	 :initform nil
	 :documentation "the size of the file")))

(defun clfm-directory-p (object)
  (eq (type-of object) 'clfm-directory))
(defun clfm-file-p (object)
  (eq (type-of object) 'clfm-file))

(defmethod print-object ((obj clfm-directory) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a" (path obj))))

(defmethod print-object ((obj clfm-file) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a" (path obj))))

(defun directory-contents (directory)
  (sort-pathnames-alphabetically
   (append (uiop:subdirectories directory)
	   (uiop:directory-files directory))))

(defun current-directory-contents ()
  (directory-contents (uiop:getcwd)))

(defun sort-pathnames-alphabetically (paths)
    ;; TODO: change this to be case insensitive but still return paths
    ;;       in their original case. 
  (loop for (d . path) in (sort (loop for path in paths
				collect (cons (string-upcase
					       (namestring path))
					      (namestring path)))
			  #'string<= :key #'car)
	collect (uiop:parse-unix-namestring path)))

(defun print-directory-structure (structure)
  (labels ((print-subdirs (subs)
	     (when subs
	       (when (path subs)
		 (print (path subs)))
	       (when (subdirectories subs)
		 (print-subdirs (subdirectories subs))))))
    (loop for d in structure
	  do (print-subdirs (subdirectories d)))))

(defmethod display-name ((file-or-directory clfm-object))
  (car (last (cl-ppcre:split "/" (path file-or-directory)))))

(labels ((directory= (dir1 dir2)
	   (let ((dirstring1 (uiop:directory-exists-p
			      (if (clfm-directory-p dir1)
				  (path dir1)
				  dir1)))
		 (dirstring2 (uiop:directory-exists-p
			      (if (clfm-directory-p dir2)
				  (path dir2)
				  dir2))))
	     (equal dirstring1 dirstring2)))
	 (get-uid (stat)
	   (let* ((uid (osicat-posix:stat-uid stat))
		  (name (assoc uid *uid-list*)))
	     (if name
		 (cdr name)
		 (let ((user (run-shell-command (format nil "awk -v val=~a -F \":\" '$3==val{print $1}' /etc/passwd" uid))))
		   (setf *gid-list* (cons (cons uid user) *gid-list*))
		   user)
		 )))
	 (get-gid (stat)
	   (let* ((gid (osicat-posix:stat-gid stat))
		  (name (assoc gid *gid-list*)))
	     (if name
		 (cdr name)
		 (let ((group (run-shell-command (format nil "awk -v val=~a -F \":\" '$4==val{print $1}' /etc/passwd" gid))))
		   (setf *gid-list* (cons (cons gid group) *gid-list*))
		   group))))
	 (collect-everything-in (dir)
	   (loop for d in (directory-contents (path dir))
		 collect (let* ((stat (osicat-posix:stat d))
				(user (get-uid stat))
				(group (get-gid stat)))
			   (cond ((uiop:directory-exists-p d)
				  (make-instance 'clfm-directory
						 :path (namestring d)
						 :user user
						 :group group
						 :subdirectories nil))
				 ((uiop:file-exists-p d)
				  (make-instance 'clfm-file
						 :path (namestring d)))
				 (t
				  "Neither a directory nor a file")))))
	 (collect-subdirs (dir)
	   (loop for d in (uiop:subdirectories (path dir))
		 collect (make-instance 'clfm-directory
					:path (namestring d)
					:subdirectories nil)))
	 (find-directory (dir dir-list)
	   (loop for thing in dir-list
		 if  (and (clfm-directory-p thing)
			  (or
			   (uiop:string-prefix-p (path thing) dir)
			   (directory= (path thing) dir)))
		   return (cond ((directory= (path thing) dir)
				 (unless (subdirectories thing)
				   (setf (subdirectories thing)
					 (collect-everything-in thing)))
				 thing)
				((not (subdirectories thing))
				 (setf (subdirectories thing)
				       (collect-subdirs thing))
				 (find-directory dir (subdirectories thing)))
				(t
				 (find-directory dir (subdirectories thing)))))))
  (defmethod access-directory-or-file ((object clfm-directory))
    (uiop:chdir (path object))
    (when (not (subdirectories object))
      (setf (subdirectories object)
	    (collect-subdirs object)))
    (setf (current-dir *application-frame*)
	  (cons object (current-dir *application-frame*)))
    (redisplay-frame-panes *application-frame*))
  (defmethod access-directory-or-file ((object clfm-file))
    (run-file-associated-program (path object)))
  (defun find-directory-in-filesystem (directory)
    (let ((dir-string (namestring directory)))
      (if (or (string= "/" dir-string)
	      (string= "/home/.." dir-string))
	  *filesystem*
	  (find-directory dir-string (subdirectories *filesystem*)))))
  (defun enter-directory (directory)
    (check-type directory clfm-directory)
    (uiop:chdir (path directory))
    (when (not (subdirectories directory))
      (setf (subdirectories directory)
	    (collect-subdirs directory)))
    (setf (current-dir *application-frame*)
	  (cons directory (current-dir *application-frame*)))))

(defun generate-filesystem-objects ()
  (let ((cwd (namestring (uiop:getcwd))))
    (labels ((collecter (dir)
	       (loop for d in (directory-contents dir)
		     collect (cond ((uiop:directory-exists-p d)
				    (make-instance 'clfm-directory
						   :path (namestring d)
						   :subdirectories
						   (when (uiop:string-prefix-p
							  (namestring d) cwd)
						     (collecter (namestring d)))))
				   ((uiop:file-exists-p d)
				    (make-instance 'clfm-file
						   :path (namestring d)))
				   (t
				    "Neither a directory nor a file")))))
      (make-instance 'clfm-directory
		     :path "/"
		     :subdirectories (collecter "/")))))

(defparameter *filesystem* (generate-filesystem-objects))

(defun initialize-directory-structure ()
  (let ((cwd (namestring (uiop:getcwd))))
    (labels ((collect-to-current-dir (dir-to-collect)
	       (loop for dir in (uiop:subdirectories dir-to-collect)
		     collect (make-instance 'clfm-directory
					    :path (namestring dir)
					    :subdirectories
					    (when (uiop:string-prefix-p
						   (namestring dir) cwd)
					      (collect-to-current-dir dir))))))
      (collect-to-current-dir "/")))
  ;; (let* (rootfs
  ;; 	 (cwd (uiop:getcwd))
  ;; 	 (destructured-fs (cdr (cl-ppcre:split "/" cwd))))
  ;;   (setf rootfs
  ;; 	  (loop for directory in (uiop:subdirectories "/")
  ;; 		collect (make-instance 'clfm-directory
  ;; 				       :path (namestring directory)
  ;; 				       :subdirs nil
  ;; 				       ;; When were in a directory on the way to
  ;; 				       ;; our cwd we should collect it. 
  ;; 				       ;; (when (uiop:string-prefix-p
  ;; 				       ;; 	      (namestring directory)
  ;; 				       ;; 	      (namestring cwd)))
  ;; 				       )))
  ;;   (loop for dir in rootfs
  ;; 	  if (uiop:string-prefix-p (path dir)
  ;; 				   )))
  )
