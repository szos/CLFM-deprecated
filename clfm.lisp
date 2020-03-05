;;;; clfm.lisp

(in-package #:clfm)

(defmacro when-let ((&body vars) &body body)
  `(let ,vars
     (when (and ,@(loop for (var val) in vars
			collect var))
       ,@body)))

(defmacro bold ((stream) &body body)
  `(with-text-face (,stream :bold)
     ,@body))

(defmacro italic ((stream) &body body)
  `(with-text-face (,stream :italic)
     ,@body))

(defmacro with-etbembo ((stream &optional type (size 16)) &body body)
  `(with-text-style (,stream (make-text-style "ETBembo"
					      ,(cond ((eq type :bold)
						      "BoldLF")
						     ((eq type :semi-bold)
						      "SemiBoldOSF")
						     ((eq type :italic)
						      "DisplayItalic")
						     (t "RomanLF"))
					      ,size))
     ,@body))

(defmacro loop-for-lines-in-string ((var string) &body body)
  `(with-input-from-string (,var ,string)
     (do ((line (read-line ,var nil)
		(read-line ,var nil)))
	 ((null line) nil)
       ,@body)))

(define-condition invalid-pathname-error (error)
  ((path :initarg :path
	 :accessor path
	 :initform nil)))

(defun make-and-verify-pathname (path)
  (let ((pname (if (pathnamep path) path
		   (make-pathname :name path))))
    (if (or (uiop:directory-exists-p path)
	    (uiop:file-exists-p path))
	pname
	(error 'invalid-pathname-error :path pname))))

(defun file-size (file)
  (let ((path (make-and-verify-pathname file)))
    (osicat-posix:stat-size
     (osicat-posix:stat path))))

(defun file-type (file)
  "Takes a path and returns a string representation of that files type"
  (cond ((uiop:directory-exists-p file)
	 "Directory")
	(t
	 (file-extension file))))

(defun chdir (path)
  (uiop:chdir path)
  (redisplay-frame-panes *application-frame* :force-p t))

(defun make-executable ()
  (sb-ext:save-lisp-and-die "clfm"
			    :toplevel (lambda ()
					(app-main))
			    :executable t
			    :purify t))

(defun initialize-fonts ()
  (labels ((my-reg (port dir)
	     (loop for f in (uiop:directory-files dir)
		   when (string= "ttf" (file-extension f))
		     do (map ()
			     (lambda (size)
			       (mcclim-truetype::make-truetype-font port f size))
			     '(8 10 12 14 16 18 24 48 72)))))
    (my-reg (find-port) (asdf:system-relative-pathname
			 "clfm" "fonts/" :type :directory)))
  (loop for f in (uiop:directory-files (asdf:system-relative-pathname
					"clfm" "fonts/" :type :directory))
	when (string= "ttf" (file-extension f))
	  do (loop for size in '(8 10 12 14 16 18 24 48 72)
		   do (mcclim-truetype::make-truetype-font (find-port) f size))))

(defun set-default-font (text-style-specification)
  (setf *default-text-style* text-style-specification))

(defclass clfm-fs-overview-directory (clfm-object)
  ((directory :initarg :directory-pathname
  	      :accessor directory-pathname
  	     :documentation "the pathname describing a clfm directory")
   (expandedp :initarg :expandedp
  	      :accessor expandedp
  	      :initform nil
  	      :documentation "controlls whether the directory is expanded or collapsed")
   (subdirs :initarg :subdirectories
	    :accessor subdirectories
	    :documentation "a list of clfm-fs-overview-directory objects from this directory")))

(defclass clfm-name-pane (application-pane) ())
(defclass clfm-size-pane (application-pane) ())
(defclass clfm-type-pane (application-pane) ())
(defclass clfm-extension-pane (application-pane) ())
(defclass clfm-modified-date-pane (application-pane) ())
(defclass clfm-user-pane (application-pane) ())
(defclass clfm-group-pane (application-pane) ())
(defclass clfm-user-group-pane (application-pane) ())
(defclass clfm-permissions-pane (application-pane) ())

(define-application-frame CLFM ()
  ((current-directory-and-history
    :initarg :current-dir
    :accessor current-dir
    :initform nil)
   (dialog-passback :initform nil
		    :initarg :dialog-passback
		    :accessor dialog-passback))
  (:menu-bar clfm-menubar)
  (:top-level (clim:default-frame-top-level :prompt #'commander-prompt
					    ;; :command-unparser #'shell-commander
					    ))
  (:panes
   (current-directory :application
    		      :display-function #'display-current-directory
		      :display-time nil)
   ;; I should have current-directory pane be a pane that contains these panes,
   ;; and then we draw in the individual panes. this will allow us to use a
   ;; box-adjuster-gadget to modify how much is visible. OR we can use a variable
   ;; to "shrink" a string and append elipses, so that
   ;; "this super long file name.pdf" becomes "this super lo...pdf", however this
   ;; wont be as ammenable to resizing on the fly... hmmm
   (file-system-overview :application
			 :display-function #'display-file-system)
   (toolbar :application
	    :height 20
	    :display-function #'display-info)
   (commander :interactor
	      :height 20)
   (options :application
	    :display-function #'display-options))
  (:layouts
   (default
    (vertically ()
      (20 toolbar)
      (horizontally ()
	(1/5 file-system-overview)
        (make-pane 'clim-extensions:box-adjuster-gadget)
	(:fill
	 current-directory))
      (make-pane 'clim-extensions:box-adjuster-gadget)
      (75 commander)))
   (options
    (vertically ()
      (20 toolbar)
      (horizontally ()
	(1/5 file-system-overview)
        (make-pane 'clim-extensions:box-adjuster-gadget)
	(:fill
	 current-directory)
	(make-pane 'clim-extensions:box-adjuster-gadget)
	(1/4 options))
      (make-pane 'clim-extensions:box-adjuster-gadget)
      (75 commander)))))

(defun commander-prompt (pane frame)
  (declare (ignore frame))
  (format pane "COMMAND: ")
  (stream-increment-cursor-position pane 5 0))

;; (defun shell-commander (command-table stream command)
;;   (declare (ignore command-table stream))
;;   (uiop:run-program command))

(defun app-main ()
  (initialize-fonts)
  (set-default-font (make-text-style "ETBembo" "RomanOSF" 14))
  (labels ((loadrc (rc-list)
	     (loop for rc in rc-list
		   do (let ((loaded (handler-case (load rc)
				      (sb-int:simple-file-error () nil))))
			(when loaded
			  (return-from loadrc 't))))))
    (loadrc
     '("~/.clfm" "~/.clfmrc" "~/.clfm.d/init.lisp" "~/.config/clfm/clfmrc")))
  (handler-case (run-frame-top-level (make-application-frame 'clfm))
    ;; we need handler case as etbembo doesnt abide by regular bold and italic
    ;; naming
    (mcclim-truetype::missing-font ()
      (setf *default-text-style* *default-text-style-fallback*)
      (run-frame-top-level
       (make-application-frame 'clfm
  			       :current-dir
  			       (list (find-directory-in-filesystem
  				      (namestring (uiop:getcwd)))))))))

(defmethod hidden-file-or-directory-p ((path string))
  (let* ((p path)
	 (l (length p)))
    (uiop:hidden-pathname-p (if (and (string= "/" (subseq p (- l 1)))
				     (not (string= "/./" (subseq p (- l 3)))))
				(subseq p 0 (- l 1))
				p))))

(defmethod hidden-file-or-directory-p ((path pathname))
  (let* ((p (format nil "~a" path))
	 (l (length p)))
    (uiop:hidden-pathname-p (if (and (string= "/" (subseq p (- l 1)))
				     (not (string= "/./" (subseq p (- l 3)))))
				(subseq p 0 (- l 1))
				p))))

(defmethod hidden-file-or-directory-p ((path clfm-object))
  (let* ((p (path path))
	 (l (length p)))
    (uiop:hidden-pathname-p (if (and (string= "/" (subseq p (- l 1)))
				     (not (string= "/./" (subseq p (- l 3)))))
				(subseq p 0 (- l 1))
				p))))

;; (defmethod hidden-file-or-directory-p ((path clfm-file))
;;   (let* ((p (path path))
;; 	 (l (length p)))
;;     (uiop:hidden-pathname-p (if (and (string= "/" (subseq p (- l 1)))
;;  				     (not (string= "/./" (subseq p (- l 3)))))
;;  				(subseq p 0 (- l 1))
;;  				p))))

(defun collect-directory-contents (directory)
  (let* ((directories (uiop:subdirectories directory))
	 (files (uiop:directory-files directory))
	 (together (append directories files)))
    (sort (loop for thing in together
		collect (format nil "~a" thing))
	  #'string<=)))

;;;;;;;;;;;;;;;;;
;;; Menu Bars ;;; 
;;;;;;;;;;;;;;;;;

(make-command-table 'clfm-menubar
		    :errorp nil
		    :menu '(("File" :menu clfm-file-menu)
			    ("View" :menu clfm-view-menu)
			    ("Open" :menu clfm-open-menu)))

(make-command-table 'clfm-file-menu
		    :errorp nil
		    :menu '(("Quit" :command com-quit)))

(make-command-table 'clfm-view-menu
		    :errorp nil
		    :menu '(("Hidden Files"
			     :command com-toggle-show-hidden-files)))

(make-command-table 'clfm-open-menu
		    :errorp nil
		    :menu '(("XDG-OPEN" :menu clfm-xdg-menu)))

(make-command-table 'clfm-xdg-menu
		    :errorp nil
		    :menu '(("Dis/Allow xdg-open"
			     :command com-toggle-allow-xdg-open)
			    ("Prefer/Avoid xdg-open"
			     :command com-toggle-prefer-xdg-open)
			    ("Popup" :command com-set-xdg-open-things)))

;;;;;;;;;;;;;;;;;;;;;
;;; Presentations ;;;
;;;;;;;;;;;;;;;;;;;;;

(define-presentation-type access-file-or-directory ())
(define-presentation-type enter-directory ())
(define-presentation-type access-file ())
(define-presentation-type expand-or-collapse-directory ())

(define-presentation-type access-object-presentation ())

(define-presentation-type toggle-option-presentation ())

;;;;;;;;;;;;;;;;
;;; Commands ;;;
;;;;;;;;;;;;;;;;

(define-clfm-command (com-quit :name "Quit") ()
  (frame-exit *application-frame*))

(define-clfm-command (com-show-options :name "Options") ()
  (let ((layout (frame-current-layout *application-frame*)))
    (if (equal layout 'options)
        (setf (frame-current-layout *application-frame*) 'default)
        (setf (frame-current-layout *application-frame*) 'options))))

(define-clfm-command (com-toggle-show-hidden-files :name "Show/Hide Hidden Files")
    ()
  (setf (hide-files?) (not (hide-files?))))

(define-clfm-command (com-toggle-allow-xdg-open :name "xdg-open allow/deny") ()
  (setf (allow-xdg-open?) (not (allow-xdg-open?))))

(define-clfm-command (com-toggle-prefer-xdg-open :name "xdg-open prefer/avoid") ()
  (setf (prefer-xdg-open?) (not (prefer-xdg-open?))))

(define-clfm-command (com-toggle-option) ((option symbol) (value symbol))
  (let ((canonical-option (assoc option *options*)))
    (setf (cdr canonical-option) (not value))
    (redisplay-frame-pane *application-frame* 'current-directory :force-p t)))

(define-presentation-to-command-translator toggle-option
    (toggle-option-presentation com-toggle-option clfm
     :gesture :select :documentation "Toggle this option")
    (obj)
  (list (car obj) (cdr obj)))

(define-clfm-command (com-run-shell-command :name "Run Shell Command")
    ((shell-command string :prompt "Shell Command"))
  (run-shell-command shell-command)
  (redisplay-frame-panes *application-frame*))

(define-clfm-command (com-up-directory :name "Up")
    ((number number :prompt "Number of Directories"))
  (loop for x from 1 to number
	do (chdir "..")))

(define-clfm-command (com-up-one-directory :name "Up1") ()
  (chdir ".."))

(define-clfm-command (com-handle-directory-contents :name nil) ((thing string))
  (cond ((uiop:directory-pathname-p thing)
	 (chdir thing))
	((uiop:file-exists-p thing)
	 (run-file-associated-program thing))))

(defparameter *viser* nil)

(define-clfm-command (com-change-directory :name "Change Directory")
    ((dir string :prompt "Directory"))
  (uiop:chdir dir)
  (let* ((cwd (uiop:getcwd))
	 (newdir (find-directory-in-filesystem cwd)))
    (setf (current-dir *application-frame*)
	  (cons newdir (current-dir *application-frame*))))
  (redisplay-frame-panes *application-frame* :force-p t))

;; (define-clfm-command (com-change-directory)
;;     ((dir clfm-directory :prompt "Directory"))
;;   (uiop:chdir dir)
;;   (let* ((cwd (uiop:getcwd))
;; 	 (newdir (find-directory-in-filesystem cwd)))
;;     (setf (current-dir *application-frame*)
;; 	  (cons newdir (current-dir *application-frame*))))
;;   (redisplay-frame-panes *application-frame*))

(define-presentation-to-command-translator change-directory-transl
    (access-object-presentation com-change-directory clfm
     :gesture :select
     :tester ((obj) (clfm-directory-p obj))
     :documentation "Change to Directory")
    (obj)
  (list (path obj)))

(define-presentation-to-command-translator change-directory-string-transl
    (access-object-presentation com-change-directory clfm
     :gesture :select
     :tester ((obj) (stringp obj))
     :documentation "Change to Directory")
    (obj)
  (list (path (find-directory-in-filesystem obj))))

(define-clfm-command (com-access-object) ((object t))
  (access-directory-or-file object))

(define-presentation-to-command-translator access-object
    (access-object-presentation com-access-object clfm
     :gesture :select
     :priority -1
     :documentation "Open")
    (o)
  (list o))

(define-clfm-command (com-chdir :name t) ((directory string))
  (chdir directory))

(define-clfm-command (com-chdir-path :name t) ((directory pathname))
  (chdir directory))

(define-presentation-to-command-translator move-to-directory
    (enter-directory com-chdir clfm
     :gesture :select
     :documentation "Change to Directory")
    (obj)
  (list obj))

(define-presentation-to-command-translator move-to-directory
    (access-file-or-directory com-chdir-path clfm
     :gesture :select
     :tester ((object) (or (uiop:directory-exists-p object)
			   ;; (and (not (uiop:file-exists-p object))
			   ;; 	)
			   ))
     :documentation "Change to Directory"
     )
    (object)
  (list object))

(define-clfm-command (com-open-file) ((file string))
  (when (uiop:file-exists-p file)
    (run-file-associated-program file)))

(define-clfm-command (com-open-file-path) ((file pathname))
  (when (uiop:file-exists-p file)
    (run-file-associated-program file)))

(define-presentation-to-command-translator open-file
    (access-file com-open-file clfm
     :gesture :select
     :documentation "Open File")
    (obj)
  (list obj))

(define-presentation-to-command-translator open-file
    (access-file-or-directory com-open-file clfm
     :gesture :select
     :tester ((object) (uiop:file-exists-p object))
     :documentation "Open File")
    (object)
  (list object))

(define-presentation-to-command-translator open-file
    (access-file-or-directory com-open-file-path clfm
     :gesture :select
     :tester ((object) (uiop:file-exists-p object))
     :documentation "Open File")
    (object)
  (list object))

(define-clfm-command (com-trash-file-or-directory) ((file string))
  ;; (uiop:launch-program (format nil "trash '~a'" file))
  (run-shell-command (format nil "trash '~a'" file))
  (redisplay-frame-panes *application-frame*))

(define-presentation-to-command-translator trash-file
    (access-file com-trash-file-or-directory clfm
     :gesture :select
     :priority -1
     :documentation "Trash File")
    (object)
  (list object))

(define-presentation-to-command-translator trash-directory
    (enter-directory com-trash-file-or-directory clfm
     :gesture :select
     :priority -1
     :documentation "Trash Directory")
    (object)
  (list object))

(define-presentation-to-command-translator trash-file
    (access-file-or-directory com-trash-file-or-directory clfm
     :gesture :select
     :priority -1
     :documentation "Trash")
    (object)
  (list object))

(define-clfm-command (com-expand-clfm-directory) ((dir clfm-fs-overview-directory))
  (setf (subdirectories dir)
	(loop for d in (remove-if #'hidden-file-or-directory-p
				  (uiop:subdirectories (path dir)))
	      collect (make-instance 'clfm-fs-overview-directory
				     :path (format nil "~a" d)
				     :expandedp nil
				     :subdirectories nil)))
  (setf (expandedp dir) t))

(define-presentation-to-command-translator expand-directory
    (expand-or-collapse-directory com-expand-clfm-directory clfm
     :gesture :select
     :tester ((dir) (not (expandedp dir)))
     :documentation "Expand Directory")
    (obj)
  (list obj))

(define-clfm-command (com-collapse-clfm-directory) ((directory clfm-fs-overview-directory))
  (labels ((collapse-subdirs (dir)
	     (when (subdirectories dir)
	       (loop for subdir in (subdirectories dir)
		     do (collapse-subdirs subdir))
	       (setf (subdirectories dir) nil))))
    (collapse-subdirs directory))
  (setf (subdirectories directory) nil)
  (setf (expandedp directory) nil))

(define-presentation-to-command-translator collapse-directory
    (expand-or-collapse-directory com-collapse-clfm-directory clfm
     :gesture :select
     :tester ((dir) (expandedp dir))
     :documentation "Collapse Directory")
    (obj)
  (list obj))

(define-presentation-to-command-translator swap-to-directory
    (expand-or-collapse-directory com-change-directory clfm
     :gesture :select
     :priority -1
     :documentation "Change to directory")
    (obj)
  (list (path obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display Methods and Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(labels ((leaf (line)
	   (car (reverse (cl-ppcre:split "/" (if (pathnamep line)
                                                 (namestring line)
						 line))))))
  (defmethod display-cwd-contents ((frame clfm) (pane clfm-name-pane))
    (declare (ignore frame))
    (with-end-of-line-action (pane :scroll)
      (let ((directory-contents (current-directory-contents)))
	(when (hide-files?)
	  (setf directory-contents
		(remove-if #'hidden-file-or-directory-p directory-contents)))
	(loop for line in (cons ".." directory-contents)
	      do (with-output-as-presentation (pane line 'access-file-or-directory
						    :single-box t)
		   (if (and (stringp line) (string= line ".."))
		       (format pane "Up One Directory~&")
		       (format pane "~a~&" (leaf line))))))))

  (defmethod display-cwd-contents ((frame clfm) (pane clfm-size-pane))
    (declare (ignore frame))
    (with-end-of-line-action (pane :scroll)
      (let ((directory-contents (current-directory-contents)))
	(when (hide-files?)
	  (setf directory-contents
		(remove-if #'hidden-file-or-directory-p directory-contents)))
	(loop for line in (cons ".." directory-contents)
	      do (with-output-as-presentation (pane line 'access-file-or-directory
						    :single-box t)
		   (if (and (stringp line) (string= line ".."))
		       (format pane " ~%")
		       (format pane "~a~&"
			       (if (uiop:directory-exists-p line) "DIR"
				   (file-size line))
			       ;; (if (and *display-directory-sizes*
			       ;; 		(uiop:directory-exists-p line))
			       ;; 	   (line))
			       )))))))

  (defmethod display-cwd-contents ((frame clfm) (pane clfm-type-pane))
    (declare (ignore frame))
    (with-end-of-line-action (pane :scroll)
      (let ((directory-contents (current-directory-contents)))
	(when (hide-files?)
	  (setf directory-contents
		(remove-if #'hidden-file-or-directory-p directory-contents)))
	(loop for line in (cons ".." directory-contents)
	      do (with-output-as-presentation (pane line 'access-file-or-directory
						    :single-box t)
		   (if (and (stringp line) (string= line ".."))
		       (format pane " ~%")
		       (format pane "~a~&" (file-type line))))))))

  (defmethod display-cwd-contents ((frame clfm) (pane clfm-extension-pane))
    (with-end-of-line-action (pane :scroll)
      (let ((directory-contents (current-directory-contents)))
	(when (hide-files?)
	  (setf directory-contents
		(remove-if #'hidden-file-or-directory-p directory-contents)))
	(loop for line in (cons ".." directory-contents)
	      do (with-output-as-presentation (pane line 'access-file-or-directory
						    :single-box t)
		   (if (and (stringp line) (string= line ".."))
		       (format pane " ~%")
		       (format pane "~a~&" (file-type line))))))))

  (defmethod display-cwd-contents ((frame clfm) (pane clfm-user-group-pane))
    (with-end-of-line-action (pane :scroll)
      (let ((directory-contents (current-directory-contents)))
	(when (hide-files?)
	  (setf directory-contents
		(remove-if #'hidden-file-or-directory-p directory-contents)))
	(loop for line in (cons ".." directory-contents)
	      do (with-output-as-presentation (pane line 'access-file-or-directory
						    :single-box t)
		   (if (and (stringp line) (string= line ".."))
		       (format pane " ~%")
		       (format pane "~a/~a~&"
			       (run-shell-command (format nil "grep 1000 /etc/passwd | awk -F \":\" '{print $1}'"))
			       (run-shell-command (format nil "groups | awk '{print $1}'")))))))))

  (defmethod display-cwd-contents ((frame clfm) (pane clfm-modified-date-pane))
    (with-end-of-line-action (pane :scroll)
      (let ((directory-contents (current-directory-contents))
	    (epoch (encode-universal-time 0 0 0 1 1 1970 0)))
	(when (hide-files?)
	  (setf directory-contents
		(remove-if #'hidden-file-or-directory-p directory-contents)))
	(loop for line in (cons ".." directory-contents)
	      do (with-output-as-presentation (pane line 'access-file-or-directory
						    :single-box t)
		   (if (and (stringp line) (string= line ".."))
		       (format pane " ~%")
		       (format pane "~a~&"
			       (multiple-value-bind (s min h d m y)
				   (decode-universal-time
				    (+ epoch
				       (osicat-posix:stat-mtime
					(osicat-posix:stat line))))
				 (format nil "~a. ~a. ~a, ~a:~a"
					 d m y h min)))))))))

  (defmethod display-cwd-contents ((frame clfm) (pane clfm-permissions-pane))
    (with-end-of-line-action (pane :scroll)
      (let ((directory-contents (current-directory-contents)))
	(when (hide-files?)
	  (setf directory-contents
		(remove-if #'hidden-file-or-directory-p directory-contents)))
	(loop for line in (cons ".." directory-contents)
	      do (with-output-as-presentation (pane line 'access-file-or-directory
						    :single-box t)
		   (if (and (stringp line) (string= line ".."))
		       (format pane " ~%")
		       (format pane "~a~&"
			       (run-shell-command
				(format nil "stat -c \"%A\" ~a" line))))))))))

(defun old/display-current-directory (frame pane)
  ;; (declare (ignore frame))
  (let ((contents (subdirectories (car (current-dir frame)))))
    (unless contents
      (setf (subdirectories (car (current-dir frame)))
	    (loop for d in (directory-contents (path
						(car (current-dir frame))))
		  collect (cond ((uiop:directory-exists-p d)
				 (make-instance 'clfm-directory
						:path (namestring d)
						:subdirectories nil))
				((uiop:file-exists-p d)
				 (make-instance 'clfm-file
						:path (namestring d)))
				(t
				 "Neither a directory nor a file")))))
    (when (hide-files?)
      (setf contents (remove-if #'hidden-file-or-directory-p contents)))
    (slim:with-table (pane)
      (with-output-as-presentation (pane
				    (concatenate 'string
						 (path
						  (car (current-dir frame)))
						 "..")
				    'access-object-presentation
				    :single-box t)
	(slim:row (slim:cell (format pane "Up One Directory"))))
      (loop for thing in contents
	    do (with-output-as-presentation (pane thing
						  'access-object-presentation
						  :single-box t)
		 (slim:row
		   (slim:cell (format pane "~a" (display-name thing)))))))))

(defun display-current-directory (frame pane)
  (let ((contents (subdirectories (car (current-dir frame)))))
    (unless contents
      (setf (subdirectories (car (current-dir frame)))
	    (loop for d in (directory-contents (path
						(car (current-dir frame))))
		  collect (cond ((uiop:directory-exists-p d)
				 (make-instance 'clfm-directory
						:path (namestring d)
						:subdirectories nil))
				((uiop:file-exists-p d)
				 (make-instance 'clfm-file
						:path (namestring d)))
				(t
				 "Neither a directory nor a file")))))
    (when (hide-files?)
      (setf contents (remove-if #'hidden-file-or-directory-p contents)))
    (slim:with-table (pane)
      (slim:row
	(with-etbembo (pane :italic)
	  (with-output-as-presentation (pane
					(concatenate 'string
						     (path
						      (car (current-dir frame)))
						     "..")
					'access-object-presentation
					:single-box t)
	    (slim:cell (format pane "Up One Directory")))
	  (when-let ((usr (display-user?)))
	    (with-output-as-presentation (pane
					  `(display-user . usr)
					  'toggle-option-presentation
					  :single-box t)
	      (slim:cell (format pane "User"))))
	  (when-let ((grp (display-group?)))
	    (with-output-as-presentation (pane
					  `(display-group . grp)
					  'toggle-option-presentation
					  :single-box t)
	      (slim:cell (format pane "Group"))))
	  (when-let ((perms (display-permissions?)))
	    (with-output-as-presentation (pane
					  `(display-permissions . perms)
					  'toggle-option-presentation
					  :single-box t)
	      (slim:cell (format pane "Permissions"))))))
      (loop for thing in contents
	    do (with-output-as-presentation (pane thing
						  'access-object-presentation
						  :single-box t)
		 (slim:row
		   (slim:cell
		     (with-etbembo (pane :semi-bold)
		       (if (clfm-directory-p thing)
			   (with-drawing-options (pane :ink +blue+)
			     (format pane "D  "))
			   (with-drawing-options (pane :ink +dark-green+)
			     (format pane "F  ")))
		       (format pane "~a" (display-name thing))))
		   (with-etbembo (pane)
		     (when (display-user?)
		       (slim:cell (format pane "~a" (user thing))))
		     (when (display-group?)
		       (slim:cell (format pane "~a" (group thing)))))
		   (when (display-permissions?)
		     (slim:cell (format pane "~a"
					(user-readable-permissions thing))))))))))

(defparameter *filesystem-from-root*
  (loop for directory in (uiop:subdirectories "/")
	collect (make-instance 'clfm-fs-overview-directory
			       :path (format nil "~a" directory)
			       :expandedp nil
			       :subdirectories nil)))

(defun display-file-system (frame pane)
  (declare (ignore frame))
  (labels ((display-directories (directory depth)
	     (with-end-of-line-action (pane :scroll)
	       (if (expandedp directory)
		   (progn
		     (with-output-as-presentation (pane
						   directory
						   'expand-or-collapse-directory)
		       (when  (< 0 depth)
			 (loop for x from 1 to (- depth 1)
			       do (stream-increment-cursor-position pane 10 0))
			 (format pane "↳"))
		       (with-etbembo (pane)
			 (format pane "~a~&" (car
					      (reverse
					       (cl-ppcre:split "/"
							       (path
								directory)))))))
		     (loop for dir in (subdirectories directory)
			   do (display-directories dir (+ depth 1))))
		   (with-output-as-presentation (pane
						 directory
						 'expand-or-collapse-directory)
		     (when  (< 0 depth)
		       (loop for x from 1 to (- depth 1)
			     do (stream-increment-cursor-position pane 10 0))
		       (format pane "↳"))
		     (with-etbembo (pane)
		       (format pane "~a~&" (car
					    (reverse
					     (cl-ppcre:split "/"
							     (path
							      directory)))))))))))
    (multiple-value-bind (x y) (bounding-rectangle-position (sheet-parent pane))
      (loop for dir in *filesystem-from-root*
	    do (display-directories dir 0))
      (scroll-extent pane x y))))

(defun display-info (frame pane)
  (declare (ignore frame))
  (labels ((looper (input &optional ac)
	     (if  input
		  (progn
		    (with-output-as-presentation
			(pane (format nil "~{~a~^/~}/"
				      (append ac (list (car input))))
			      'access-object-presentation
			      :single-box t)
		      (format pane "~a/" (car input)))
		    (looper (cdr input) (append ac (list (car input)))))
		  ;; (with-output-as-presentation (pane "/"))
		  (unless ac
		    (with-output-as-presentation (pane "/"
						       'access-object-presentation
						       :single-box t)
		      (format pane "/"))))))
    (with-etbembo (pane :italic 16)
      (format pane "Current Directory: "))
    (with-etbembo (pane :semi-bold)
      (looper (cl-ppcre:split "/" (namestring (uiop:getcwd)))))))

(defun display-options (frame pane)
  (declare (ignore frame))
  (with-end-of-line-action (pane :scroll)
    (slim:with-table (pane)
      (with-etbembo (pane :bold)
	(slim:row
	  (slim:cell (format pane ""))
	  (slim:cell (format pane "Option"))))
      (loop for option in *options*
	    do (slim:row
		 (with-output-as-presentation (pane option
						    'toggle-option-presentation
						    :single-box t)
		   (slim:cell
		     (when (cdr option)
		       (with-etbembo (pane :bold)
			 (format pane "× "))))
		   (slim:cell
		     (with-etbembo (pane :semi-bold)
		       (format pane "~a" (car option))))))))))
