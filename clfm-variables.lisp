
(in-package :clfm)

(defparameter *default-text-style-fallback*
  (make-text-style :serif :roman :normal))

(defparameter *options*
  '((allow-xdg-open . t)
    (prefer-xdg-open . nil)
    (hide-hidden-files . t)
    (display-group . t)
    (display-user . t)
    (display-permissions . t)))

(macrolet ((make-option-functions (name option-name)
	     (let ((value (gensym))
		   (var (gensym)))
	       `(progn
		  (defun ,name ()
		    (cdr (assoc ',option-name *options*)))
		  (defmethod (setf ,name) (,value)
		    (let ((,var (assoc ',option-name *options*)))
		      (setf (cdr ,var) ,value)))))))
  (make-option-functions display-permissions? display-permissions)
  (make-option-functions display-group? display-group)
  (make-option-functions display-user? display-user)
  (make-option-functions prefer-xdg-open? prefer-xdg-open)
  (make-option-functions hide-files? hide-hidden-files)
  (make-option-functions allow-xdg-open? allow-xdg-open))

(defparameter *uid-list* '())

(defparameter *gid-list* '())
