
(in-package :clfm)

(defparameter *default-text-style-fallback*
  (make-text-style :sans-serif :roman :normal))

(defparameter *options*
  '((allow-xdg-open . t)
    (prefer-xdg-open . nil)
    (hide-hidden-files . t)))

(defun prefer-xdg-open? ()
  (cdr (assoc 'prefer-xdg-open *options*)))

(defmethod (setf prefer-xdg-open?) (new-val)
  (let ((var (assoc 'prefer-xdg-open *options*)))
    (setf (cdr var) new-val)))

(defun hide-files? ()
  (cdr (assoc 'hide-hidden-files *options*)))

(defmethod (setf hide-files?) (new-val)
  (let ((hide (assoc 'hide-hidden-files *options*)))
    (setf (cdr hide) new-val)))

(defun allow-xdg-open? ()
  (cdr (assoc 'allow-xdg-open *options*)))

(defmethod (setf allow-xdg-open?) (new-val)
  (let ((var (assoc 'allow-xdg-open *options*)))
    (setf (cdr var) new-val)))

(defparameter *hide-hidden-files* t)

(defparameter *allow-xdg-open* t)

(defparameter *prefer-xdg-open* nil)
