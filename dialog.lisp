
;; (defpackage #:clfm.dialog
;;   (:use #:clim #:clim-lisp #:clim-tab-layout #:clim-extensions)
;;   (:export "make-dialog" ;; "with-large-text"
;; 	   ))

;; (in-package #:clfm.dialog)

;; (defmacro with-large-text ((pane &optional (size 48)) &body body)
;;   (with-text-style (pane (make-text-style (f)))))

(defmacro make-dialog ((frame-name pane-name &body display-function-body)
		       &optional (buttons '(("Yes" t t)
					    ("No" nil t))))
  (alexandria:with-gensyms (localbind name display display-function)
    `(let (,localbind)
       (labels ((make-button (title return quit)
		  (let ((,pane-name
			  (find-pane-named *application-frame* ',display)))
		    (make-pane 'push-button
			       :label title
			       :activate-callback
			       (lambda (&rest ignore)
				 (declare (ignore ignore))
				 (progn (setf ,localbind (eval return))
					(when quit
					  (frame-exit *application-frame*)))))))
		(,display-function (,frame-name ,pane-name)
		  ,@display-function-body))
	 (define-application-frame ,name () ()
	   (:panes (,display :application
			     :display-function #',display-function))
	   (:layouts
	    (default
	     (vertically ()
               ,display
	       (tabling ()
		 (loop for (title return quit) in ',buttons
		 	     collect (make-button title return quit)))))))
	 (run-frame-top-level
	  (make-application-frame ',name))
	 ,localbind))))

(defmacro test/make-dialog ((frame-name pane-name &body display-function-body)
			    &optional (buttons '(("Yes" t t) ("No" nil t))))
  (alexandria:with-gensyms (localbind name display display-function temp-name)
    `(let (,localbind)
       (labels ((make-button (title return quit)
		  (make-pane 'push-button
			     :label title
			     :activate-callback
			     (lambda (&rest ignore)
			       (declare (ignore ignore))
			       (let ((,temp-name
				       (find-pane-named *application-frame*
							',display)))
				 ;; (setf ,localbind (eval return))
				 (setf ,localbind
				       (eval `(let ((,',pane-name ,,temp-name
						      ;; (find-pane-named
						      ;;  *application-frame*
						      ;;  ',',display)
						      ))
						,return)))
				 (redisplay-frame-pane *application-frame*
						       ,temp-name)
				 (when quit (frame-exit *application-frame*))))))
		(,display-function (,frame-name ,pane-name)
		  ,@display-function-body))
	 (define-application-frame ,name () ()
	   (:panes (,display :application
			     :display-function #',display-function))
	   (:layouts
	    (default
	     (vertically ()
               ,display
	       (tabling ()
		 (loop for (title return quit) in ',buttons
		       collect ;; `(let ((,',pane-name (find-pane-named
			       ;; 			     *application-frame*
			       ;; 			     ,',display)))
			       ;; 	  (make-pane 'push-button
			       ;; 		     :label ,title
			       ;; 		     :activate-callback
			       ;; 		     ))
		       (make-button title return quit)))))))
	 (run-frame-top-level
	  (make-application-frame ',name))
	 ,localbind))))


