
;(defflavor codelet
;  (codelet-type ; E.g., bottom-up-bond-scout.
;   arguments 
;   urgency-bin ; E.g., %very-high-bin%
;   (index-in-bin nil) ; This codelet's position in its urgency bin.
;   (time-stamp nil) ; The time (in codelet-steps) when this codelet was 
;                    ; posted
;   structure-category) ; E.g., 'bond.
;  ()
;  :gettable-instance-variables
;  :settable-instance-variables
;  :initable-instance-variables)

;(defmethod (codelet :print-to-output-file) (output-file)
;  (with-open-file 
;      (ostream output-file :direction :output 
;	  :if-exists :append :if-does-not-exist :create)    
;  (format ostream "codelet-type: ~a~&" codelet-type)))
;
;;---------------------------------------------
;
;(defmethod (codelet :print) ()
;  (format t "codelet-type: ~a, arguments: ~a" codelet-type arguments)
;  (format t " urgency-bin ~a, time-stamp ~a,~&" 
;	  (send urgency-bin :pname) time-stamp)
;  (format t "~%"))
;
;;---------------------------------------------
;
;(defmethod (codelet :run) ()
;; This is the method that runs the codelet.    
;  (apply codelet-type arguments))
;
;;---------------------------------------------
;
;(defmethod (codelet :remove-probability) ()
;; Returns the probability of removing this codelet from the coderack
;; (a function of the codelet's urgency and age).
;; The 1+ allows some probability for codelets with the highest urgency
;; to be removed.
;  (* (- *codelet-count* time-stamp) 
;     (1+ (- (send *extremely-high-bin* :urgency-value)
;	    (send urgency-bin :urgency-value)))))
;
;;---------------------------------------------
;
;(defun make-codelet (codelet-type arguments urgency-bin-name 
;	             &optional structure-category)
;; Returns a new codelet.
;  (make-instance 'codelet 
;      :codelet-type codelet-type
;      :arguments arguments
;      :urgency-bin (eval urgency-bin-name)
;      :structure-category structure-category))
;      
;;---------------------------------------------
