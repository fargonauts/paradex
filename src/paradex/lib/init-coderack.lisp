
(defun get-bottom-up-codelets ()
; Adds various bottom-up codelets to *codelets-to-post*, deciding on how
; many to add and urgency, as a function of how much each type of codelet 
; is needed.

  ; Add bottom-up description codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'description))
	   'heads)
   then (loop for i from 1 
	      to (get-num-of-codelets-to-post 'description) do
	      (push (make-codelet 'bottom-up-description-scout nil 
		                  '*low-bin*) *codelets-to-post*)))

  ; Add bottom-up bond codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'bond)) 'heads)
   then (loop for i from 1 to (get-num-of-codelets-to-post 'bond) do
              (push (make-codelet 'bottom-up-bond-scout nil 
		                  '*low-bin*) *codelets-to-post*)))

  ; Add bottom-up group codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'group)) 'heads)
   then (loop for i from 1 to (get-num-of-codelets-to-post 'group) do
              (push (make-codelet 'group-scout--whole-string nil 
		                 '*low-bin*) *codelets-to-post*)))

  ; Add bottom-up replacement codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'replacement)) 
	   'heads)
   then (loop for i from 1 
	      to (get-num-of-codelets-to-post 'replacement) do
              (push (make-codelet 'replacement-finder nil '*low-bin*) 
	            *codelets-to-post*)))

  ; Add bottom-up correspondence codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'correspondence))
	              'heads)
   then (loop for i from 1 
	      to (get-num-of-codelets-to-post 'correspondence) do
              (push (make-codelet 'bottom-up-correspondence-scout nil 
		                  '*low-bin*) *codelets-to-post*)
              (push (make-codelet 'important-object-correspondence-scout nil 
		                  '*low-bin*) *codelets-to-post*)))

  ; Add bottom-up rule codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'rule)) 'heads)
   then (loop for i from 1 to (get-num-of-codelets-to-post 'rule) do
              (push (make-codelet 'rule-scout nil '*low-bin*) 
		    *codelets-to-post*)))

  ; Add bottom-up rule-translator codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'translated-rule)) 
	   'heads)
   then (loop for i from 1 
	      to (get-num-of-codelets-to-post 'translated-rule) do
              (push (make-codelet 'rule-translator nil 
		                  (if* (> *temperature* 25) 
		                   then '*low-bin* else '*high-bin*))
	            *codelets-to-post*)))

  ; Add bottom-up breaker codelets.
  (push (make-codelet 'breaker nil '*extremely-low-bin*) 
	*codelets-to-post*))

;---------------------------------------------

(defun init-coderack ()
; Makes a coderack called *coderack*.
  (setq *coderack* (make-coderack))
  (setq *codelet-list* nil)
  (setq *coderack-initialized* t))

;---------------------------------------------

(defun post-initial-codelets ()
; Post the initial codelets the program starts out with.
  (loop for i from 1 to (* 2 (length (send *workspace* :object-list))) do
        (send *coderack* :post 
	      (make-codelet 'bottom-up-bond-scout nil '*very-low-bin*))
	(send *coderack* :post 
	      (make-codelet 'replacement-finder nil '*very-low-bin*))
	(send *coderack* :post
	      (make-codelet 'bottom-up-correspondence-scout nil 
		            '*very-low-bin*)))

  (send *coderack* :post-codelet-list *codelets-to-post*)
  (setq *codelets-to-post* nil))
  
;---------------------------------------------
