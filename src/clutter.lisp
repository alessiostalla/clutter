(in-package :clutter)

(defclass -with-attributes ()
  ((attributes :initform (make-hash-table) :initarg :attributes :accessor attributes-of))
  (:documentation "Mixin for elements that can have attributes"))

(defmethod attributes-of ((c symbol))
  (attributes-of (find-class c)))

(defmethod attributes-of (anything)
  (declare (ignore anything))
  nil)

(defclass direct-slot-definition-with-attributes (closer-mop:standard-direct-slot-definition -with-attributes) ())
(defclass effective-slot-definition-with-attributes (closer-mop:standard-effective-slot-definition -with-attributes) ())

(defclass standard-class-with-attributes (closer-mop:standard-class -with-attributes) ())

(defmethod closer-mop:validate-superclass ((class standard-class-with-attributes) (superclass standard-class))
  t)

(defmethod closer-mop:finalize-inheritance :after ((class -with-attributes))
  (setf (attributes-of class)
	(apply #'merge-attributes (closer-mop:class-precedence-list class))))

(defun attributes->hash-table (attributes)
  (let ((table (make-hash-table)))
    (dolist (attr attributes)
      (setf (gethash (car attr) table) (cdr attr)))
    table))

(defmethod closer-mop:direct-slot-definition-class ((class standard-class-with-attributes) &rest initargs)
  (declare (ignore initargs))
  (find-class 'direct-slot-definition-with-attributes))

(defmethod closer-mop:effective-slot-definition-class ((class standard-class-with-attributes) &rest initargs)
  (declare (ignore initargs))
  (find-class 'effective-slot-definition-with-attributes))

(defmethod closer-mop:compute-effective-slot-definition ((class standard-class-with-attributes) name direct-slot-definitions)
  (declare (ignorable class name))
  (let ((result (call-next-method)))
    (setf (attributes-of result)
	  (apply #'merge-attributes direct-slot-definitions))
    result))

(defclass merged-attributes ()
  ((own-attributes :initarg :own :reader own-attributes)
   (ancestors :initarg :ancestors :reader ancestors)))

(defun merge-attributes (instance &rest ancestors)
  (make-instance 'merged-attributes
		 :own (let ((attrs (attributes-of instance)))
			(cond ((consp attrs) (attributes->hash-table attrs))
			      ((null attrs) (make-hash-table))
			      (t attrs)))
		 :ancestors ancestors))

(defgeneric get-attribute (map name))
(defmethod get-attribute ((map hash-table) name)
  (gethash name map))
(defmethod get-attribute ((map merged-attributes) name)
  (multiple-value-bind (value present?) (get-attribute (own-attributes map) name)
    (if present?
	(values value present?)
	(do+ ((for attributed (in (ancestors map)))
	      (returning (values nil nil)))
	  (let ((attrs (attributes-of attributed)))
	    (when attrs
	      (multiple-value-bind (value present?) (get-attribute attrs name)
		(when present? (return-from get-attribute (values value present?))))))))))

(defgeneric set-attribute (map name value))
(defmethod set-attribute ((map hash-table) name value)
  (setf (gethash name map) value))
(defmethod set-attribute ((map merged-attributes) name value)
  (set-attribute (own-attributes map) name value))

(defun attribute (name attributed-element)
  (let ((attrs (attributes-of attributed-element)))
    (when attrs (get-attribute attrs name))))

(defun (setf attribute) (value name attributed-element)
  (let ((attrs (attributes-of attributed-element)))
    (if attrs
	(set-attribute attrs name value)
	(error "~A is not an attributed element" attributed-element))))
  
(defun slot (name class)
  (or (find-if (lambda (def) (eq name (closer-mop:slot-definition-name def)))
	       (closer-mop:class-slots (if (symbolp class) (find-class class) class)))
      (error "~A does not name a slot in ~A" name class)))
