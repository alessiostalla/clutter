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

(defmethod closer-mop:finalize-inheritance ((class standard-class-with-attributes))
  (call-next-method)
  (setf (attributes-of class)
	(apply #'merge-attributes (remove nil (mapcar #'attributes-of (closer-mop:class-precedence-list class)))))
  class)

(defmethod closer-mop:ensure-class-using-class ((class standard-class-with-attributes) name &rest args &key attributes)
  (declare (ignorable class name args))
  (let ((class (call-next-method)))
    (when attributes
      (setf (attributes-of class) (make-hash-table))
      (dolist (attr attributes)
	(setf (attribute (car attr) class) (cdr attr))))
    class))

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
	  (apply #'merge-attributes (remove nil (mapcar #'attributes-of direct-slot-definitions))))
    result))

(defclass merged-attributes () ((attributes-list :initarg :attributes-list :reader attributes-list)))

(defun merge-attributes (&rest attrs)
  (if attrs
      (make-instance 'merged-attributes :attributes-list attrs)
      (make-hash-table)))

(defgeneric get-attribute (map name))
(defmethod get-attribute ((map hash-table) name)
  (gethash name map))
(defmethod get-attribute ((map merged-attributes) name)
  (do+ ((for attrs (in (attributes-list map))))
    (multiple-value-bind (value present?) (get-attribute attrs name)
      (when present? (return-from get-attribute (values value t)))))
  (values nil nil))

(defgeneric set-attribute (map name value))
(defmethod set-attribute ((map hash-table) name value)
  (setf (gethash name map) value))
(defmethod set-attribute ((map merged-attributes) name value)
  (let ((attrs (car (attributes-list map))))
    (if attrs
	(set-attribute attrs name value)
	(error "No attributes to set"))))

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
