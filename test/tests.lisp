(defpackage clutter/tests
  (:use :cl :clutter :rove))
(in-package :clutter/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :clutter)' in your Lisp.

(deftest attributes-basic
  (testing "Classes can have attributes"
    (defclass test-class-1 () ()
      (:metaclass standard-class-with-attributes)
      (:attributes (pretty-name . "A test class") (history "Created class")))
    (closer-mop:finalize-inheritance (find-class 'test-class-1))
    (ok (string= (attribute 'pretty-name 'test-class-1) "A test class"))
    (ok (equal (attribute 'history 'test-class-1) '("Created class"))))
  (testing "Slots can have attributes"
    (defclass test-class-2 ()
      ((a :attributes ((pretty-name . "The A slot") (history "Added slot")))
       (b :attributes ((pretty-name . "The B slot") (history "Added slot") (another . attribute))))
      (:metaclass standard-class-with-attributes))
    (closer-mop:finalize-inheritance (find-class 'test-class-2))
    (ok (string= (attribute 'pretty-name (slot 'a 'test-class-2)) "The A slot"))
    (ok (equal (attribute 'history (slot 'a 'test-class-2)) '("Added slot")))
    (ok (string= (attribute 'pretty-name (slot 'b 'test-class-2)) "The B slot"))
    (ok (equal (attribute 'history (slot 'b 'test-class-2)) '("Added slot")))
    (ok (eq (attribute 'another (slot 'b 'test-class-2)) 'attribute))))

(deftest attributes-redefinition
  (testing "We can redefine attributes on classes"
    (defclass test-class-3 () ()
      (:metaclass standard-class-with-attributes)
      (:attributes (pretty-name . "A test class") (history "Created class") (to-remove t)))
    (closer-mop:finalize-inheritance (find-class 'test-class-3))
    (defclass test-class-3 () ()
      (:metaclass standard-class-with-attributes)
      (:attributes (pretty-name . "A test class") (history "Redefined class" "Created class")))
    (closer-mop:finalize-inheritance (find-class 'test-class-3))
    (ok (string= (attribute 'pretty-name 'test-class-3) "A test class"))
    (ok (equal (attribute 'history 'test-class-3) '("Redefined class" "Created class")))
    (ok (null (attribute 'to-remove 'test-class-3))))
  (testing "We can redefine attributes on slots"
    (defclass test-class-4 ()
      ((a :attributes ((pretty-name . "The A slot") (history "Added slot")))
       (b :attributes ((pretty-name . "The B slot") (history "Added slot") (another . attribute))))
      (:metaclass standard-class-with-attributes))
    (closer-mop:finalize-inheritance (find-class 'test-class-4))
    (defclass test-class-4 ()
      ((a :attributes ((pretty-name . "The A slot") (history "Redefined A" "Added slot") (another . attribute)))
       (b :attributes ((pretty-name . "The B slot") (history "Redefined B" "Added slot"))))
      (:metaclass standard-class-with-attributes))
    (closer-mop:finalize-inheritance (find-class 'test-class-4))
    (ok (string= (attribute 'pretty-name (slot 'a 'test-class-4)) "The A slot"))
    (ok (equal (attribute 'history (slot 'a 'test-class-4)) '("Redefined A" "Added slot")))
    (ok (string= (attribute 'pretty-name (slot 'b 'test-class-4)) "The B slot"))
    (ok (equal (attribute 'history (slot 'b 'test-class-4)) '("Redefined B" "Added slot")))
    (ok (eq (attribute 'another (slot 'a 'test-class-4)) 'attribute))))

(deftest attributes-inheritance
  (testing "Inherited attributes in classes"
    (defclass test-class-5 () ()
      (:metaclass standard-class-with-attributes)
      (:attributes (pretty-name . "A test class") (history "Created class") (truth . t) (common . 1)))
    (defclass test-class-6 () ()
      (:metaclass standard-class-with-attributes)
      (:attributes (pretty-name . "A test class") (history "Created class") (falsity) (common . 2)))
    (defclass test-class-7 (test-class-5 test-class-6) ()
      (:metaclass standard-class-with-attributes)
      (:attributes (pretty-name . "A test subclass") (history "Created class by inheritance") (another . attr)))
    (closer-mop:finalize-inheritance (find-class 'test-class-7))
    (ok (string= (attribute 'pretty-name 'test-class-7) "A test subclass"))
    (ok (equal (attribute 'history 'test-class-7) '("Created class by inheritance")))
    (ok (eq (attribute 'truth 'test-class-7) t))
    (ok (eq (attribute 'falsity 'test-class-7) nil))
    (ok (= (attribute 'common 'test-class-7) 1))
    (ok (eq (attribute 'another 'test-class-7) 'attr)))
  (testing "Inherited attributes in slots"
    :todo))
