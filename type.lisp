#|
 This file is a part of type-templates
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.type-templates)

(define-condition template-unfulfillable (error)
  ((template :initarg :template)
   (arguments :initarg :arguments))
  (:report (lambda (c s) (format s "The template~%  ~s~%cannot be expanded for the arguments~%  ~s"
                                 (slot-value c 'template)
                                 (slot-value c 'arguments)))))

(define-condition no-such-place (error)
  ((qualifier :initarg :qualifier)
   (type :initarg :type))
  (:report (lambda (c s) (format s "No such place~%  ~s~%on~%  ~s"
                                 (slot-value c 'qualifier)
                                 (slot-value c 'type)))))

(define-condition not-a-template-type (error)
  ((type :initarg :type))
  (:report (lambda (c s) (format s "This is not a template type:~%  ~s"
                                 (slot-value c 'type)))))

;;; Template type mechanism
(defgeneric type-instance (base-type &rest template-args))
(defgeneric template-arguments (template-type))
(defgeneric constructor (template-type))
(defgeneric lisp-type (template-type))
(defgeneric place (template-type qualifier))
(defgeneric instances (template-type))

(defmethod lisp-type (type)
  (check-type type (or symbol list))
  type)

(defclass template-type ()
  ((lisp-type :initarg :lisp-type :initform (error "lisp-type argument missing.") :reader lisp-type)
   (constructor :initarg :constructor :initform (error "constructor argument missing.") :reader constructor)
   (places :initarg :places :initform (error "") :reader places)))

(defmethod print-object ((type template-type) stream)
  (print-unreadable-object (type stream :type T)
    (format stream "~a <~{~a~^ ~}>" (lisp-type type)
            (ignore-errors (template-arguments type)))))

(defmethod type-instance ((type template-type) &rest targs)
  (loop for instance in (instances type)
        do (when (equal targs (template-arguments instance))
             (return instance))
        finally (error "No such type instance of~%  ~a~%with template arguments~%  ~a"
                       (type-of type) targs)))

(defmethod place ((type template-type) qualifier)
  (loop for (names place) in (places type)
        do (when (find qualifier names)
             (return place))
        finally (error 'no-such-place :qualifier qualifier :type type)))

(defmethod place-type ((type template-type) qualifier)
  (loop for (names place type) in (places type)
        do (when (find qualifier names)
             (return type))
        finally (error 'no-such-place :qualifier qualifier :type type)))

(defmethod instances ((type class))
  (instances (allocate-instance type)))

(defmethod instances ((type symbol))
  (instances (find-class type)))

(defmethod type-instance ((base symbol) &rest template-args)
  (let ((class (find-class base)))
    (cond ((subtypep (class-name class) 'template-type)
           (apply #'type-instance (allocate-instance (find-class base)) template-args))
          (T
           (error 'not-a-template-type :type base)))))

(defmacro define-type-instance ((template-type name) &body args)
  `(let ((instance (make-instance ',template-type :lisp-type ',name ,@(loop for arg in args collect `',arg))))
     (setf (instances instance) (list* instance (remove ',name (instances instance) :key #'lisp-type)))
     (defmethod type-instance ((type (eql ',name)) &rest args)
       (declare (ignore args))
       instance)))

(defun emit-template-type (parent name fields template-args)
  (let ((constructor (compose-name NIL '% name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (define-type-instance (,parent ,name)
         :constructor ,(compose-name NIL '% name)
         :places ,(loop for (name type alias) in fields
                        collect `(,alias ,name ,type))
         ,@template-args)

       (export '(,name ,(compose-name #\- name 'copy) ,(compose-name #\- name 'p)
                 ,@(loop for (name) in fields)))
       (declaim (inline ,constructor ,@(mapcar #'first fields)))
       (defstruct (,name (:constructor ,constructor
                             ,(loop for (name type alias) in fields collect name))
                         (:copier ,(compose-name #\- name 'copy))
                         (:predicate ,(compose-name #\- name 'p))
                         (:conc-name NIL))
         ,@(loop for (name type alias) in fields
                 collect `(,name NIL :type ,type)))

       (defmethod print-object ((,name ,name) stream)
         (write (list ',name ,@(loop for field in fields collect `(,(first field) ,name)))
                :stream stream))

       (defmethod make-load-form ((,name ,name) &optional env)
         (declare (ignore env))
         (list ',constructor ,@(loop for field in fields collect `(,(first field) ,name)))))))

(defmacro define-template-type (name template-args name-constructor &body body)
  (let ((fields (gensym "FIELDS"))
        (class (compose-name #\- name 'type)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,class (template-type)
         ((instances :initform () :accessor instances :allocation :class)
          ,@(loop for arg in template-args
                  collect `(,arg :initform (error "template argument missing")
                                 :initarg ,arg
                                 :reader ,arg))))

       (defmethod template-arguments ((,class ,class))
         (list ,@(loop for arg in template-args
                       collect `(,arg ,class))))

       (defmacro ,(compose-name #\- 'define name) ,template-args
         (let ((,fields ()))
           (labels ((field (name &key (type T) alias)
                      (push (list name type alias) ,fields)))
             ,@body
             (emit-template-type ',class ,name-constructor (nreverse ,fields)
                                 (loop for arg in (list ,@template-args)
                                       for temp in ',template-args
                                       collect temp collect arg))))))))

(defclass type-alias (template-type)
  ((instances :initform () :accessor instances :allocation :class)
   (constructor :initform NIL)
   (places :initform ())))

(defmethod template-arguments ((alias type-alias))
  ())

(defmacro define-type-alias (name &rest types)
  (destructuring-bind (type-name &optional (class-name (compose-name #\- name 'type))) (enlist name)
    `(progn
       (defclass ,class-name (type-alias)
         ((instances :initform () :accessor instances :allocation :class)))
       (setf (instances (allocate-instance (find-class ',class-name)))
             (list ,@(loop for type in types collect `(type-instance ',type))))
       (deftype ,type-name ()
         '(or ,@types)))))
