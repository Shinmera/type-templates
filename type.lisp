(in-package #:org.shirakumo.type-templates)

(define-condition template-unfulfillable (error)
  ((template :initarg :template)
   (arguments :initarg :arguments))
  (:report (lambda (c s) (format s "The template~%  ~s~%cannot be expanded for the arguments~%  ~s"
                                 (slot-value c 'template)
                                 (slot-value c 'arguments)))))

(define-condition no-such-slot (error)
  ((qualifier :initarg :qualifier)
   (type :initarg :type))
  (:report (lambda (c s) (format s "No such slot~%  ~s~%on~%  ~s"
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
(defgeneric slots (template-type))
(defgeneric direct-slots (template-type))
(defgeneric compute-slots (template-type)
  (:method-combination append :most-specific-last))
(defgeneric compute-type-instance-definition (template-type))
(defgeneric slot (template-type qualifier))
(defgeneric place (template-type qualifier))
(defgeneric place-form (template-type qualifier var))
(defgeneric place-type (template-type qualifier))
(defgeneric instances (template-type))

(defmethod lisp-type (type)
  (check-type type (or symbol list))
  type)

(defclass slot ()
  ((names :initarg :names :reader names)
   (accessor :initarg :accessor :reader accessor)
   (lisp-type :initarg :lisp-type :initform T :reader lisp-type)
   (value :initarg :value :reader value)
   (read-only :initarg :read-only :initform NIL :reader read-only)
   (computed :initarg :computed :initform NIL :reader computed)))

(defmethod initialize-instance :after ((slot slot) &key)
  (when (and (slot-boundp slot 'value)
             (not (computed slot)))
    (setf (slot-value slot 'read-only) T)))

(defun realized-slot-p (slot)
  (not (slot-boundp slot 'value)))

(defclass template-type ()
  ((lisp-type :initarg :lisp-type :initform (error "lisp-type argument missing.") :reader lisp-type)
   (parent :initarg :parent :initform NIL :reader parent)
   (constructor :initarg :constructor :initform (error "constructor argument missing.") :reader constructor)
   (slots :initarg :slots :initform () :reader slots)))

(defmethod shared-initialize :after ((type template-type) slots &key)
  (setf (slot-value type 'slots) (compute-slots type)))

(defmethod print-object ((type template-type) stream)
  (print-unreadable-object (type stream :type T)
    (format stream "~a <~{~a~^ ~}>" (lisp-type type)
            (ignore-errors (template-arguments type)))))

(defmethod type-instance ((type template-type) &rest targs)
  (cond ((null targs)
         type)
        (T
         (loop for instance in (instances type)
               do (when (equal targs (template-arguments instance))
                    (return instance))
               finally (error "No such type instance of~%  ~a~%with template arguments~%  ~a"
                              (type-of type) targs)))))

(defmethod slot ((type template-type) qualifier)
  (loop for slot in (slots type)
        do (when (find qualifier (names slot))
             (return slot))
        finally (error 'no-such-slot :qualifier qualifier :type type)))

(defmethod place ((type template-type) qualifier)
  (accessor (slot type qualifier)))

(defmethod place-form ((type template-type) (slot slot) var)
  (cond ((realized-slot-p slot)
         `(,(accessor slot) ,var))
        ((computed slot)
         (funcall (compile NIL (value slot)) var))
        (T
         (value slot))))

(defmethod place-form ((type template-type) qualifier var)
  (let ((slot (slot type qualifier)))
    (place-form type slot var)))

(defmethod place-type ((type template-type) (slot slot))
  (lisp-type slot))

(defmethod place-type ((type template-type) qualifier)
  (lisp-type (slot type qualifier)))

(defmethod template-type ((type template-type))
  type)

(defmethod template-arguments ((type template-type))
  (loop for slot in (template-arguments (class-of type))
        collect (slot-value type slot)))

(defmethod instances ((type class))
  (instances (allocate-instance type)))

(defmethod instances ((type symbol))
  (instances (find-class type)))

(defmethod template-arguments ((name symbol))
  (if (subtypep name 'template-type)
      (template-arguments (find-class name))
      (template-arguments (type-instance name))))

(defmethod template-type ((name symbol))
  (if (subtypep name 'template-type)
      (find-class name)
      (template-type (find-class name))))

(defmethod type-instance ((base symbol) &rest template-args)
  (let ((class (find-class base)))
    (cond ((subtypep (class-name class) 'template-type)
           (apply #'type-instance (allocate-instance (find-class base)) template-args))
          (T
           (error 'not-a-template-type :type base)))))

(defstruct (type-object
            (:constructor NIL)
            (:copier NIL)
            (:predicate NIL)))

(defmethod template-type ((object type-object))
  (template-type (class-of object)))

(defmethod type-instance ((object type-object) &rest args)
  (apply #'type-instance (class-of object) args))

(defmethod template-arguments ((object type-object))
  (template-arguments (type-instance object)))

(defmethod compute-type-instance-definition ((type template-type))
  (let ((name (lisp-type type))
        (include (parent type))
        (constructor (constructor type))
        (slots (direct-slots type)))
    `(progn
       (export '(,name ,(compose-name #\- name 'copy) ,(compose-name #\- name 'p) ,@(mapcar #'accessor slots)))
       (declaim (inline ,constructor ,@(mapcar #'accessor slots)))
       (defstruct (,name 
                   (:include ,(or include 'type-object))
                   (:constructor ,constructor ,(mapcar #'accessor (remove-if-not #'realized-slot-p (slots type))))
                   (:copier ,(compose-name #\- name 'copy))
                   (:predicate ,(compose-name #\- name 'p))
                   (:conc-name NIL))
         ,@(loop for slot in slots
                 when (realized-slot-p slot)
                 collect `(,(accessor slot) NIL :type ,(lisp-type slot) :read-only ,(read-only slot))))

       (defmethod template-type ((,name (eql (find-class ',name))))
         (find-class ',(type-of type)))

       (defmethod type-instance ((,name (eql (find-class ',name))) &rest args)
         (declare (ignore args))
         (type-instance ',(type-of type) ,@(loop for arg in (template-arguments type) collect `',arg)))

       (defmethod make-load-form ((,name ,name) &optional env)
         (declare (ignore env))
         (list ',constructor ,@(loop for slot in slots
                                     when (realized-slot-p slot)
                                     collect `(,(accessor slot) ,name))))

       ,@(loop for slot in slots
               unless (realized-slot-p slot)
               collect `(defun ,(accessor slot) (,name)
                          ,@(if (computed slot)
                                (list (funcall (compile NIL (value slot)) name))
                                `((declare (ignore ,name))
                                  ,(value slot))))
               unless (or (realized-slot-p slot) (read-only slot))
               collect `(defun (setf ,(accessor slot)) (value ,name)
                          (setf ,(funcall (compile NIL (value slot)) name) value))))))

(defmacro define-type-instance-struct (template-type type-name)
  (let ((type (or (find type-name (instances template-type) :key #'lisp-type)
                  (error "No instance of ~s known with name ~s" template-type type-name))))
    (compute-type-instance-definition type)))

(defmacro define-type-instance ((template-type name) &body args)
  (let ((instance (apply #'make-instance template-type :lisp-type name args)))
    (setf (instances instance) (list* instance (remove name (instances instance) :key #'lisp-type))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((instance (make-instance ',template-type :lisp-type ',name ,@(loop for arg in args collect `',arg))))
       (setf (instances instance) (list* instance (remove ',name (instances instance) :key #'lisp-type)))
       (defmethod type-instance ((type (eql ',name)) &rest args)
         (declare (ignore args))
         instance))

     (define-type-instance-struct ,template-type ,name)))

(defun emit-template-type (parent name template-args include)
  (let ((constructor (compose-name NIL '% name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (define-type-instance (,parent ,name)
         :constructor ,constructor
         :parent ,include
         ,@template-args))))

(defmacro define-template-type (name template-args name-constructor &body body)
  (let ((slots (gensym "SLOTS"))
        (class (compose-name #\- name 'type)))
    (form-fiddle:with-body-options (body options include) body
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,class (,(if include (first include) 'template-type))
           ((instances :initform () :accessor instances :allocation :class)
            ,@(loop for arg in (rest include)
                    for i from 0
                    when (constantp arg)
                    collect `(,(nth i (template-arguments (find-class (first include))))
                              :initform ,arg))
            ,@(loop for arg in template-args
                    collect `(,(or (when include (find arg (template-arguments (find-class (first include))) :test #'string=))
                                   arg)
                              :initform (error ,(format NIL "Template argument ~s missing" arg))
                              :initarg ,arg
                              :reader ,arg))))

         (defmethod template-arguments ((,class (eql (find-class ',class))))
           ',template-args)
         
         (flet ((,slots (,class)
                  (let ((,slots ())
                        ,@(loop for arg in template-args
                                collect `(,arg (,arg ,class))))
                    (labels ((field (name &rest args &key type alias &allow-other-keys)
                               (remf args :type)
                               (remf args :alias)
                               (push (apply #'make-instance 'slot :accessor name :lisp-type type :names (enlist alias) args)
                                     ,slots)))
                      ,@body
                      (nreverse ,slots)))))
           (defmethod direct-slots ((,class ,class))
             (,slots ,class))
           
           (defmethod compute-slots append ((,class ,class))
             (,slots ,class)))

         (defmacro ,(compose-name #\- 'define name) ,template-args
           (emit-template-type ',class ,name-constructor
                               (loop for arg in (list ,@template-args)
                                     for temp in ',template-args
                                     collect temp collect arg)
                               ,(if include
                                    `(lisp-type (type-instance ',(first include) ,@(rest include)))
                                    NIL)
                               ,@options))))))

(defclass type-alias (template-type)
  ((instances :initform () :accessor instances :allocation :class)
   (constructor :initform NIL)
   (slots :initform ())))

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
