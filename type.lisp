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
   (constructor :initarg :constructor :initform (error "constructor argument missing.") :reader constructor)
   (slots :initarg :slots :initform (error "SLOTS required") :reader slots)))

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

(defmethod slot ((type template-type) qualifier)
  (loop for slot in (slots type)
        do (when (find qualifier (names slot))
             (return slot))
        finally (error 'no-such-slot :qualifier qualifier :type type)))

(defmethod place ((type template-type) qualifier)
  (accessor (slot type qualifier)))

(defmethod place-form ((type template-type) qualifier var)
  (let ((slot (slot type qualifier)))
    (cond ((realized-slot-p slot)
           `(,(accessor slot) ,var))
          ((computed slot)
           (funcall (compile NIL (value slot)) var))
          (T
           (value slot)))))

(defmethod place-type ((type template-type) qualifier)
  (lisp-type (slot type qualifier)))

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
  `(let ((instance (make-instance ',template-type :lisp-type ',name ,@args)))
     (setf (instances instance) (list* instance (remove ',name (instances instance) :key #'lisp-type)))
     (defmethod type-instance ((type (eql ',name)) &rest args)
       (declare (ignore args))
       instance)))

(defun emit-template-type (parent name slots template-args &key print-object make-object)
  (let ((constructor (compose-name NIL '% name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (define-type-instance (,parent ,name)
         :constructor ',(compose-name NIL '% name)
         :slots (list ,@(loop for slot in slots
                              collect `(make-instance 'slot ,@(loop for s in '(names accessor lisp-type value read-only computed)
                                                                    when (slot-boundp slot s)
                                                                    collect (intern (string s) "KEYWORD")
                                                                    when (slot-boundp slot s)
                                                                    collect `',(slot-value slot s)))))
         ,@(loop for arg in template-args collect `',arg))

       (export '(,name ,(compose-name #\- name 'copy) ,(compose-name #\- name 'p) ,@(mapcar #'accessor slots)))
       (declaim (inline ,constructor ,@(enlist (first make-object)) ,@(mapcar #'accessor slots)))
       (defstruct (,name (:constructor ,constructor ,(mapcar #'accessor (remove-if-not #'realized-slot-p slots)))
                         (:copier ,(compose-name #\- name 'copy))
                         (:predicate ,(compose-name #\- name 'p))
                         (:conc-name NIL))
         ,@(loop for slot in slots
                 when (realized-slot-p slot)
                 collect `(,(accessor slot) NIL :type ,(lisp-type slot) :read-only ,(read-only slot))))

       (defmethod print-object ((,name ,name) stream)
         ,(cond (print-object
                 (funcall print-object name 'stream slots))
                (make-object
                 `(write (list ',(first make-object)
                               ,@(loop for slot-name in (second make-object)
                                       for slot = (find slot-name slots :key #'names :test #'member)
                                       collect `(,(accessor slot) ,name)))
                         :stream stream))
                (T
                 `(write (list ',constructor ,@(loop for slot in slots 
                                                     when (realized-slot-p slot)
                                                     collect `(,(accessor slot) ,name)))
                         :stream stream))))

       (defmethod make-load-form ((,name ,name) &optional env)
         (declare (ignore env))
         (list ',constructor ,@(loop for slot in slots
                                     when (realized-slot-p slot)
                                     collect `(,(accessor slot) ,name))))

       ,@(when make-object
           `((defun ,(first make-object) ,(second make-object)
               (,constructor ,@(cddr make-object)))))

       ,@(loop for slot in slots
               unless (realized-slot-p slot)
               collect `(defun ,(accessor slot) (,name)
                          ,@(if (computed slot)
                                (list (funcall (compile NIL (value slot)) name))
                                `((declare (ignore ,name))
                                  (value slot))))
               unless (or (realized-slot-p slot) (read-only slot))
               collect `(defun (setf ,(accessor slot)) (value ,name)
                          (setf ,(funcall (compile NIL (value slot)) name) value))))))

(defmacro define-template-type (name template-args name-constructor &body body)
  (let ((slots (gensym "SLOTS"))
        (class (compose-name #\- name 'type)))
    (form-fiddle:with-body-options (body options) body
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
           (let ((,slots ()))
             (labels ((field (name &rest args &key type alias &allow-other-keys)
                        (remf args :type)
                        (remf args :alias)
                        (push (apply #'make-instance 'slot :accessor name :lisp-type type :names (enlist alias) args)
                              ,slots)))
               ,@body
               (emit-template-type ',class ,name-constructor (nreverse ,slots)
                                   (loop for arg in (list ,@template-args)
                                         for temp in ',template-args
                                         collect temp collect arg)
                                   ,@options))))))))

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
