(in-package #:org.shirakumo.type-templates)

;;; Template mechanism
(defmacro define-template (name &rest args)
  (let ((template-args (loop until (listp (car args))
                             collect (pop args)))
        (macro-name (gensym "NAME")))
    (destructuring-bind (args . body) args
      `(defmacro ,(compose-name #\- 'define name) (,@template-args &optional (,macro-name))
         (flet ((template-unfulfillable ()
                  (error 'template-unfulfillable :template ',name :arguments (list ,@template-args))))
           (declare (ignorable #'template-unfulfillable))
           (let ((body (progn ,@body))
                 (name (or ,macro-name (compose-name #\/ ',name ,@(loop for arg in template-args
                                                                        when (char= #\< (char (string arg) 0))
                                                                        collect arg)))))
             `(progn
                (declaim (ftype (function ,(loop with types = (declared-variable-types body)
                                                 for arg in ',args
                                                 collect (or (second (assoc arg types)) 'T))
                                          ,(declared-return-type body))
                                ,name)
                         ,@(when (find 'inline (declarations body))
                             `((inline ,name))))
                (defun ,name ,',args
                  (declare (optimize speed (safety 0) (debug 0) (compilation-speed 0))
                           ,@(remove 'inline (declarations body)))
                  ,@(remove-if #'declaration-p body)))))))))

(defmacro do-combinations (template &rest argument-combinations)
  (destructuring-bind (template &optional name) (enlist template)
    `(progn ,@(loop for combination in (apply #'enumerate-combinations argument-combinations)
                    when (handler-case (funcall (macro-function template) `(,template ,@combination) NIL)
                           (template-unfulfillable () NIL))
                    collect `(,template ,@combination ,@(if name (list (apply #'format-name name combination))))))))

(defun merge-identical-branches (branches)
  (labels ((recurse (branches)
             (let ((pure ()))
               (loop for (tt . sub) in branches
                     for entry = (assoc tt pure)
                     do (cond (entry
                               ;; Don't merge if the leaf is a branch target and instead
                               ;; prefer the earlier branch target always.
                               (when (consp sub)
                                 (setf (cdr entry) (append (cdr entry) sub))))
                              (T
                               (push (cons tt sub) pure))))
               (setf branches (reverse pure)))
             (loop for cons in branches
                   do (when (consp (cdr cons))
                        (setf (cdr cons) (recurse (cdr cons)))))
             branches))
    (recurse branches)))

(defun duplicate-subtype-branches (branches)
  (labels ((recurse (branches)
             (loop for branch in branches
                   for tt = (car branch)
                   do (loop for (ott . osub) in branches
                            do (when (and (not (eq tt ott))
                                          (subtypep tt ott)
                                          (listp (cdr branch)))
                                 ;; If this is a subtype, clone all branches of the supertype
                                 (setf (cdr (last branch)) (copy-tree osub)))))
             (loop for (tt . sub) in branches
                   do (when (consp sub) (recurse sub)))
             branches))
    (recurse branches)))

(defun emit-type-dispatch (args parts)
  ;; FIXME: This does not work right if the types are not disjoint and multiple tree branches could
  ;;        be taken. Subtype relationships must be respected
  (let ((tree (merge-identical-branches
               (duplicate-subtype-branches
                (prefix-tree
                 (loop for (type rettype . expansion) in parts
                       for i from 0
                       collect (append type i)))))))
    (labels ((emit-dispatch (args types)
               `(etypecase ,(first args)
                  ,@(loop for (type . rest) in types
                          collect `(,type
                                    ,(if (consp rest)
                                         (emit-dispatch (rest args) rest)
                                         (destructuring-bind (rettype . body) (rest (nth rest parts))
                                           (if (eql T rettype)
                                               `(progn ,@body)
                                               `(the (values ,rettype &optional) (progn ,@body))))))))))
      (emit-dispatch args tree))))

(defmacro define-type-dispatch (name args &body expansions)
  (let* ((argvars (lambda-list-variables args))
         (expansions (loop for expansion in expansions
                           for deficit = (- (length argvars) (length (first expansion)))
                           do (when (< deficit 0)
                                (error "Dispatch expansion~%  ~s~%contains more variables than specified in the arglist."
                                       expansion))
                           collect (list* (append (first expansion) (make-list deficit :initial-element 'null))
                                          (rest expansion))))
         (argtypes (loop with i = -1
                         for arg in args
                         collect (cond ((find arg lambda-list-keywords)
                                        arg)
                                       (T
                                        (incf i)
                                        `(or ,@(loop for (args) in expansions collect (nth i args)))))))
         (rettype `(or ,@(loop for (args rettype) in expansions collect rettype))))
    `(progn
       #-sbcl (declaim (inline ,name))
       #-sbcl (declare (ftype (function ,argtypes (values ,rettype &optional)) ,name))
       (defun ,name ,args
         (declare (optimize speed (debug 1) (safety 1) (compilation-speed 0)))
         ,(emit-type-dispatch argvars expansions))
       #+sbcl
       (sb-c:defknown ,name ,argtypes ,rettype
           (sb-c:any)
         :overwrite-fndb-silently T)
       #++
       (sb-c:defoptimizer (,name sb-c:derive-type) (,args)
         (let ,(loop for arg in argvars
                     collect `(,arg (if ,arg (sb-c::lvar-type ,arg) (sb-c::specifier-type 'NULL))))
           (cond ,@(loop for (argtypes rettype) in expansions
                         collect `((and ,@(loop for argtype in argtypes
                                                for arg in argvars
                                                for i from 0
                                                collect `(sb-c::csubtypep ,arg (sb-c::specifier-type ',argtype))))
                                   (sb-c::specifier-type ',rettype)))
                 (T (sb-c::specifier-type T)))))
       ;; NOTE: The defoptimizer isn't needed, SBCL can derive the type on its own just fine.
       #+sbcl
       ,@(loop for (type result . body) in expansions
               ;; FIXME: this is not great. optional placement should be better.
               for opttypes = (remove 'null type)
               collect `(sb-c:deftransform ,name (,args ,opttypes)
                          (dbg "Expanding transform (~a~{ ~a~})" ',name ',opttypes)
                          ',@body)))))

(defmacro define-dependent-dispatch-type (name (type-list i &rest args) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,name (,type-list ,i ,@args)
       (declare (ignorable ,type-list ,i))
       ,@body)))

(defun enumerate-template-type-combinations (types)
  (labels ((expand-type (type)
             (cond ((integerp type)
                    (list type))
                   ((listp type)
                    (if (eql 'function (first type))
                        (lambda (types i)
                          (apply (fdefinition (first (second type))) types i (rest (second type))))
                        (loop for sub in type append (expand-type sub))))
                   ((vectorp type)
                    (list type))
                   ((subtypep type 'template-type)
                    (instances type))
                   (T
                    (list type)))))
    (let ((expanded (apply #'enumerate-combinations (mapcar #'expand-type types))))
      ;; Perform back substitution of positional types
      (dolist (types expanded expanded)
        (loop for cons on types
              for i from 0
              do (typecase (car cons)
                   (integer
                    (when (= (car cons) i)
                      (error "Positional reference cannot refer to itself!"))
                    (setf (car cons) (nth (car cons) types)))
                   (vector
                    (setf (car cons) (nth (aref (car cons) 1) (template-arguments (nth (aref (car cons) 0) types)))))
                   (function
                    (setf (car cons) (funcall (car cons) types i)))))))))

(defun determine-template-arguments (types)
  (remove-duplicates
   (loop for type in types
         when (typep type 'template-type)
         append (template-arguments type))))

(defmacro define-templated-dispatch (name args &body body)
  (form-fiddle:with-body-options (expansions options ignore-template-types) body
    (assert (null options))
    (flet ((full-template-args (types template-args)
             (append (loop for arg in template-args
                           collect (if (vectorp arg)
                                       (nth (aref arg 0) types)
                                       arg))
                     (determine-template-arguments
                      (loop for type in types
                            unless (loop for ignored in ignore-template-types
                                         thereis (eql (type-of type) ignored))
                            collect type)))))
      `(define-type-dispatch ,name ,args
         ,@(loop for (types template . template-args) in expansions
                 append (loop for type in (enumerate-template-type-combinations types)
                              for form = (if (listp template)
                                             `(,(mapcar #'lisp-type type) T
                                               (,(apply #'compose-name #\/ (car template) (full-template-args type (rest template))) ,@template-args))
                                             `(,(mapcar #'lisp-type type) T
                                               (,(apply #'compose-name #\/ template (full-template-args type template-args)) ,@(lambda-list-variables args))))
                              if (fboundp (car (third form)))
                              collect form
                              else do (alexandria:simple-style-warning "Dispatch omitted for ~a as the raw function is undefined."
                                                                       (car (third form)))))))))

;; NOTE: this does not work with &REST as we cannot automatically deal with
;;       conversion or deconversion of variadic arguments as a list in the
;;       plain defun.
(defmacro define-alias (fun args &body expansion)
  (let* ((argvars (lambda-list-variables args))
         (arggens (loop for var in argvars collect (gensym (string var)))))
    `(progn
       (macrolet ((thunk ()
                    (let ,(loop for arg in argvars
                                collect `(,arg ',arg))
                      ,@expansion)))
         (defun ,fun ,args
           ,(form-fiddle:lambda-docstring `(lambda () ,@expansion))
           (thunk)))
       (define-compiler-macro ,fun ,(loop for arg in args
                                          collect (if (listp arg)
                                                      (list (first arg) `',(second arg))
                                                      arg))
         ,(form-fiddle:lambda-docstring `(lambda () ,@expansion))
         `(let ,(list ,@(loop for arg in argvars
                              for gen in arggens
                              collect `(list ',gen ,arg)))
            ,(let ,(loop for arg in argvars
                         for gen in arggens
                         collect `(,arg ',gen))
               ,@expansion))))))
