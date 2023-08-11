(in-package #:org.shirakumo.type-templates)

#+type-templates-debug
(defun dbg (format &rest values)
  (format *debug-io* "~&TEMPLATES: ~?~%" format values))
#-type-templates-debug
(defun dbg (format &rest values)
  (declare (ignore format values))
  NIL)

(defun enlist (list-ish &rest els)
  (if (listp list-ish) list-ish (list* list-ish els)))

(defun format-name (format &rest args)
  (let ((str (format NIL "~?" format args)))
    (intern
     (ecase (readtable-case *readtable*)
       (:upcase (string-upcase str))
       (:downcase (string-downcase str))
       (:preserve str)
       (:invert (loop for i from 0 below (length str)
                      for char = (char str i)
                      do (setf (char str i) (if (upper-case-p char) (char-downcase char) (char-upcase char)))
                      finally (return str)))))))

(defun compose-name (separator &rest parts)
  (let ((separator (string separator)))
    (intern
     (with-output-to-string (out)
       (flet ((s (a)
                (let ((s (typecase a
                           (string a)
                           (symbol (symbol-name a))
                           (T (princ-to-string a)))))
                  (write-string s out))))
         (s (first parts))
         (loop for part in (rest parts)
               do (when separator (write-string separator out))
                  (s part)))))))

(defun enumerate-combinations (&rest combinations)
  (if (cdr combinations)
      (loop for comb in (enlist (first combinations))
            nconc (loop for rest in (apply #'enumerate-combinations (rest combinations))
                        collect (list* comb rest)))
      (loop for comb in (enlist (first combinations))
            collect (list comb))))

(defun prefix-tree (combinations)
  (let ((table (make-hash-table :test 'eql)))
    (loop for (car . cdr) in combinations
          do (if (consp cdr)
                 (push cdr (gethash car table))
                 (setf (gethash car table) cdr)))
    (loop for key being the hash-keys of table
          for combinations being the hash-values of table
          collect (list* key (if (listp combinations)
                                 (prefix-tree combinations)
                                 combinations)))))

(defun declaration-p (thing)
  (and (listp thing) (eql 'declare (car thing))))

(defun declarations (forms)
  (loop for form = (pop forms)
        while (declaration-p form)
        append (rest form)))

(defun declared-variable-types (forms)
  (loop for declaration in (declarations forms)
        when (and (listp declaration) (eql 'type (first declaration)))
        append (loop with type = (second declaration)
                     for arg in (rest declaration)
                     collect (list arg type))))

(defun declared-return-type (forms)
  (loop for declaration in (declarations forms)
        when (and (listp declaration) (eql 'return-type (first declaration)))
        return (second declaration)
        finally (return T)))

(declaim (declaration return-type))

(defun lambda-list-variables (arglist)
  (loop for arg in arglist
        unless (find arg LAMBDA-LIST-KEYWORDS)
        collect (if (listp arg) (car arg) arg)))

(defmacro define-type-with-converter (name base-type (value) &body conversion)
  (let ((valueg (gensym "VALUE")))
    `(progn
       (deftype ,name (&rest args)
         (if args
             (list* ',base-type (mapcar #',name args))
             ',base-type))
       
       (declaim (ftype (function (T) ,base-type) ,name))
       (defun ,name (,value)
         (declare (optimize speed (debug 1) (safety 1) (compilation-speed 0)))
         ,@conversion)

       (define-compiler-macro ,name (,valueg &environment env)
         (if (constantp ,valueg env)
             `(load-time-value
               (let ((,',value ,,valueg))
                 ,@',conversion))
             `(let ((,',value ,,valueg))
                ,@',conversion))))))

(defun union* (&rest lists)
  (let ((result ()))
    (dolist (list lists (nreverse result))
      (dolist (element list)
        (pushnew element result)))))
