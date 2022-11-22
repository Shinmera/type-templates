#|
 This file is a part of type-templates
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.type-templates
  (:use #:cl)
  ;; toolkit.lisp
  (:export
   #:format-name
   #:compose-name
   #:enumerate-combinations
   #:return-type
   #:define-type-with-converter)
  ;; type.lisp
  (:export
   #:template-unfulfillable
   #:no-such-place
   #:not-a-template-type
   #:type-instance
   #:template-arguments
   #:constructor
   #:lisp-type
   #:place
   #:place-type
   #:instances
   #:template-type
   #:places
   #:define-type-instance
   #:define-template-type
   #:field
   #:type-alias
   #:define-type-alias)
  ;; template.lisp
  (:export
   #:define-template
   #:do-combinations
   #:define-type-dispatch
   #:define-templated-dispatch
   #:define-alias))
