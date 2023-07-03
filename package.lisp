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
   #:no-such-slot
   #:not-a-template-type
   #:type-instance
   #:template-arguments
   #:constructor
   #:lisp-type
   #:slots
   #:slot
   #:place
   #:place-form
   #:place-type
   #:instances
   #:slot
   #:names
   #:accessor
   #:value
   #:read-only
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
