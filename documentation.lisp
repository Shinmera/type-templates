(in-package #:org.shirakumo.type-templates)

;; toolkit.lisp
(docs:define-docs
  (function format-name
    "")
  
  (function compose-name
    "")
  
  (function enumerate-combinations
    "")
  
  (function return-type
    "")
  
  (function define-type-with-converter
    ""))

;; type.lisp
(docs:define-docs
  (type template-unfulfillable
    "")
  
  (type no-such-slot
    "")
  
  (type not-a-template-type
    "")
  
  (function type-instance
    "")
  
  (function template-arguments
    "")
  
  (function constructor
    "")
  
  (function lisp-type
    "")
  
  (function slots
    "")
  
  (function direct-slots
    "")
  
  (function compute-slots
    "")
  
  (function compute-type-instance-definition
    "")
  
  (type slot
    "")
  
  (function realized-slot-p
    "")
  
  (function place
    "")
  
  (function place-form
    "")
  
  (function place-type
    "")
  
  (function instances
    "")
  
  (function slot
    "")
  
  (function names
    "")
  
  (function accessor
    "")
  
  (function value
    "")
  
  (function read-only
    "")
  
  (type template-type
    "")
  
  (function places
    "")

  (type type-object
    "")
  
  (function define-dependent-dispatch-type
    "")
  
  (function define-type-instance
    "")
  
  (function define-template-type
    "")
  
  (function field
    "")
  
  (type type-alias
    "")
  
  (function define-type-alias
    ""))

;; template.lisp
(docs:define-docs
  (function define-template
    "")
  
  (function do-combinations
    "")
  
  (function do-type-combinations
    "")
  
  (function do-instance-combinations
    "")
  
  (function define-type-dispatch
    "")
  
  (function define-templated-dispatch
    "")
  
  (function define-alias
    "")
  
  (function define-slot-accessor
    ""))
