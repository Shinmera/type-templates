(in-package #:org.shirakumo.type-templates)

;; toolkit.lisp
(docs:define-docs
  (function format-name
    "Format a new symbol.

The symbol will be interned in the current package.
The name is case-shuffled to the current readtable case after
formatting.

See CL:*PACKAGE*
See CL:FORMAT
See COMPOSE-NAME")
  
  (function compose-name
    "Compose a symbol out of parts.

The symbol will be interned in the current package.
Separator should be a string designator that will be injected between
each part. Each part otherwise is added to the symbol name via
PRINC-TO-STRING.

See CL:*PACKAGE*
See FORMAT-NAME")
  
  (function enumerate-combinations
    "Returns a list of all possible permutations of the given sets of items.

Eg: (enumerate-combinations '(a b) '(1 2))
  => ((a 1) (a 2) (b 1) (b 2))")
  
  (declaration return-type
    "A declaration used to define the return type of a templated function.")
  
  (function define-type-with-converter
    "Define an alias for a longer lisp type with a conversion function to coerce values to fit within that type.

BASE-TYPE should be the type this is an alias for, and CONVERSION the
body of the function used to coerce any value to the requested type."))

;; type.lisp
(docs:define-docs
  (type template-unfulfillable
    "Error signalled to notify that the template cannot be expanded for the given arguments.

This is also available as a local function of the same name within
DEFINE-TEMPLATE, to allow you to easily signal the condition.

See DEFINE-TEMPLATE")
  
  (type no-such-slot
    "Error signalled when trying to fetch a slot that does not exist.

See SLOT")

  (type no-such-instance
    "Error signalled when trying to fetch a type instance that does not exist.

See TEMPLATE-TYPE (type)")
  
  (type not-a-template-type
    "Error signalled when trying to coerce a type that does not name a template-type.

See TEMPLATE-TYPE (type)")
  
  (function type-instance
    "Returns an instance of the given template type that fulfils the given template arguments.

If no such instance exists, an error of type NO-SUCH-INSTANCE is
signalled.

See NO-SUCH-INSTANCE
See TEMPLATE-TYPE (type)")
  
  (function template-arguments
    "Returns the arguments of the template instance or template type.

See TEMPLATE-TYPE (type)")
  
  (function constructor
    "Returns the name of the constructor function for the type instance.

See TEMPLATE-TYPE (type)")
  
  (function lisp-type
    "Returns the name of the lisp-type for the type instance.

See TEMPLATE-TYPE (type)")
  
  (function slots
    "Returns the list of SLOT instances for the type instance.

This returns all slots, including inherited ones.

See SLOT (type)
See DIRECT-SLOTS
See TEMPLATE-TYPE (type)")
  
  (function direct-slots
    "Returns the list of SLOT instances for the type instance.

This only returns slots that were defined directly for this instance,
excluding inherited slots.

See SLOT (type)
See SLOTS
See TEMPLATE-TYPE (type)")
  
  (function compute-slots
    "Returns a list of all SLOT instances for the type instance.

This returns all slots, including inherited ones.

See SLOT (type)
See SLOTS
See TEMPLATE-TYPE (type)")
  
  (function compute-type-instance-definition
    "Computes the form used to define a type instance of the template type.

Users may add methods to this that include the results of
CALL-NEXT-METHOD in their return value, in order to append extra
definitions, such as for PRINT-OBJECT.

See TEMPLATE-TYPE (type)")
  
  (type slot
    "Represents the metadata of a slot on a template type.

See TEMPLATE-TYPE
See NAMES
See ACCESSOR
See LISP-TYPE
See VALUE
See READ-ONLY
See COMPUTED
See REALIZED-SLOT-P")
  
  (function realized-slot-p
    "Returns true if the slot is realised on the structure and holds a value, and isn't constant.

See SLOT (type)")
  
  (function place
    "Returns the name of the place for the given slot.

QUALIFIER can either be a SLOT instance or the name of a slot on the
TEMPLATE-TYPE.

See SLOT
See PLACE-FORM
See TEMPLATE-TYPE (type)")
  
  (function place-form
    "Returns an accessor form of the place for the given slot.

This form can be used to read or SETF the value off the slot on the
instance bound to the symbol passed in VAR.

See SLOT
See PLACE
See TEMPLATE-TYPE (type)")
  
  (function place-type
    "Returns the lisp-type of the value stored in the slot.

See SLOT
See PLACE
See TEMPLATE-TYPE (type)")
  
  (function instances
    "Returns a list of all instances of the template type.

See TEMPLATE-TYPE (type)")
  
  (function slot
    "Find a slot with the given qualifier on the type instance.

If the slot does not exist, an error of type NO-SUCH-SLOT is
signalled.

See NO-SUCH-SLOT (type)
See SLOT (type)
See TEMPLATE-TYPE (type)")
  
  (function names
    "Returns the list of alternate names for the slot.

See SLOT (type)")
  
  (function accessor
    "Returns the name of the accessor function for the slot.

See SLOT (type)")
  
  (function value
    "Returns the constant value form for the slot.

If the slot is realised and does not have a constant value, an error
is signalled.

See SLOT (type)
See REALIZED-SLOT-P")
  
  (function read-only
    "Returns true if the slot can only be read, but not set.

See SLOT (type)")

  (function template-type
    "Returns the TEMPLATE-TYPE named by the given symbol.

See TEMPLATE-TYPE (type)")
  
  (type template-type
    "Represents an instantiable type definition.

See LISP-TYPE
See PARENT
See CONSTRUCTOR
See SLOTS
See INSTANCES
See TYPE-INSTANCE
See SLOT
See PLACE
See PLACE-FORM
See PLACE-TYPE
See TEMPLATE-ARGUMENTS
See DEFINE-TEMPLATE-TYPE
See DEFINE-TYPE-INSTANCE")
  
  (type type-object
    "Supertype for all structures derived from template-types.

See TEMPLATE-TYPE (type)
See TEMPLATE-TYPE
See TYPE-INSTANCE
See TEMPLATE-ARGUMENTS")
  
  (function define-type-instance
    "Define an instance of a template type using the specified name and template arguments.

See COMPUTE-TYPE-INSTANCE-DEFINITION
See TEMPLATE-TYPE (type)")
  
  (function define-template-type
    "Define a new template type.

NAME will be concatenated together with -TYPE and interned into the
local package to produce the name of the template type.
NAME will also be concatenated together with DEFINE- to name a macro
used to define instances of the template type.

TEMPLATE-ARGS should be a list of arguments the template accepts.
NAME-CONSTRUCTOR should be a form that, when evaluated, returns a
symbol naming the type instance for the given template arguments.

BODY may accept the following keyword arguments:
:INCLUDE  --- Specify another template-type to use as the supertype.
   This is done via a list that should be composed out of the
   template-type name and the template arguments to supply for that
   supertype. You may use the name of local template arguments.

The rest of the BODY should be a number of forms. The forms are
evaluated in an environment where the FIELD function is bound and
should use it to emit information about the slots on the resulting
type instance.

See FIELD
See TEMPLATE-TYPE (type)
See DEFINE-TYPE-INSTANCE")
  
  (function field
    "Function used to emit a slot of a template type instance.

The function has the following signature:
  (NAME &key TYPE ALIAS ACCESSOR VALUE READ-ONLY COMPUTED)

NAME      --- The name of the slot
TYPE      --- The lisp-type of the slot's value (defaults to T)
ALIAS     --- A list of alias names (defaults to NIL)
ACCESSOR  --- The name of the accessor for the slot (defaults to NAME)
VALUE     --- A form for the constant value of the slot. If COMPUTED
  is true, this should be a lambda form that is a function of one
  argument, the name of a variable, and returns the form to use to
  access the value of the slot. If COMPUTED is NIL, then this should
  be a constant expression filling the slot's value
READ-ONLY --- If true, the slot can only be read, not set
COMPUTED  --- See above

This function is local to the body off DEFINE-TEMPLATE-TYPE.

See DEFINE-TEMPLATE-TYPE")
  
  (type type-alias
    "Representation of an alias of another template-type.

See TEMPLATE-TYPE (type)
See DEFINE-TYPE-ALIAS")
  
  (function define-type-alias
    "Define an alias for a number of template type instances.

NAME will name a lisp-type of NAME.
NAME will also be concatenated together with -TYPE to name a
TEMPLATE-TYPE that holds the type instances.

See TYPE-ALIAS"))

;; template.lisp
(docs:define-docs
  (function define-template
    "Define a new template function.

NAME will be used as the prefix for all emitted template instances.
NAME will be concatenated together with DEFINE- to name the macro that
is used to emit template instances.

The structure of a DEFINE-TEMPLATE use should be as follows:

  (define-template NAME TEMPLATE-ARGUMENT+ INSTANCE-LAMBDA-LIST BODY*)

TEMPLATE-ARGUMENTS should be symbols naming the variables bound to the
template arguments during BODY. INSTANCE-LAMBDA-LIST should be the
lambda-list of an emitted function instance. BODY should be any number
of forms which return a list of body forms to emit into the resulting
function instance.

The first of those forms may be a DECLARE expression, with the
following special declarations:

  TYPE         --- Used to declare types of the function instance's
    arguments. Together with the RETURN-TYPE these will be used to
    create an FTYPE declaration for the function instance.
  RETURN-TYPE  --- The function instance will declare this as the type
    of the return value.
  INLINE       --- The function instance will be declared inline

The resulting definition macro will take the template arguments as
arguments as well as an optional name for the resulting function
instance. If no name is given, the name is automatically composed out
off the template name and the template arguments, separated by a
slash.

A local function is bound during evaluation of BODY named
TEMPLATE-UNFULFILLABLE. When called, a TEMPLATE-UNFULFILLABLE error is
signalled to signify that the given template arguments are not a valid
combination.

See COMPOSE-NAME
See DEFINE-TEMPLATED-DISPATCH")
  
  (function do-combinations
    "Expand all possible combinations of arguments into forms.

This will try to expand into all permutations off template arguments
for the given template-type for which the template does not signal a
TEMPLATE-UNFULFILLABLE error.

See DEFINE-TEMPLATE
See TEMPLATE-UNFULFILLABLE
See ENUMERATE-COMBINATIONS")
  
  (function do-type-combinations
    "Expand all possible type arguments for instances of the given template-type.

OTHER-TEMPLATE-ARGS can be further permutation lists that precede the
arguments of the template type instances.

This is useful if the template accepts the same template arguments as
the template type.

See TEMPLATE-TYPE (type)
See DO-COMBINATIONS")
  
  (function do-instance-combinations
    "Expand all lisp-type names for instances of the given template-types.

This is useful if the template accepts type instance names for
template-arguments.

See TEMPLATE-TYPE (type)
See DO-COMBINATIONS")
  
  (function define-type-dispatch
    "Define a dispatcher function.

EXPANSIONS should have the following structure:

  EXPANSIONS ::= EXPANSION*
  EXPANSION  ::= ((ARGTYPE*) RETTYPE EXPANSION)
  ARGTYPE    --- A lisp-type for the corresponding lambda-list
    variable. If the number of argtypes is shorter than the number of
    variables in the lambda-list, the remainder are automatically
    bound to NULL.
  RETTYPE    --- A lisp-type for the type of the value returned in
    this expansion.
  EXPANSION  --- A form that the type dispatcher will expand into
    should this branch of argument types match.

See DEFINE-TEMPLATED-DISPATCH")

  (function define-dependent-dispatch-type
    "Defines a function that can be used to compute more complex dependent types in templated dispatch function definitions.

TYPE-LIST is bound to the list of ARGTYPEs in the expansion. I is
bound to the index of the current dependent dispatch type to
expand. ARGS are bound to the respective arguments of the call.

BODY should return a concrete type to use in place of the dependent
type expression, or NIL if there is no dependent type and the
expansion should be eliminated.

See DEFINE-TEMPLATED-DISPATCH")
  
  (function define-templated-dispatch
    "Define a dispatcher function using type template information.

BODY accepts the following keyword arguments:
:IGNORE-TEMPLATE-TYPES --- A list of template type names that should
  be ignored for the resulting template invocation's template
  arguments.

The rest of the body should have the following structure:

  EXPANSIONS   ::= EXPANSION*
  EXPANSION    ::= ((ARGTYPE*) TEMPLATE TEMPLATE-ARG*)
                 | ((ARGTYPE*) (TEMPLATE TEMPLATE-ARG*) ARG*)
  ARGTYPE      ::= TEMPLATE-TYPE
                 | REFERENCE
                 | DEPENDENT
                 | REF-ARGUMENT
  TEMPLATE-TYPE --- The name of a template-type, for each of the
    instances of which this expansion will be instantiated.
  REFERENCE     --- A number duplicating the concrete type at that
    position of the ARGTYPE list.
  DEPENDENT     ::= #'(DEPENDENT-NAME ARG*)
  ALIAS-NAME    --- The name of a dependent dispatch type function.
  REF-ARGUMENT  --- A vector of two elements, the first being the
    position of the template type instance to reference in the ARGTYPE
    list, and the second being the number of the template argument of
    that template type instance to use.
  TEMPLATE      --- The name of the template to call.
  TEMPLATE-ARG  --- Additional template arguments that will be
    prepended before the combined template arguments of the
    TEMPLATE-TYPE instance expanded for this EXPANSION. Note that this
    includes *all* template arguments of all template-type instances
    in the ARGTYPES list that aren't excluded via the
    IGNORE-TEMPLATE-TYPES option above.
  ARG           --- An argument to pass to the function call.

The effective function call will be computed based on the template
name and the template arguments, inserting a slash between each as per
the standard naming convention.

See DEFINE-TEMPLATE
See DEFINE-TYPE-DISPATCH
See DEFINE-DEPENDENT-DISPATCH-TYPE
See TEMPLATE-TYPE (type)")
  
  (function define-alias
    "Define a simple alias function.

EXPANSION should evaluate to a form that is used in the function
body. This is similar to defining a compiler-macro, except that it
tries to automatically define both a compiler macro and a regular
function definition at once.")
  
  (function define-slot-accessor
    "Shorthand to define a dispatcher for an accessor of a given slot of a template type.

See DEFINE-TYPE-DISPATCH
See TEMPLATE-TYPE (type)
See INSTANCES
See SLOT"))
