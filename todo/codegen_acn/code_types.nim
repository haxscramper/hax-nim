import tables

type
  ## Common type properities
  TypeSpec* = enum
    const_t ## Constant (immutable)
    ptr_t ## Pointer type
    ref_t ## Reference type

  ## Common type types
  TypeKind* = enum
    hash_t   ## Hashtable
    vec_t    ## Dynamic array
    string_t ## String
    int_t    ## Signed integral
    enum_t   ## C-style enumeration (not a sum type)
    other_t  ## Other type
  Type* = ref object
    spec*: seq[TypeSpec]
    case kind*: TypeKind
      of other_t, enum_t, string_t:
        name*: string ## Name of the type. For enumeration it is the
                      ## name of the enum.
      of hash_t, vec_t:
        subtypes*: seq[Type] ## List of subtypes (for vector it is
                             ## type of the elements, for hash it is
                             ## key type and value type)
      of int_t:
        nil

type
  ## Variable
  Var* = object
    name*: string ## name of the variable
    vtyp*: Type ## type

type
  ## Access type for class fields/methods
  AcsType* = enum
    acsPublic ## Public
    acsPrivate ## Private
    acsProtected ## Protected


type
  ## Single section of the class
  ClsSection* = ref object
    acsType* : AcsType   ## public/private ...
    body* : seq[ref Acn] ## Nodes in the section
    comm* : string       ## Section comment for this section

  AcnKind* = enum
    acnEnum       ## C-style enumeration
    acnClass      ## C++/Java-like class
    acnFunction   ## Free function
    acnPredicate  ## Expression that evaluates to boolean
    acnIfStmt     ## If Conditional
    acnElseIfStmt ## Else-if conditional
    acnElseStmt   ## Else
    acnCode       ## Code literal
    acnSwitch     ## C-style switch
    acnField      ## Class field
    acnWhile      ## While loop
    # NOTE things to be added later: sum and product types, _either_,
    # optional, unique and shared pointers.

  Acn* = ref object
    name*: string ## Name of the acn node. Used for
                  ## classes/enums/function etc, ignored for
                  ## if/else-if/while etc.
    body*: seq[Acn] ## Child nodes. For class it is content of the
                    ## class (declaration of the fields, methods
                    ## etc.), for if/else-if/while etc. it their body.
    comm*: string ## *Regular comment* string
    case kind*: AcnKind
      of acnEnum:
        ## Names of the enum fields. Names of the fields are stored in
        ## `name` fields of the variable type
        eFields*: seq[Var]
      of acnClass:
        ## List of parent classes
        parents*: seq[(string, string)] # TODO replace with pair acsType+Type
        ## Class sections.
        sections*: seq[ClsSection]

      of acnFunction:
        ## Resulting type of the function
        restype*: string # TODO replace with `Type`
        ## List of the arguments
        args*: seq[Var] # TODO add optional default parameters
      of acnPredicate, acnCode:
        ## Code literal
        code*: string
      of acnIfStmt, acnElseIfStmt, acnElseStmt, acnWhile:
        ## Predicate to test agains
        cond*: Acn # TODO Change name to `predicate`
      of acnSwitch:
        ## Variable to switch against
        swVar*: Var
        ## List of switch cases. Pair "case-value + case-action"
        swCases*: seq[(string, Acn)] # TODO Add "acnValue" and use it
                                     # for case switches
        ## Default switch action
        swDefault*: Acn
      of acnField:
        ## Class field
        val*: Var


type
  ## Template of the type to substitute value into. Represented as
  ## sequence of string literals that should be spliced with another
  ## string literals when deriving type. For example template that
  ## contains ``["std::vector <", ">"]`` when used with
  ## ``["QString"]`` in template substition will generate
  ## ``std::vector<QString>``. Templates do not hold any semantic
  ## meaning.
  TypeTemplate* = object
    strLiterals*: seq[string]

  ## Collection of type templates for substituting types into
  TypeTemplates* = object
    templates*: Table[TypeKind, TypeTemplate]


type
  ## Concrete code node. Used for converting to final code string.
  CNode* = ref object
    code*: string ## First lines of code
    under*: seq[CNode] ## Code under
