import macros
import options
import strutils
import hmisc/halgorithm
import sugar
import strformat
import hashes
import sequtils
import tables


when false:
  template iterValType*(arg: untyped): untyped =
    when compiles(arg[0]):
      typeof(arg[0])
    else:
      typeof(arg)

  var ress {.inject.}: seq[iterValType((0 .. 2))]
  let iterFor_i = iterator (): iterValType(0 .. 2) =
                    for val in 0 .. 2:
                      yield val

  let iterFor_q = iterator (): iterValType(@[1, 2, 4]) =
    for val in @[1, 2, 4]:
      yield val

# dumpTree:
#   (not finished(iterFor_i)) and (not finished(iterFor_q))

# template t(d: string = "default"): untyped =
#   echo "h"

when false:
  macro loop(arg, body: untyped): untyped =
    echo arg.toStrLit()
    echo body.toStrLit()

  macro loop1(body: untyped): untyped =
    quote do:
      block:
        type ResType {.inject.} = int
        var r {.inject.}: ResType
        loop((), `body`)
        r


  loop([res = [all, tuple], nn = [11]], (lfor i in 0..2; lmax i; lcoll i))
  echo typeof(loop1((lfor i in 0..2; lmax i; lcoll i)))
  echo typeof(loop1((lfor i in 0..2; lmax i; lcoll i)))

  loop [res = all]:
    lfor 12

when false:
  import hmisc/hterms
  type
    Arithm = Term[string, int]
    ArithmEnv = TermEnv[string, int]
    ArithmSys = RedSystem[string, int]


  let mf = makeFunctor[string, int]
  let mc = makeConstant[string, int]
  let mp = makePlaceholder[string, int]
  let mv = makeVariable[string, int]

  let t1 = Arithm()
  let t2 = Arithm()
  var env = ArithmEnv()
  var sys = ArithmSys()

  # A + 0 -> A
  sys["+".mf(@[mv "A", mc 0])] = mv "A"

  # A + S(B) -> S(A + B)
  sys["+".mf(@[mv "A", "S".mf(@[mv "B"])])] =
    "S".mf(@["+".mf(@[mv "A", mv "B"])])

  # A * 0 -> 0
  sys["*".mf(@[mv "A", mc 0])] = mc 0

  # A * S(B) -> A + (A * B)
  sys["*".mf(@[mv "A", "S".mf(@[mv "B"])])] =
    "+".mf(@[mv "A", "+".mf(@[mv "A", mv "B"])])

  let sum = "+".mf(@[
      "S".mf(@["S".mf(@[mc 0])]),
      "S".mf(@["S".mf(@[mc 0])])
    ])

  echo reduce(sum, sys)

import hmisc/hterms_callback

type
  CaseTerm[Tree, Enum] = object
    case tkind: TermKind
      of tkFunctor:
        functor: Enum
        sons: seq[CaseTerm[Tree, Enum]]
      of tkConstant:
        value: Tree
      of tkVariable:
        name: string
      of tkPlaceholder:
        nil

proc makeImpl[Tree, Enum](
  # constantType, functorType: set[Enum],
  # kindField: untyped,
                            ): TermImpl[CaseTerm[Tree, Enum], string, Enum, Tree] =
  # TODO check for disjoint sets
  TermImpl[CaseTerm[Tree, Enum], string, Enum, Tree](
    getKind: (
      # Return type of the term based on it's wrapper kind *and*
      # content's kind.
      proc(t: CaseTerm[Tree, Enum]): TermKind = t.tkind
        # case t.tkind:
        #   of tkVariable, tkPlaceholder:
        #     return t.tkind
        #   of tkFunctor, tkConstant:
        #     if t.content.kindField in constantType:
        #       return tkConstant
        #     elif t.content.kindField in functorType:
        #       return tkFunctor
        #     else:
        #       assert false, "Missing value in constant/functor sets: " & $t.content.kindField
    ),
    setNth: (
      proc(self: var CaseTerm[Tree, Enum], idx: int, value: CaseTerm[Tree, Enum]): void =
        self.sons[idx] = value
    ),
    getNth: (
      proc(self: CaseTerm[Tree, Enum], idx: int): auto = self.sons[idx]
    ),
    getNthMod: (
      proc(self: var CaseTerm[Tree, Enum], idx: int): var CaseTerm[Tree, Enum] =
        self.sons[idx]
    ),
    getVName: (
      proc(self: CaseTerm[Tree, Enum]): string = self.name
    ),
    getFSym: (
      proc(self: CaseTerm[Tree, Enum]): Enum = self.functor
    ),
    getSubt: (
      proc(self: CaseTerm[Tree, Enum]): seq[CaseTerm[Tree, Enum]] =
        self.sons
    ),
    setSubt: (
      proc(self: var CaseTerm[Tree, Enum], subt: seq[CaseTerm[Tree, Enum]]): void =
        self.sons = subt
    ),
    getValue: (
      proc(self: CaseTerm[Tree, Enum]): Tree =
        self.value
    ),

    unifCheck: (
      proc(self, other: CaseTerm[Tree, Enum]): bool =
        self.tkind == other.tkind and
        (
          block:
            if self.tkind == tkFunctor:
              self.functor == other.functor
            else:
              true
        )
    ),

    makePlaceholder: (
      proc(): CaseTerm[Tree, Enum] = CaseTerm[Tree, Enum](
        tkind: tkPlaceholder
      )
    ),
    makeConstant: (
      proc(val: Tree): CaseTerm[Tree, Enum] =
        CaseTerm[Tree, Enum](tkind: tkConstant, value: val)
    ),
    makeVariable: (
      proc(name: string): CaseTerm[Tree, Enum] =
        CaseTerm[Tree, Enum](tkind: tkVariable, name: name)
    ),
    makeFunctor: (
      proc(sym: Enum, subt: seq[CaseTerm[Tree, Enum]]): CaseTerm[Tree, Enum] =
        CaseTerm[Tree, Enum](tkind: tkFunctor, functor: sym, sons: subt)
    )
  )

template defineToTermProc[Tree, Enum](
  kindField, sonsField: untyped,
  functorKinds, constantKinds: set[Enum]
                                    ): untyped =
  proc toTerm(tree: Tree): CaseTerm[Tree, Enum] =
    if tree.kindField in constantKinds:
      return CaseTerm[Tree, Enum](
        tkind: tkConstant, value: tree
      )
    elif tree.kindField in functorKinds:
      return CaseTerm[Tree, Enum](
        tkind: tkFunctor, sons: tree.sonsField.mapIt(it.toTerm())
      )
    else:
      assert false, $typeof(Tree) & " cannot be converted to CaseTermTree. Kind " &
        $tree.kindField & " is not in functor/constant sets"

template defineFromTermProc[Tree, Enum](kindField, sonsField: untyped): untyped =
  proc fromTerm(term: CaseTerm[Tree, Enum]): Tree =
    assert term.tkind in {tkFunctor, tkConstant},
       "Cannot convert under-substituted term back to tree. " &
         $term.tkind & " has to be replaced with valu"

    if term.tkind == tkFunctor:
      result = Tree(
        kindField: term.functor,
        sonsField: term.sons.mapIt(it.fromTerm())
      )
    else:
      result = term.value

proc hash[Tree, Enum](a: CaseTerm[Tree, Enum]): Hash =
  # static:
  #   assert compiles(hash(Tree)), "No hash implementation exists for" & $Tree

  var h: Hash = 0
  h = h !& hash(a.tkind)
  case a.tkind:
    of tkVariable: h = h !& hash(a.name)
    of tkConstant: h = h !& hash(a.value)
    of tkFunctor: h = h !& hash(a.functor)
    of tkPlaceholder: discard

proc `==`[Tree, Enum](lhs, rhs: CaseTerm[Tree, Enum]): bool =
  lhs.tkind == rhs.tkind and (
    case lhs.tkind:
      of tkConstant: lhs.value == rhs.value
      of tkVariable: lhs.name == rhs.name
      of tkFunctor:
        lhs.functor == rhs.functor and
        zip(lhs.sons, rhs.sons).allOfIt(it[0] == it[1])
      of tkPlaceholder:
        true
  )

template subnodesEq(lhs, rhs, field: untyped): untyped =
  zip(lhs.field, rhs.field).allOfIt(it[0] == it[1])


template defineTermSystemFor[Tree, Enum](
  kindField, sonsField: untyped,
  implName: untyped,
  functorKinds, constantKinds: set[Enum]
                                    ): untyped  =

  static:
    assert compiles(hash(Tree)), "Missing implementation of hash for " & $typeof(Tree)
    assert compiles(Tree == Tree), "Missing implementation of `== `for " & $typeof(Tree)


  proc toTerm(tree: Tree): CaseTerm[Tree, Enum] =
    if tree.kindField in constantKinds:
      return CaseTerm[Tree, Enum](
        tkind: tkConstant, value: tree
      )
    elif tree.kindField in functorKinds:
      return CaseTerm[Tree, Enum](
        tkind: tkFunctor, sons: tree.sonsField.mapIt(it.toTerm())
      )
    else:
      assert false, $typeof(Tree) & " cannot be converted to CaseTermTree. Kind " &
        $tree.kindField & " is not in functor/constant sets"

  proc fromTerm(term: CaseTerm[Tree, Enum]): Tree =
    assert term.tkind in {tkFunctor, tkConstant},
       "Cannot convert under-substituted term back to tree. " &
         $term.tkind & " has to be replaced with valu"

    if term.tkind == tkFunctor:
      result = Tree(
        kindField: term.functor,
        sonsField: term.sons.mapIt(it.fromTerm())
      )
    else:
      result = term.value

  const implName = makeImpl[Tree, Enum]()


type
  AstKind = enum
    # Constant values
    akStrLit
    akIntLit
    akIdent

    # Functors
    akCall
    akCondition

  Ast = object
    case kind: AstKind
    of akStrLit, akIdent:
      strVal: string
    of akIntLit:
      intVal: int
    else:
      sons: seq[Ast]

proc hash(a: Ast): Hash =
  var h: Hash = 0
  h = h !& hash(a.kind)

proc `==`(lhs, rhs: Ast): bool =
  lhs.kind == rhs.kind and
  (
    case lhs.kind:
      of akStrLit, akIdent: lhs.strVal == rhs.strVal
      of akIntLit: lhs.intVal == rhs.intVal
      else: subnodesEq(lhs, rhs, sons)
  )

defineTermSystemFor[Ast, AstKind](
    kindField = kind,
    sonsField = sons,
    implName = astImpl,
    functorKinds = {akCall .. akCondition},
    constantKinds = {akStrLit .. akIdent}
)

let rSystem = RedSystem[CaseTerm[Ast, AstKind]](

)

let obj = Ast(kind: akCall, sons: @[
  Ast(kind: akIdent, strVal: "someFunc"),
  Ast(kind: akIntLit, intVal: 9000)
])

let res = reduce(
  obj.toTerm(

  ),
  rSystem,
  astImpl
)

if res.ok:
  echo res.term.fromTerm()
else:
  echo "res not ok"
