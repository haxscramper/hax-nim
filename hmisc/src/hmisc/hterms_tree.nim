import hterms_callback
import macros
import hashes, sequtils
import halgorithm

export sequtils


# proc hash*[Tree, Enum](a: CaseTerm[Tree, Enum]): Hash =
#   var h: Hash = 0
#   h = h !& hash(a.tkind)
#   case a.tkind:
#     of tkVariable: h = h !& hash(a.name)
#     of tkConstant: h = h !& hash(a.value)
#     of tkFunctor: h = h !& hash(a.functor)
#     of tkPlaceholder: discard

# proc `==`*[Tree, Enum](lhs, rhs: CaseTerm[Tree, Enum]): bool =
#   lhs.tkind == rhs.tkind and (
#     case lhs.tkind:
#       of tkConstant: lhs.value == rhs.value
#       of tkVariable: lhs.name == rhs.name
#       of tkFunctor:
#         lhs.functor == rhs.functor and
#         subnodesEq(lhs, rhs, sons)
#       of tkPlaceholder:
#         true
#   )

# proc makeImpl*[Tree, Enum](
#   # constantType, functorType: set[Enum],
#   # kindField: untyped,
#   strGen: proc(t: Tree): string
#   ): TermImpl[CaseTerm[Tree, Enum], string, Enum, Tree] =
#   ## Create instance of implementation callbacks for particular `CaseTerm`.
#   # TODO check for disjoint sets
#   TermImpl[CaseTerm[Tree, Enum], string, Enum, Tree](
#     getKind: (
#       # Return type of the term based on it's wrapper kind *and*
#       # content's kind.
#       proc(t: CaseTerm[Tree, Enum]): TermKind = t.tkind
#     ),
#     setNth: (
#       proc(self: var CaseTerm[Tree, Enum],
#            idx: int, value: CaseTerm[Tree, Enum]): void =
#         self.sons[idx] = value
#     ),
#     getNth: (
#       proc(self: CaseTerm[Tree, Enum], idx: int): auto = self.sons[idx]
#     ),
#     getNthMod: (
#       proc(self: var CaseTerm[Tree, Enum], idx: int): var CaseTerm[Tree, Enum] =
#         self.sons[idx]
#     ),
#     getVName: (
#       proc(self: CaseTerm[Tree, Enum]): string = self.name
#     ),
#     getFSym: (
#       proc(self: CaseTerm[Tree, Enum]): Enum = self.functor
#     ),
#     getSubt: (
#       proc(self: CaseTerm[Tree, Enum]): seq[CaseTerm[Tree, Enum]] =
#         self.sons
#     ),
#     setSubt: (
#       proc(self: var CaseTerm[Tree, Enum],
#            subt: seq[CaseTerm[Tree, Enum]]): void =
#         self.sons = subt
#     ),
#     getValue: (
#       proc(self: CaseTerm[Tree, Enum]): Tree =
#         self.value
#     ),

#     unifCheck: (
#       proc(self, other: CaseTerm[Tree, Enum]): bool =
#         self.tkind == other.tkind and
#         (
#           block:
#             if self.tkind == tkFunctor:
#               self.functor == other.functor
#             else:
#               true
#         )
#     ),

#     makePlaceholder: (
#       proc(): CaseTerm[Tree, Enum] = CaseTerm[Tree, Enum](
#         tkind: tkPlaceholder
#       )
#     ),
#     makeConstant: (
#       proc(val: Tree): CaseTerm[Tree, Enum] =
#         CaseTerm[Tree, Enum](tkind: tkConstant, value: val)
#     ),
#     makeVariable: (
#       proc(name: string): CaseTerm[Tree, Enum] =
#         CaseTerm[Tree, Enum](tkind: tkVariable, name: name)
#     ),
#     makeFunctor: (
#       proc(sym: Enum, subt: seq[CaseTerm[Tree, Enum]]): CaseTerm[Tree, Enum] =
#         CaseTerm[Tree, Enum](tkind: tkFunctor, functor: sym, sons: subt)
#     ),
#     valStrGen: strGen
#   )

# macro defineTermSystemFor*(
#   treeType, enumType: untyped,
#   kindField, sonsField: untyped,
#   implName, val2String: untyped,
#   functorKinds, constantKinds: untyped,
#   treeMaker: untyped,
#   doExport: bool = true
#       ): untyped  =
#   ## Horrible boilerplate automation setup
#   ##
#   ## :params:
#   ##    :treeType: Type that will be used as value for `CaseTerm`
#   ##               constants
#   ##    :enumType: Type that will be used as functor for `CaseTerm`
#   ##               functors
#   ##    :kindField: Field in `treeType` that is used to determine it's
#   ##                type. Should be `typeof(enumType)`
#   ##    :sonsField: Field in `treeType` that is used to get list of the
#   ##                children for tree instance
#   ##    :implName: Name of the implementation `const` that will be
#   ##               generated for this particular instance
#   ##    :val2String: Function for converting `treeType` to string
#   ##    :functorKinds: Set of kinds that will be converted into functor
#   ##                   case term. Should support `in` operator. For most
#   ##                   cases `set[enumType]` will suffice
#   ##    :constantKinds: Set of kinds that will be converted into constant
#   ##                    case terms. Same requirements as `functorKinds`.
#   ##    :treeMaker: procedure to generate instance of new `treeType`. Two
#   ##                parameters are expected: `treeMaker(kind: enumType,
#   ##                sons: seq[treeType])`
#   ##    :doExport: Whether or not to mark generated procs/consts as
#   ##               exported. `true` by default, should be used only for
#   ##               unit testing.
#   ##
#   ## For application see `hterms_nimast.nim`

#   let trueSym = quote do: true
#   let falseSym = quote do: false

#   let
#     treeSym = ident "tree"
#     termSym = ident "term"

#   if doExport == trueSym:
#     result = quote do:
#       static:
#         var val: `treeType`
#         assert compiles(val == val),
#           "Missing implementation of `== `for " & $typeof(`treeType`)

#       proc toTerm*(`treeSym`: `treeType`): CaseTerm[`treeType`, `enumType`] =
#         if tree.`kindField` in `constantKinds`:
#           return CaseTerm[`treeType`, `enumType`](
#             tkind: tkConstant, value: tree
#           )
#         elif tree.`kindField` in `functorKinds`:
#           return CaseTerm[`treeType`, `enumType`](
#             tkind: tkFunctor,
#             functor: tree.`kindField`,
#             sons: toSeq(tree.`sonsField`).mapIt(it.toTerm())
#           )
#         else:
#           assert false, $typeof(`treeType`) &
#             " cannot be converted to CaseTermTree. Kind " &
#             $tree.`kindField` & " is not in functor/constant sets"

#       proc fromTerm*(`termSym`: CaseTerm[`treeType`, `enumType`]): `treeType` =
#         assert term.tkind in {tkFunctor, tkConstant},
#            "Cannot convert under-substituted term back to tree. " &
#              $term.tkind & " has to be replaced with value"

#         if term.tkind == tkFunctor:
#           result = `treeMaker`(
#             kind = term.functor,
#             sons = term.sons.mapIt(it.fromTerm())
#           )
#         else:
#           result = term.value

#       const `implName`* = makeImpl[`treeType`, `enumType`](
#         strGen = `val2String`
#       )

#   elif doExport == falseSym:
# #===================  do not export generated symbols  ===================#

#     result = quote do:
#       static:
#         var val: `treeType`
#         assert compiles(val == val),
#           "Missing implementation of `== `for " & $typeof(`treeType`)

#       proc toTerm(`treeSym`: `treeType`): CaseTerm[`treeType`, `enumType`] =
#         if tree.`kindField` in `constantKinds`:
#           return CaseTerm[`treeType`, `enumType`](
#             tkind: tkConstant, value: tree
#           )
#         elif tree.`kindField` in `functorKinds`:
#           return CaseTerm[`treeType`, `enumType`](
#             tkind: tkFunctor,
#             functor: tree.`kindField`,
#             sons: toSeq(tree.`sonsField`).mapIt(it.toTerm())
#           )
#         else:
#           assert false, $typeof(`treeType`) &
#             " cannot be converted to CaseTermTree. Kind " &
#             $tree.`kindField` & " is not in functor/constant sets"

#       proc fromTerm(`termSym`: CaseTerm[`treeType`, `enumType`]): `treeType` =
#         assert term.tkind in {tkFunctor, tkConstant},
#            "Cannot convert under-substituted term back to tree. " &
#              $term.tkind & " has to be replaced with value"

#         if term.tkind == tkFunctor:
#           result = `treeMaker`(
#             kind = term.functor,
#             sons = term.sons.mapIt(it.fromTerm())
#           )
#         else:
#           result = term.value

#       const `implName` = makeImpl[`treeType`, `enumType`](
#         strGen = `val2String`
#       )
# #=================================  end  =================================#
#   else:
#     assert false, "Expeced either `true` or `false` for `doExport`"
