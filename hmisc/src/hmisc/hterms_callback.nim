## Term algorithms. Implmenetation uses callback functions for getting
## values/types from terms.

import hashes, sequtils, tables, strformat, strutils
import helpers, deques, intsets, halgorithm
export tables, intsets

import htrie
export htrie

type
  TermKind* = enum
    tkVariable
    tkFunctor
    tkConstant
    tkPlaceholder

  TermPath = seq[int]
  VarSym* = string

  Term*[V, F] = object
    case tkind*: TermKind
      of tkFunctor:
        functor: F
        subterms: seq[Term[V, F]]
      of tkConstant:
        value: V
      of tkVariable:
        name: VarSym
      of tkPlaceholder:
        nil


  TermImpl*[V, F] = object
    getFSym*: proc(val: V): F
    isFunctor*: proc(val: V): bool
    isFunctorSym*: proc(val: F): bool
    getSubt*: proc(val: V): seq[V]
    # setSubt*: proc(val: var V, subt: seq[V])
    makeFunctor*: proc(sym: F, subt: seq[V]): V
    valStrGen*: proc(val: V): string ## Conver value to string.

  TermEnv*[V, F] = object
    ## Mapping between varuable symbols and values
    values*: Table[VarSym, Term[V, F]]

  GenProc*[V, F] = proc(env: TermEnv[V, F]): Term[V, F] ## Proc
  ## for generaing Values during rewriting.

  MatchProc*[V, F] = proc(test: Term[V, F]): Option[TermEnv[V, F]]

  TermMatcher*[V, F] = object
    case isPattern*: bool
    of true:
      patt*: seq[Term[V, F]]
      first: set[F]
    of false:
      matcher*: MatchProc[V, F]


  RulePair*[V, F] = object
    rule*: TermMatcher[V, F]
    gen*: GenProc[V, F]

  RuleId = int16
  RedSystem*[V, F] = object
    rules*: seq[RulePair[V, F]]


#==========================  making new terms  ===========================#


func makePlaceholder*[V, F](): Term[V, F] =
  Term[V, F](tkind: tkPlaceholder)

func makeConstant*[V, F](val: V): Term[V, F] =
  Term[V, F](tkind: tkConstant, value: val)

func makeVariable*[V, F](name: VarSym): Term[V, F] =
  Term[V, F](tkind: tkVariable, name: name)

func makeFunctor*[V, F](
  sym: F, subt: seq[Term[V, F]]): Term[V, F] =
  Term[V, F](tkind: tkFunctor, functor: sym, subterms: subt)

#======================  accessing term internals  =======================#

func getKind*[V, F](t: Term[V, F]): TermKind =
  t.tkind

func getVName*[V, F](t: Term[V, F]): VarSym =
  assert t.getKind() == tkVariable
  t.name

func getFSym*[V, F](t: Term[V, F]): F =
  assert t.getKind() == tkFunctor
  t.functor

func getNth*[V, F](
  t: Term[V, F], idx: int): Term[V, F]=
  assert t.getKind() == tkFunctor
  t.subterms[idx]

func getNthMod*[V, F](
  t: var Term[V, F], idx: int): var Term[V, F]=
  assert t.getKind() == tkFunctor
  t.subterms[idx]

func getSubt*[V, F](
  t: Term[V, F]): seq[Term[V, F]] =
  assert t.getKind() == tkFunctor
  t.subterms

func setSubt*[V, F](
  t: var Term[V, F], subt: seq[Term[V, F]]): void =
  assert t.getKind() == tkFunctor
  t.subterms = subt

func getValue*[V, F](self: Term[V, F]): V =
  assert self.getKind() == tkConstant
  self.value

#=======================  converting to/from term  =======================#

proc toTerm*[V, F](val: V, cb: TermImpl[V, F]): Term[V, F] =
  if cb.isFunctor(val):
    return makeFunctor[V, F](cb.getFSym(val), cb.getSubt(val).mapIt(it.toTerm(cb)))
  else:
    return makeConstant[V, F](val)

proc fromTerm*[V, F](term: Term[V, F], cb: TermImpl[V, F]): V =
  assert term.getKind() in {tkFunctor, tkConstant},
   "Cannot convert under-substituted term back to tree. " &
     $term.getKind() & " has to be replaced with value"

  if term.getKind() == tkFunctor:
    result = cb.makeFunctor(
      term.getFSym(),
      term.getSubt().mapIt(it.fromTerm(cb)))
  else:
    result = term.getValue()

#==================================  2  ==================================#
proc assertCorrect*[V, F](impl: TermImpl[V, F]): void =
  ## Check if all fields in `impl` have been correctly initalized
  for name, value in impl.fieldPairs():
    assert (not value.isNil()), name & " cannot be nil"

func makeRulePair*[V, F](
  rule: TermMatcher[V, F], gen: GenProc[V, F]): RulePair[V, F] =
  ## Create rule pair insance
  RulePair[V, F](rule: rule, gen: gen)

func makeMatcher*[V, F](matcher: MatchProc[V, F]): TermMatcher[V, F] =
  ## Create term matcher instance for matching procs
  TermMatcher[V, F](isPattern: false, matcher: matcher)

func makeMatcher*[V, F](patt: Term[V, F]): TermMatcher[V, F] =
  result = TermMatcher[V, F](isPattern: true, patt: @[ patt ])

  if patt.getKind() == tkFunctor:
    result.first = { patt.functor }

func makeFunctor*[V, F](patts: seq[Term[V, F]]): TermMatcher[V, F] =
  result = TermMatcher[V, F](isPattern: true, patt: patts)
  for patt in patts:
    if patt.getKind() == tkFunctor:
      result.first.incl patt.functor


func makeGenerator*[V, F](obj: Term[V, F]): GenProc[V, F] =
  ## Create closure proc that will output `obj` as value
  return proc(env: TermEnv[V, F]): Term[V, F] =
    return obj

func makeEnvironment*[V, F](values: seq[(VarSym, Term[V, F])] = @[]): TermEnv[V, F] =
  ## Create new environment using `values` as initial binding values
  TermEnv[V, F](values: values.toTable())

func makeReductionSystem*[V, F](
  values: seq[RulePair[V, F]]): RedSystem[V, F] =
  RedSystem[V, F](rules: values)

func isBound*[V, F](env: TermEnv[V, F], term: VarSym): bool =
  ## Check if variable is bound to somethin in `env`
  (term in env.values) # and env[term] != term

func `[]`*[V, F](e: TermEnv[V, F], t: VarSym): Term[V, F] =
  ## Access value from environment.
  try:
    e.values[t]
  except KeyError:
    # TODO check if `VarSym` can be converter to string
    # TODO use define to constrol exception verbosity
    let vars = e.mapPairs($lhs).joinq()
    raise newException(
      KeyError,
      &"Missing variable `{t}` in environment. Have vars: {vars}")

func `[]=`*[V, F](system: var RedSystem[V, F], lhs, rhs: Term[V, F]): void =
  ## Add rule to environment
  system.rules[lhs] = rhs

func `[]=`*[V, F](env: var TermEnv[V, F], variable: VarSym, value: Term[V, F]): void =
  ## Set value for variable in environemt
  env.values[variable] = value


func `==`*[V, F](lhs, rhs: Term[V, F]): bool =
  lhs.tkind == rhs.tkind and (
    case lhs.tkind:
      of tkConstant: lhs.value == rhs.value
      of tkVariable: lhs.name == rhs.name
      of tkFunctor: lhs.functor == rhs.functor and subnodesEq(lhs, rhs, subterms)
      of tkPlaceholder: true
  )

iterator items*[V, F](system: RedSystem[V, F]): RulePair[V, F] =
  ## Iterate over all rules in rewriting system
  for pair in system.rules:
    yield pair

iterator pairs*[V, F](system: RedSystem[V, F]): (RuleId, RulePair[V, F]) =
  ## Iterate over all rules with their indices in rewriting system
  for idx, pair in system.rules:
    yield (RuleId(idx), pair)


iterator pairs*[V, F](env: TermEnv[V, F]): (VarSym, Term[V, F]) =
  ## Iterate over all variables and values in evnironment
  for lhs, rhs in pairs(env.values):
    yield (lhs, rhs)

func len*[V, F](env: TermEnv[V, F]): int =
  ## Get number of itesm in enviroenmt
  env.values.len()

func bindTerm[V, F](
  variable, value: Term[V, F], env: TermEnv[V, F], ): TermEnv[V, F]

func copy*[V, F](term: Term[V, F], env: TermEnv[V, F]): (Term[V, F], TermEnv[V, F]) =
  ## Create copy of a term. All variables are replaced with new ones.
  # DOC what is returned?
  let inputEnv = env
  case getKind(term):
    of tkConstant:
      return (term, inputEnv)
    of tkVariable:
      let deref = term.dereference(env)
      if getKind(deref) == tkVariable:
        var newVar = term
        # inc newVar.genIdx
        var resEnv = bindTerm(deref, newVar, env)
        return (newVar, resEnv)
      else:
        return (deref, inputEnv)

    of tkFunctor:
      var resEnv = env
      var subterms: seq[Term[V, F]]
      for arg in getSubt(term):
        let (tmpArg, tmpEnv) = arg.copy(resEnv)
        resEnv = tmpEnv
        subterms.add tmpArg

      return (makeFunctor(getFSym(term), subterms), resEnv)

    of tkPlaceholder:
      return (term, inputEnv)

func bindTerm[V, F](variable, value: Term[V, F], env: TermEnv[V, F]): TermEnv[V, F] =
  ## Create environment where `variable` is bound to `value`
  result = env
  case getKind(value):
    of tkConstant, tkVariable, tkPlaceholder:
      result[getVName(variable)] = value
    of tkFunctor:
      let (newTerm, newEnv) = value.copy(env)
      result = newEnv
      result[getVName(variable)] = newTerm

func dereference*[V, F](
  term: Term[V, F], env: TermEnv[V, F], ): Term[V, F]=
  ## Traverse binding chain in environment `env` and return value of
  ## the `term`
  result = term

  while getKind(result) == tkVariable and isBound(env, getVName(result)):
    let value = env[getVName(result)]
    if getKind(value) == tkConstant or value == result:
      result = value
      break

    result = value

func unif*[V, F](
  t1, t2: Term[V, F],
  env: TermEnv[V, F] = makeEnvironment[V, F]()): Option[TermEnv[V, F]] =
  ## Attempt to unify two terms. On success substitution (environment)
  ## is return for which two terms `t1` and `t2` could be considered
  ## equal.
  let
    val1 = dereference(t1, env)
    val2 = dereference(t2, env)
    k1 = getKind(val1)
    k2 = getKind(val2)

  if k1 == tkConstant and k2 == tkConstant:
    if val1 == val2:
      return some(env)
    else:
      return none(TermEnv[V, F])
  elif k1 == tkVariable:
    return some(bindTerm(val1, val2, env))
  elif k2 == tkVariable:
    return some(bindTerm(val2, val1, env))
  elif (k1, k2) in @[(tkConstant, tkFunctor), (tkFunctor, tkConstant)]:
    return none(TermEnv[V, F])
  else:
    var tmpRes = env
    if getFSym(val1) != getFSym(val2):
      return none(TermEnv[V, F])

    if getSubt(val1).len != getSubt(val2).len:
      # TEST with different-sized term unification
      # TODO provide `reason` for failure
      return none(TermEnv[V, F])

    for idx, (arg1, arg2) in zip(getSubt(val1), getSubt(val2)):
      let res = unif(arg1, arg2, tmpRes)
      if res.isSome():
        tmpRes = res.get()
      else:
        return none(TermEnv[V, F])

    return some(tmpRes)

iterator redexes*[V, F](
  term: Term[V, F], ): tuple[red: Term[V, F], path: TermPath] =
  ## Iterate over all redex in term
  var que: Deque[(Term[V, F], TermPath)]
  que.addLast((term, @[0]))
  while que.len > 0:
    let (nowTerm, path) = que.popFirst()
    if getKind(nowTerm) == tkFunctor:
      for idx, subTerm in getSubt(nowTerm):
        que.addLast((subTerm, path & @[idx]))

    yield (red: nowTerm, path: path)


func varlist*[V, F](term: Term[V, F], path: TermPath = @[0]): seq[(Term[V, F], TermPath)] =
  ## Output list of all variables in term
  case getKind(term):
    of tkConstant, tkPlaceholder:
      return @[]
    of tkVariable:
      return @[(term, path)]
    of tkFunctor:
      for idx, sub in getSubt(term):
        result &= sub.varlist(path & @[idx])


proc setAtPath*[V, F](term: var Term[V, F], path: TermPath, value: Term[V, F]): void =
  case getKind(term):
    of tkFunctor:
      if path.len == 1:
        term = value
      else:
        setAtPath(
          term = getNthMod(term, path[1]),
          path = path[1 .. ^1],
          value = value)
    of tkVariable:
      term = value
    of tkPlaceholder:
      assert false, "Cannot assign to placeholder: " & $term & " = " & $value
    of tkConstant:
      assert false, "Cannot assign to constant: " & $term & " = " & $value

proc substitute*[V, F](term: Term[V, F], env: TermEnv[V, F]): Term[V, F] =
  ## Substitute all variables in term with their values from environment
  result = term
  for (v, path) in term.varlist():
    if env.isBound(getVName(v)):
      result.setAtPath(path, v.dereference(env))

proc treeRepr*[V, F](term: Term[V, F], cb: TermImpl[V, F], depth: int = 0): string =
  let ind = "  ".repeat(depth)
  case getKind(term):
    of tkConstant:
      return cb.valStrGen(getValue(term))
        .split("\n").mapIt(ind & "cst " & it).join("\n")
    of tkPlaceholder:
      return ind & "plh _"
    of tkVariable:
      return ind & "var " & getVName(term)
    of tkFunctor:
      return (
        @[ ind & "fun " & $(getFSym(term)) ] &
        getSubt(term).mapIt(treeRepr(it, cb, depth + 1))
      ).join("\n")

type
  ReduceConstraints* = enum
    rcNoConstraints
    rcRewriteOnce
    rcApplyOnce


  ReductionState = object
    rewPaths: Trie[int, set[RuleId]]
    constr: ReduceConstraints
    maxDepth: int


proc registerUse(rs: var ReductionState, path: TermPath, id: RuleId): void =
  case rs.constr:
    of rcApplyOnce:
      if path notin rs.rewPaths:
        var tmp: set[RuleId]
        rs.rewPaths[path] = tmp

      rs.rewPaths[path].incl id
    of rcRewriteOnce:
      var tmp: set[RuleId]
      rs.rewPaths[path] = tmp

    else:
      discard

proc canRewrite(rs: ReductionState, path: TermPath): bool =
  path.len < rs.maxDepth and not (
    # Avoid rewriting anyting on this path
    rs.constr == rcRewriteOnce and
    rs.rewPaths.prefixHasValue(path)
  )

proc cannotUse(rs: ReductionState, path: TermPath, rule: RuleId): bool =
    # Avoid using this rule again on the same path
    (rs.constr == rcApplyOnce) and
    (rs.rewPaths.prefixHasValue(path)) and
    toSeq(rs.rewPaths.prefixedValues(path)).anyOfIt(rule in it)


proc reduce*[V, F](
  term: Term[V, F],
  system: RedSystem[V, F],
  maxDepth: int = 40,
  maxIterations: int = 4000,
  reduceConstraints: ReduceConstraints = rcApplyOnce
                ): tuple[term: Term[V, F], ok: bool, rewPaths: Trie[int, set[RuleId]]] =
  ##[

Perform reduction of `term` using `system` rules.

Iterate over all subterms (redexes) in `term` and try each reduction
rule in `system`. If rule matches, replace subterm with output of the
rule value generator.

## Parameters

:term: term to reduce
:system: collection of rules (matcher - generator pairs)
:cb: implementation callbacks for term
:maxDepth: do not reduce terms deeper than this value
:maxIterations: stop reduction attempts after reaching this value
:reduceConstaints: Configuration for continous application of
                   reduction rules on the same paths.
   - **rcNoConstaints** Reduce as long as reduction can take
     place
   - **rcRewriteOnce** do not rewrite subterm or any of it's
     descendants after it has been reduced once
   - **rcApplyOnce** do not use the same rule on term or any of
     it's descendants after rule has been applied once. Reduction
     of the term (or descendants) might still take place but
     using different rules.

  ]##
  var tmpTerm = term
  var rs = ReductionState(constr: reduceConstraints, maxDepth: maxDepth)
  # var rewPaths: Trie[int, IntSet]
  defer:
    result.rewPaths = rs.rewPaths

  block outerLoop:
    var iterIdx: RuleId = 0
    while true:
      var canReduce = false
      for (redex, path) in tmpTerm.redexes():
        if rs.canRewrite(path):
          for idx, rule in system:
            if rs.cannotUse(path, idx):
              continue

            let lhs: TermMatcher[V, F] = rule.rule
            let gen: GenProc[V, F] = rule.gen
            # Reached max iteration count
            if iterIdx > maxIterations:
              break outerLoop

            inc iterIdx
            var unifRes: Option[TermEnv[V, F]]
            case lhs.isPattern:
              of true:
                unifRes = unif(lhs.patt[0] #[ IMPLEMENT HACK ]#, redex)
              of false:
                unifRes = lhs.matcher(redex)

            # Unification ok, calling generator proc to get replacement
            if unifRes.isSome():
              rs.registerUse(path, idx)
              let newEnv = unifRes.get()

              # New value from generator
              let tmpNew = (gen(newEnv)).substitute(newEnv)
              setAtPath(tmpTerm, path, tmpNew)

              if getKind(tmpTerm) notin {tkVariable, tkConstant}:
                canReduce = true
                result.ok = true
              else:
                return (tmpTerm, true, rs.rewPaths)

      if not canReduce:
        result.term = tmpTerm
        break
