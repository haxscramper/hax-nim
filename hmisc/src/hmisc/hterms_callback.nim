## Term algorithms. Implmenetation uses callback functions for getting
## values/types from terms.

import hashes, sequtils, tables, strformat, strutils
import helpers, deques, intsets, halgorithm

import htrie

type
  TermKind* = enum
    tkVariable
    tkFunctor
    tkConstant
    tkPlaceholder

  TermPath = seq[int]
  TermImpl*[Obj, VarSym, FunSym, Val] = object

    getKind*: proc(self: Obj): TermKind ## Get term kind
    setNth*: proc(self: var Obj, idx: int, value: Obj): void
    getNth*: proc(self: Obj, idx: int): Obj
    getNthMod*: proc(self: var Obj, idx: int): var Obj

    getVName*: proc(self: Obj): VarSym
    getFSym*: proc(self: Obj): FunSym
    getSubt*: proc(self: Obj): seq[Obj]
    setSubt*: proc(self: var Obj, subt: seq[Obj]): void

    getValue*: proc(self: Obj): Val
    ## Get value for term `self`

    unifCheck*: proc(self, other: Obj): bool
    ## Procedure to quickly check if two objects can be unified at all

    makePlaceholder*: proc(): Obj
    ## Generate instance of `tkPlaceholder` term
    makeConstant*: proc(val: Val): Obj
    ## Generate instance of `tkConstant` term
    makeVariable*: proc(name: VarSym): Obj
    ## Generate instance of `tkVariable` term
    makeFunctor*: proc(sym: FunSym, subt: seq[Obj]): Obj
    ## Generate instance of `tkFunctor` term using `subt` as subterms
    valStrGen*: proc(val: Val): string
    ## Conver value to string.

  TermEnv*[VarSym, Obj] = object
    ## Mapping between varuable symbols and values
    values*: Table[VarSym, Obj]

  TermMatcher*[VarSym, Obj] = object
    case isPattern*: bool
    of true:
      patt*: Obj
    of false:
      matcher*: proc(test: Obj): Option[TermEnv[VarSym, Obj]]

  GenProc*[VarSym, Obj] = proc(env: TermEnv[VarSym, Obj]): Obj

  RulePair*[VarSym, Obj] = object
    rule*: TermMatcher[VarSym, Obj]
    gen*: GenProc[VarSym, Obj]

  RedSystem*[VarSym, Obj] = object
    rules*: seq[RulePair[VarSym, Obj]]

proc assertCorrect*[Obj, VarSym, FunSym, Val](impl: TermImpl[Obj, VarSym, FunSym, Val]): void =
  ## Check if all fields in `impl` have been correctly initalized
  for name, value in impl.fieldPairs():
    assert (not value.isNil()), name & " cannot be nil"

proc makeRulePair*[VarSym, Obj](
  rule: TermMatcher[VarSym, Obj],
  gen: proc(env: TermEnv[VarSym, Obj]): Obj): RulePair[VarSym, Obj] =
  ## Create rule pair insance
  RulePair[VarSym, Obj](rule: rule, gen: gen)

proc makePattern*[VarSym, Obj](obj: Obj): TermMatcher[VarSym, Obj] =
  ## Create term matcher instance with for patterns
  TermMatcher[VarSym, Obj](patt: obj, isPattern: true)

proc makeMatcher*[VarSym, Obj](
  matcher: proc(test: Obj): Option[TermEnv[VarSym, Obj]]): TermMatcher[VarSym, Obj] =
  ## Create term matcher instance for matching procs
  TermMatcher[VarSym, Obj](isPattern: false, matcher: matcher)

proc makeGenerator*[VarSym, Obj](obj: Obj): proc(env: TermEnv[VarSym, Obj]): Obj =
  ## Create closure proc that will output `obj` as value
  return proc(env: TermEnv[VarSym, Obj]): Obj =
    return obj

proc makeEnvironment*[VarSym, Obj](values: seq[(VarSym, Obj)] = @[]): TermEnv[VarSym, Obj] =
  ## Create new environment using `values` as initial binding values
  TermEnv[VarSym, Obj](values: values.toTable())

proc isBound*[VarSym, Obj](env: TermEnv[VarSym, Obj], term: VarSym): bool =
  ## Check if variable is bound to somethin in `env`
  (term in env.values) # and env[term] != term

proc `[]`*[VarSym, Obj](e: TermEnv[VarSym, Obj], t: VarSym): Obj =
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

proc `[]=`*[VarSym, Obj](
  system: var RedSystem[VarSym, Obj], lhs, rhs: Obj): void =
  ## Add rule to environment
  system.rules[lhs] = rhs

proc `[]=`*[VarSym, Obj](
  env: var TermEnv[VarSym, Obj], variable: VarSym, value: Obj): void =
  ## Set value for variable in environemt

  env.values[variable] = value

iterator items*[VarSym, Obj](system: RedSystem[VarSym, Obj]):
         RulePair[VarSym, Obj] =
  ## Iterate over all rules in rewriting system
  for pair in system.rules:
    yield pair

iterator pairs*[VarSym, Obj](system: RedSystem[VarSym, Obj]):
         (int, RulePair[VarSym, Obj]) =
  ## Iterate over all rules with their indices in rewriting system
  for idx, pair in system.rules:
    yield (idx, pair)


iterator pairs*[VarSym, Obj](
  env: TermEnv[VarSym, Obj]): (VarSym, Obj) =
  ## Iterate over all variables and values in evnironment
  for lhs, rhs in pairs(env.values):
    yield (lhs, rhs)

proc len*[VarSym, Obj](env: TermEnv[VarSym, Obj]): int =
  ## Get number of itesm in enviroenmt
  env.values.len()

proc bindTerm[Obj, VarSym, FunSym, Val](
  variable, value: Obj,
  env: TermEnv[VarSym, Obj],
  cb: TermImpl[Obj, VarSym, FunSym, Val]): TermEnv[VarSym, Obj]

proc copy*[Obj, VarSym, FunSym, Val](
  term: Obj, env: TermEnv[VarSym, Obj],
  cb: TermImpl[Obj, VarSym, FunSym, Val]): (Obj, TermEnv[VarSym, Obj]) =
  ## Create copy of a term. All variables are replaced with new ones.
  let inputEnv = env
  case cb.getKind(term):
    of tkConstant:
      return (term, inputEnv)
    of tkVariable:
      let deref = term.dereference(env, cb)
      if cb.getKind(deref) == tkVariable:
        var newVar = term
        # inc newVar.genIdx
        var resEnv = bindTerm(deref, newVar, env, cb)
        return (newVar, resEnv)
      else:
        return (deref, inputEnv)

    of tkFunctor:
      var resEnv = env
      var subterms: seq[Obj]
      for arg in cb.getSubt(term):
        let (tmpArg, tmpEnv) = arg.copy(resEnv, cb)
        resEnv = tmpEnv
        subterms.add tmpArg

      return (cb.makeFunctor(cb.getFSym(term), subterms), resEnv)

    of tkPlaceholder:
      return (term, inputEnv)

proc bindTerm[Obj, VarSym, FunSym, Val](
  variable, value: Obj, env: TermEnv[VarSym, Obj] ,
  cb: TermImpl[Obj, VarSym, FunSym, Val]): TermEnv[VarSym, Obj] =
  ## Create environment where `variable` is bound to `value`
  result = env
  case cb.getKind(value):
    of tkConstant, tkVariable, tkPlaceholder:
      result[cb.getVName(variable)] = value
    of tkFunctor:
      let (newTerm, newEnv) = value.copy(env, cb)
      result = newEnv
      result[cb.getVName(variable)] = newTerm

proc dereference*[Obj, VarSym, FunSym, Val](
  term: Obj, env: TermEnv[VarSym, Obj], cb: TermImpl[Obj, VarSym, FunSym, Val]): Obj =
  ## Traverse binding chain in environment `env` and return value of
  ## the `term`
  result = term

  while cb.getKind(result) == tkVariable and isBound(env, cb.getVName(result)):
    let value = env[cb.getVName(result)]
    if cb.getKind(value) == tkConstant or value == result:
      result = value
      break

    result = value

proc unif*[Obj, VarSym, FunSym, Val](
  t1, t2: Obj,
  cb: TermImpl[Obj, VarSym, FunSym, Val],
  env: TermEnv[VarSym, Obj] = makeEnvironment[VarSym, Obj]()
    ): Option[TermEnv[VarSym, Obj]] =
  ## Attempt to unify two terms. On success substitution (environment)
  ## is return for which two terms `t1` and `t2` could be considered
  ## equal.
  let
    val1 = dereference(t1, env, cb)
    val2 = dereference(t2, env, cb)
    k1 = cb.getKind(val1)
    k2 = cb.getKind(val2)

  if k1 == tkConstant and k2 == tkConstant:
    if val1 == val2:
      return some(env)
    else:
      return none(TermEnv[VarSym, Obj])
  elif k1 == tkVariable:
    return some(bindTerm(val1, val2, env, cb))
  elif k2 == tkVariable:
    return some(bindTerm(val2, val1, env, cb))
  elif (k1, k2) in @[(tkConstant, tkFunctor), (tkFunctor, tkConstant)]:
    return none(TermEnv[VarSym, Obj])
  else:
    var tmpRes = env
    if cb.getFSym(val1) != cb.getFSym(val2):
      return none(TermEnv[VarSym, Obj])

    if cb.getSubt(val1).len != cb.getSubt(val2).len:
      # TEST with different-sized term unification
      # TODO provide `reason` for failure
      return none(TermEnv[VarSym, Obj])

    for idx, (arg1, arg2) in zip(cb.getSubt(val1), cb.getSubt(val2)):
      let res = unif(arg1, arg2, cb, tmpRes)
      if res.isSome():
        tmpRes = res.get()
      else:
        return none(TermEnv[VarSym, Obj])

    return some(tmpRes)

iterator redexes*[Obj, VarSym, FunSym, Val](
  term: Obj, cb: TermImpl[Obj, VarSym, FunSym, Val]
                                ): tuple[red: Obj, path: TermPath] =
  ## Iterate over all redex in term
  var que: Deque[(Obj, TermPath)]
  que.addLast((term, @[0]))
  while que.len > 0:
    let (nowTerm, path) = que.popFirst()
    if cb.getKind(nowTerm) == tkFunctor:
      for idx, subTerm in cb.getSubt(nowTerm):
        que.addLast((subTerm, path & @[idx]))

    yield (red: nowTerm, path: path)


proc varlist*[Obj, VarSym, FunSym, Val](
  term: Obj, cb: TermImpl[Obj, VarSym, FunSym, Val], path: TermPath = @[0]): seq[(Obj, TermPath)] =
  ## Output list of all variables in term
  case cb.getKind(term):
    of tkConstant, tkPlaceholder:
      return @[]
    of tkVariable:
      return @[(term, path)]
    of tkFunctor:
      for idx, sub in cb.getSubt(term):
        result &= sub.varlist(cb, path & @[idx])


proc setAtPath*[Obj, VarSym, FunSym, Val](
  term: var Obj, path: TermPath, value: Obj, cb: TermImpl[Obj, VarSym, FunSym, Val]): void =
  case cb.getKind(term):
    of tkFunctor:
      if path.len == 1:
        term = value
      else:
        setAtPath(
          term = cb.getNthMod(term, path[1]),
          path = path[1 .. ^1],
          value = value,
          cb
        )
    of tkVariable:
      term = value
    of tkPlaceholder:
      assert false, "Cannot assign to placeholder: " & $term & " = " & $value
    of tkConstant:
      assert false, "Cannot assign to constant: " & $term & " = " & $value

proc substitute*[Obj, VarSym, FunSym, Val](
  term: Obj, env: TermEnv[VarSym, Obj], cb: TermImpl[Obj, VarSym, FunSym, Val]): Obj =
  ## Substitute all variables in term with their values from environment
  result = term
  for (v, path) in term.varlist(cb):
    if env.isBound(cb.getVName(v)):
      result.setAtPath(path, v.dereference(env, cb), cb)

proc treeRepr*[Obj, VarSym, FunSym, Val](
  term: Obj,
  cb: TermImpl[Obj, VarSym, FunSym, Val],
  depth: int = 0): string =

  let ind = "  ".repeat(depth)
  case cb.getKind(term):
    of tkConstant:
      return cb.valStrGen(cb.getValue(term))
        .split("\n").mapIt(ind & "cst " & it).join("\n")
    of tkPlaceholder:
      return ind & "plh _"
    of tkVariable:
      return ind & "var " & cb.getVName(term)
    of tkFunctor:
      return ind & "fun " & $(cb.getFSym(term)) & "\n" &
        cb.getSubt(term).mapIt(treeRepr(it, cb, depth + 1)).join("\n")

type
  ReduceConstraints* = enum
    rcNoConstraints
    rcRewriteOnce
    rcApplyOnce

proc reduce*[Obj, VarSym, FunSym, Val](
  term: Obj,
  system: RedSystem[VarSym, Obj],
  cb: TermImpl[Obj, VarSym, FunSym, Val],
  maxDepth: int = 40,
  maxIterations: int = 4000,
  reduceConstraints: ReduceConstraints = rcApplyOnce
                ): tuple[term: Obj, ok: bool] =
  ## Perform reduction of `term` using `system` rules.
  ##
  ## Iterate over all subterms (redexes) in `term` and try each reduction
  ## rule in `system`. If rule matches, replace subterm with output of
  ## the rule value generator.
  ##
  ## :params:
  ##    :term: term to reduce
  ##    :system: collection of rules (matcher - generator pairs)
  ##    :cb: implementation callbacks for term
  ##    :maxDepth: do not reduce terms deeper than this value
  ##    :maxIterations: stop reduction attempts after reaching this value
  ##    :reduceConstaints: Configuration for continous application of
  ##                       reduction rules on the same paths.
  ##       - **rcNoConstaints** Reduce as long as reduction can take
  ##         place
  ##       - **rcRewriteOnce** do not rewrite subterm or any of it's
  ##         descendants after it has been reduced once
  ##       - **rcApplyOnce** do not use the same rule on term or any of
  ##         it's descendants after rule has been applied once. Reduction
  ##         of the term (or descendants) might still take place but
  ##         using different rules.
  var tmpTerm = term
  var rewPaths: Trie[int, IntSet]
  block outerLoop:
    var iterIdx: int = 0
    while true:
      var canReduce = false
      for (redex, path) in tmpTerm.redexes(cb):
        if path.len < maxDepth and not (
          # Avoid rewriting anyting on this path
          reduceConstraints == rcRewriteOnce and
          rewPaths.prefixHasValue(path)
        ):
          # echo "rewriting at path ", path
          for idx, rule in system:
            if (
              # Avoid using this rule again on the same path
              (reduceConstraints == rcApplyOnce) and
              (rewPaths.prefixHasValue(path)) and
              toSeq(rewPaths.parentValues(path)).anyOfIt(idx in it)
            ):
              continue

            let lhs: TermMatcher[VarSym, Obj] = rule.rule
            let gen: GenProc[VarSym, Obj] = rule.gen
            # Reached max iteration count
            if iterIdx > maxIterations:
              break outerLoop

            inc iterIdx
            var unifRes: Option[TermEnv[VarSym, Obj]]
            case lhs.isPattern:
              of true:
                unifRes = unif(lhs.patt, redex, cb)
              of false:
                unifRes = lhs.matcher(redex)

            # Unification ok, calling generator proc to get replacement
            if unifRes.isSome():
              case reduceConstraints:
                of rcApplyOnce:
                  if path notin rewPaths:
                    rewPaths[path] = IntSet()

                  rewPaths[path].incl idx
                of rcRewriteOnce: rewPaths[path] = IntSet()
                else:
                  discard

              let newEnv = unifRes.get()

              # New value from generator
              let tmpNew = (gen(newEnv)).substitute(newEnv, cb)
              setAtPath(tmpTerm, path, tmpNew, cb)

              if cb.getKind(tmpTerm) notin {tkVariable, tkConstant}:
                canReduce = true
                result[1] = true
              else:
                return (tmpTerm, true)

      if not canReduce:
        result[0] = tmpTerm
        break
