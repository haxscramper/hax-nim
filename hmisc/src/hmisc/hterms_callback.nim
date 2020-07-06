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
    getSubt*: proc(val: V): seq[V]
    # getKind*: proc(self: Obj): TermKind ## Get term kind
    # setNth*: proc(self: var Obj, idx: int, value: Obj): void
    # getNth*: proc(self: Obj, idx: int): Obj
    # getNthMod*: proc(self: var Obj, idx: int): var Obj

    # getVName*: proc(self: Tree): VarSym
    # getFSym*: proc(self: Obj): FunSym
    # getSubt*: proc(self: Obj): seq[Obj]
    # setSubt*: proc(self: var Obj, subt: seq[Obj]): void

    # getValue*: proc(self: Obj): Val
    # ## Get value for term `self`

    # REFACTOR - absolete with new algorithm/implementation
    # unifCheck*: proc(self, other: Obj): bool
    # ## Procedure to quickly check if two objects can be unified at all

    # makePlaceholder*: proc(): Term[V, F]
    # ## Generate instance of `tkPlaceholder` term
    # makeConstant*: proc(val: Val): Term[V, F]
    # ## Generate instance of `tkConstant` term
    # makeVariable*: proc(name: VarSym): Term[V, F]
    # ## Generate instance of `tkVariable` term
    # makeFunctor*: proc(sym: FunSym, subt: seq[Term[V, F]]): Term[V, F]
    # ## Generate instance of `tkFunctor` term using `subt` as subterms
    valStrGen*: proc(val: V): string
    ## Conver value to string.

  TermEnv*[V, F] = object
    ## Mapping between varuable symbols and values
    values*: Table[VarSym, Term[V, F]]

  GenProc*[V, F] = proc(env: TermEnv[V, F]): Term[V, F] ## Proc
  ## for generaing Values during rewriting.

  MatchProc*[V, F] = proc(test: Term[V, F]): Option[TermEnv[V, F]]

  TermMatcher*[V, F] = object
    case isPattern*: bool
    of true:
      patt*: Term[V, F]
    of false:
      matcher*: MatchProc[V, F]


  RulePair*[V, F] = object
    rule*: TermMatcher[V, F]
    gen*: GenProc[V, F]

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
  t.kind

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

# func unifChec

#==================================  2  ==================================#

# REFACTOR remove (make TermImple fields `not nil` ?)
proc assertCorrect*[V, F](impl: TermImpl[V, F]): void =
  ## Check if all fields in `impl` have been correctly initalized
  for name, value in impl.fieldPairs():
    assert (not value.isNil()), name & " cannot be nil"

func makeRulePair*[V, F](
  rule: TermMatcher[V, F], gen: GenProc[V, F]): RulePair[V, F] =
  ## Create rule pair insance
  RulePair[V, F](rule: rule, gen: gen)

func makePattern*[V, F](obj: Term[V, F]): TermMatcher[V, F] =
  ## Create term matcher instance with for patterns
  TermMatcher[V, F](patt: obj, isPattern: true)

func makeMatcher*[V, F](matcher: MatchProc[V, F]): TermMatcher[V, F] =
  ## Create term matcher instance for matching procs
  TermMatcher[V, F](isPattern: false, matcher: matcher)

func makeGenerator*[V, F](obj: Term[V, F]): GenProc[V, F] =
  ## Create closure proc that will output `obj` as value
  return proc(env: TermEnv[V, F]): Term[V, F] =
    return obj

func makeEnvironment*[V, F](values: seq[(VarSym, Term[V, F])] = @[]): TermEnv[V, F] =
  ## Create new environment using `values` as initial binding values
  TermEnv[V, F](values: values.toTable())

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

func `[]=`*[VarSym, Obj](
  system: var RedSystem[VarSym, Obj], lhs, rhs: Obj): void =
  ## Add rule to environment
  system.rules[lhs] = rhs

func `[]=`*[VarSym, Obj](
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

proc bindTerm[V, F](
  variable, value: Term[V, F], env: TermEnv[V, F], cb: TermImpl[V, F]): TermEnv[V, F]

proc copy*[V, F](
  term: Term[V, F], env: TermEnv[V, F],
  cb: TermImpl[V, F]): (Term[V, F], TermEnv[V, F]) =
  ## Create copy of a term. All variables are replaced with new ones.
  # DOC what is returned?
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
      var subterms: seq[Term[V, F]]
      for arg in cb.getSubt(term):
        let (tmpArg, tmpEnv) = arg.copy(resEnv, cb)
        resEnv = tmpEnv
        subterms.add tmpArg

      return (cb.makeFunctor(cb.getFSym(term), subterms), resEnv)

    of tkPlaceholder:
      return (term, inputEnv)

proc bindTerm[V, F](
  variable, value: Term[V, F], env: TermEnv[V, F] ,
  cb: TermImpl[V, F]): TermEnv[V, F] =
  ## Create environment where `variable` is bound to `value`
  result = env
  case cb.getKind(value):
    of tkConstant, tkVariable, tkPlaceholder:
      result[cb.getVName(variable)] = value
    of tkFunctor:
      let (newTerm, newEnv) = value.copy(env, cb)
      result = newEnv
      result[cb.getVName(variable)] = newTerm

proc dereference*[V, F](
  term: Term[V, F], env: TermEnv[V, F], cb: TermImpl[V, F]): Term[V, F]=
  ## Traverse binding chain in environment `env` and return value of
  ## the `term`
  result = term

  while cb.getKind(result) == tkVariable and isBound(env, cb.getVName(result)):
    let value = env[cb.getVName(result)]
    if cb.getKind(value) == tkConstant or value == result:
      result = value
      break

    result = value

proc unif*[V, F](
  t1, t2: Term[V, F],
  cb: TermImpl[V, F],
  env: TermEnv[V, F] = makeEnvironment[V, F]()
    ): Option[TermEnv[V, F]] =
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
      return none(TermEnv[V, F])
  elif k1 == tkVariable:
    return some(bindTerm(val1, val2, env, cb))
  elif k2 == tkVariable:
    return some(bindTerm(val2, val1, env, cb))
  elif (k1, k2) in @[(tkConstant, tkFunctor), (tkFunctor, tkConstant)]:
    return none(TermEnv[V, F])
  else:
    var tmpRes = env
    if cb.getFSym(val1) != cb.getFSym(val2):
      return none(TermEnv[V, F])

    if cb.getSubt(val1).len != cb.getSubt(val2).len:
      # TEST with different-sized term unification
      # TODO provide `reason` for failure
      return none(TermEnv[V, F])

    for idx, (arg1, arg2) in zip(cb.getSubt(val1), cb.getSubt(val2)):
      let res = unif(arg1, arg2, cb, tmpRes)
      if res.isSome():
        tmpRes = res.get()
      else:
        return none(TermEnv[V, F])

    return some(tmpRes)

iterator redexes*[V, F](
  term: Term[V, F], cb: TermImpl[V, F]): tuple[red: Term[V, F], path: TermPath] =
  ## Iterate over all redex in term
  var que: Deque[(Term[V, F], TermPath)]
  que.addLast((term, @[0]))
  while que.len > 0:
    let (nowTerm, path) = que.popFirst()
    if cb.getKind(nowTerm) == tkFunctor:
      for idx, subTerm in cb.getSubt(nowTerm):
        que.addLast((subTerm, path & @[idx]))

    yield (red: nowTerm, path: path)


proc varlist*[V, F](
  term: Term[V, F], cb: TermImpl[V, F], path: TermPath = @[0]): seq[(Term[V, F], TermPath)] =
  ## Output list of all variables in term
  case cb.getKind(term):
    of tkConstant, tkPlaceholder:
      return @[]
    of tkVariable:
      return @[(term, path)]
    of tkFunctor:
      for idx, sub in cb.getSubt(term):
        result &= sub.varlist(cb, path & @[idx])


proc setAtPath*[V, F](
  term: var Term[V, F], path: TermPath,
  value: Term[V, F], cb: TermImpl[V, F]): void =
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

proc substitute*[V, F](
  term: Term[V, F], env: TermEnv[V, F], cb: TermImpl[V, F]): Term[V, F] =
  ## Substitute all variables in term with their values from environment
  result = term
  for (v, path) in term.varlist(cb):
    if env.isBound(cb.getVName(v)):
      result.setAtPath(path, v.dereference(env, cb), cb)

proc treeRepr*[V, F](
  term: Term[V, F],
  cb: TermImpl[V, F],
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

proc reduce*[V, F](
  term: Term[V, F],
  system: RedSystem[V, F],
  cb: TermImpl[V, F],
  maxDepth: int = 40,
  maxIterations: int = 4000,
  reduceConstraints: ReduceConstraints = rcApplyOnce
                ): tuple[term: Term[V, F], ok: bool, rewPaths: Trie[int, IntSet]] =
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
  defer:
    result.rewPaths = rewPaths

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
          # echo "Testing path ", path
          for idx, rule in system:
            if (
              # Avoid using this rule again on the same path
              (reduceConstraints == rcApplyOnce) and
              (rewPaths.prefixHasValue(path)) and
              toSeq(rewPaths.prefixedValues(path)).anyOfIt(idx in it)
            ):
              # echo "===="
              # echo &"Skipping rule {idx} at path {path}"
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
                unifRes = unif(lhs.patt, redex, cb)
              of false:
                unifRes = lhs.matcher(redex)

            # Unification ok, calling generator proc to get replacement
            if unifRes.isSome():
              # echo "===="
              # echo &"Using rule {idx} at path {path}"
              # echo &"Reduction constraints: {reduceConstraints}"
              # echo "has prefix - ", rewPaths.prefixHasValue(path)
              # echo &"Known paths: "
              # for path in rewPaths.paths():
              #   echo &"Path: {path}, value: ", toSeq(rewPaths[path])

              # echo "(reduceConstraints == rcApplyOnce) and", (reduceConstraints == rcApplyOnce)
              # echo "(rewPaths.prefixHasValue(path)) and", (rewPaths.prefixHasValue(path))
              # echo "toSeq(rewPaths.parentValues(path)).anyOfIt(idx in it)", toSeq(rewPaths.prefixedValues(path)).anyOfIt(idx in it)

              case reduceConstraints:
                of rcApplyOnce:
                  if path notin rewPaths:
                    rewPaths[path] = IntSet()

                  rewPaths[path].incl idx
                of rcRewriteOnce:
                  rewPaths[path] = IntSet()

                else:
                  discard

              # echo "^^^^^^^^"
              let newEnv = unifRes.get()

              # New value from generator
              let tmpNew = (gen(newEnv)).substitute(newEnv, cb)
              setAtPath(tmpTerm, path, tmpNew, cb)

              if cb.getKind(tmpTerm) notin {tkVariable, tkConstant}:
                canReduce = true
                result.ok = true
              else:
                return (tmpTerm, true, rewPaths)

      if not canReduce:
        result.term = tmpTerm
        break
