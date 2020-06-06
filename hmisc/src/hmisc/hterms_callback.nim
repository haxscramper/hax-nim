## Term algorithms. Implmenetation uses callback functions for getting
## values/types from terms.

import hashes, sequtils, tables, strformat, strutils
import helpers, deques, intsets

import htrie

type
  TermKind* = enum
    tkVariable
    tkFunctor
    tkConstant
    tkPlaceholder

  TermPath = seq[int]
  TermImpl*[Obj, VarSym, FunSym, Val] = object

    getKind*: proc(self: Obj): TermKind
    setNth*: proc(self: var Obj, idx: int, value: Obj): void
    getNth*: proc(self: Obj, idx: int): Obj
    getNthMod*: proc(self: var Obj, idx: int): var Obj

    getVName*: proc(self: Obj): VarSym
    getFSym*: proc(self: Obj): FunSym # XXX change to `getFSym`
    getSubt*: proc(self: Obj): seq[Obj]
    setSubt*: proc(self: var Obj, subt: seq[Obj]): void
    getValue*: proc(self: Obj): Val

    unifCheck*: proc(self, other: Obj): bool ## | Procedure to quickly
    ## check if two objects can be unified at all

    makePlaceholder*: proc(): Obj
    makeConstant*: proc(val: Val): Obj
    makeVariable*: proc(name: VarSym): Obj
    makeFunctor*: proc(sym: FunSym, subt: seq[Obj]): Obj
    valStrGen*: proc(val: Val): string

  TermEnv*[VarSym, Obj] = object
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
  for name, value in impl.fieldPairs():
    assert (not value.isNil()), name & " cannot be nil"

proc makeRulePair*[VarSym, Obj](
  rule: TermMatcher[VarSym, Obj],
  gen: proc(env: TermEnv[VarSym, Obj]): Obj): RulePair[VarSym, Obj] =
  RulePair[VarSym, Obj](rule: rule, gen: gen)

proc makePattern*[VarSym, Obj](obj: Obj): TermMatcher[VarSym, Obj] =
  TermMatcher[VarSym, Obj](patt: obj, isPattern: true)

proc makeMatcher*[VarSym, Obj](
  matcher: proc(test: Obj): Option[TermEnv[VarSym, Obj]]): TermMatcher[VarSym, Obj] =
  TermMatcher[VarSym, Obj](isPattern: false, matcher: matcher)

proc makeGenerator*[VarSym, Obj](obj: Obj): proc(env: TermEnv[VarSym, Obj]): Obj =
  return proc(env: TermEnv[VarSym, Obj]): Obj =
    return obj

proc makeEnvironment*[VarSym, Obj](values: seq[(VarSym, Obj)] = @[]): TermEnv[VarSym, Obj] =
  ## Create new environment using `values` as initial binding values
  TermEnv[VarSym, Obj](values: values.toTable())

proc isBound*[VarSym, Obj](env: TermEnv[VarSym, Obj], term: VarSym): bool =
  (term in env.values) # and env[term] != term

proc `[]`*[VarSym, Obj](e: TermEnv[VarSym, Obj], t: VarSym): Obj = e.values[t]

proc `[]=`*[VarSym, Obj](system: var RedSystem[VarSym, Obj], lhs, rhs: Obj): void =
  system.rules[lhs] = rhs

proc `[]=`*[VarSym, Obj](
  env: var TermEnv[VarSym, Obj], variable: VarSym, value: Obj): void =

  env.values[variable] = value

iterator items*[VarSym, Obj](system: RedSystem[VarSym, Obj]):
         RulePair[VarSym, Obj] =
  for pair in system.rules:
    yield pair

iterator pairs*[VarSym, Obj](system: RedSystem[VarSym, Obj]):
         (int, RulePair[VarSym, Obj]) =
  for idx, pair in system.rules:
    yield (idx, pair)


iterator pairs*[VarSym, Obj](
  env: TermEnv[VarSym, Obj]): (VarSym, Obj) =
  for lhs, rhs in pairs(env.values):
    yield (lhs, rhs)

proc len*[VarSym, Obj](env: TermEnv[VarSym, Obj]): int =
  env.values.len()

proc bindTerm[Obj, VarSym, FunSym, Val](
  variable, value: Obj,
  env: TermEnv[VarSym, Obj],
  cb: TermImpl[Obj, VarSym, FunSym, Val]): TermEnv[VarSym, Obj]

proc copy*[Obj, VarSym, FunSym, Val](
  term: Obj, env: TermEnv[VarSym, Obj], cb: TermImpl[Obj, VarSym, FunSym, Val]): (Obj, TermEnv[VarSym, Obj]) =
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

    for (arg1, arg2) in zip(cb.getSubt(val1), cb.getSubt(val2)):
      let res = unif(arg1, arg2, cb, tmpRes)
      if res.isSome():
        tmpRes = res.get()
      else:
        return none(TermEnv[VarSym, Obj])

    return some(tmpRes)

iterator redexes*[Obj, VarSym, FunSym, Val](term: Obj, cb: TermImpl[Obj, VarSym, FunSym, Val]
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
              # echo "rule #", idx, " can be applied on path ", path
              case reduceConstraints:
                of rcApplyOnce:
                  if path notin rewPaths:
                    # echo "adding value to path ", path
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
