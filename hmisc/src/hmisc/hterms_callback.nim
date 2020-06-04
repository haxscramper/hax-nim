## Term algorithms. Implmenetation uses callback functions for getting
## values/types from terms.

import hashes, sequtils, tables, strformat, strutils
import helpers, deques

type
  Failure* = ref object of CatchableError
  TermKind* = enum
    tkVariable
    tkFunctor
    tkConstant
    tkPlaceholder

  TermPath = seq[int]
  TermImpl*[Obj, Sym, Val] = object

    getKind*: proc(self: Obj): TermKind
    setNth*: proc(self: var Obj, idx: int, value: Obj): void
    getNth*: proc(self: Obj, idx: int): Obj
    getNthMod*: proc(self: var Obj, idx: int): var Obj

    getVName*: proc(self: Obj): Sym
    getTsym*: proc(self: Obj): Sym
    getSubt*: proc(self: Obj): seq[Obj]
    setSubt*: proc(self: var Obj, subt: seq[Obj]): void
    getValue*: proc(self: Obj): Val

    unifCheck*: proc(self, other: Obj): bool ## | Procedure to quickly
    ## check if two objects can be unified at all

    makePlaceholder*: proc(): Obj
    makeConstant*: proc(val: Val): Obj
    makeVariable*: proc(name: Sym): Obj
    makeFunctor*: proc(sym: Sym, subt: seq[Obj]): Obj

  TermEnv*[Obj] = object
    values*: Table[Obj, Obj]

  RedSystem*[Obj] = object
    rules*: Table[Obj, Obj]

proc assertCorrect*[Obj, Sym, Val](impl: TermImpl[Obj, Sym, Val]): void =
  for name, value in impl.fieldPairs():
    assert (not value.isNil()), name & " cannot be nil"

proc makeEnvironment*[Obj](values: seq[(Obj, Obj)] = @[]): TermEnv[Obj] =
  ## Create new environment using `values` as initial binding values
  TermEnv[Obj](values: values.toTable())

proc isBound*[Obj](env: TermEnv[Obj], term: Obj): bool =
  (term in env.values) and env[term] != term

proc `[]`*[Obj](e: TermEnv[Obj], t: Obj): Obj = e.values[t]

proc `[]=`*[Obj](system: var RedSystem[Obj], lhs, rhs: Obj): void =
  system.rules[lhs] = rhs

proc `[]=`*[Obj](env: var TermEnv[Obj], variable, value: Obj): void =
  env.values[variable] = value

iterator pairs*[Obj](system: RedSystem[Obj]): tuple[lhs, rhs: Obj] =
  for lhs, rhs in system.rules:
    yield (lhs, rhs)


iterator pairs*[Obj](env: TermEnv[Obj]): tuple[lhs, rhs: Obj] =
  for lhs, rhs in env.values:
    yield (lhs, rhs)

# func `==`*[Obj](t1, t2: Obj): bool =
#   ## Check if two terms are **identical**, regardless of the
#   ## environemtn value.
#   if t1.kind != t2.kind:
#     return false

#   case t1.kind:
#     of tkConstant:
#       return t1.value == t2.value
#     of tkVariable:
#       return t1.name == t2.name
#     of tkFunctor:
#       if t1.sym == t2.sym and t1.subt.len() == t2.subt.len():
#         for (arg1, arg2) in zip(t1.subt, t2.subt):
#           if arg1 != arg2:
#             return false

#         return true
#       else:
#         return false

#     of tkPlaceholder:
#       return true # XXXX


proc bindTerm[Obj, Sym, Val](
  variable, value: Obj,
  env: TermEnv[Obj],
  cb: TermImpl[Obj, Sym, Val]): TermEnv[Obj]

proc copy*[Obj, Sym, Val](
  term: Obj, env: TermEnv[Obj], cb: TermImpl[Obj, Sym, Val]): (Obj, TermEnv[Obj]) =
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

      return (cb.makeFunctor(cb.getTSym(term), subterms), resEnv)

    of tkPlaceholder:
      return (term, inputEnv)

proc bindTerm[Obj, Sym, Val](
  variable, value: Obj, env: TermEnv[Obj] ,
  cb: TermImpl[Obj, Sym, Val]): TermEnv[Obj] =
  ## Create environment where `variable` is bound to `value`
  result = env
  case cb.getKind(value):
    of tkConstant, tkVariable, tkPlaceholder:
      result[variable] = value
    of tkFunctor:
      let (newTerm, newEnv) = value.copy(env, cb)
      result = newEnv
      result[variable] = newTerm

proc dereference*[Obj, Sym, Val](
  term: Obj, env: TermEnv[Obj], cb: TermImpl[Obj, Sym, Val]): Obj =
  ## Traverse binding chain in environment `env` and return value of
  ## the `term`
  result = term

  while isBound(env, result):
    let value = env[result]
    if cb.getKind(value) == tkConstant or value == result:
      result = value
      break

    result = value

proc unif*[Obj, Sym, Val](
  t1, t2: Obj,
  cb: TermImpl[Obj, Sym, Val],
  env: TermEnv[Obj] = makeEnvironment[Obj]()
    ): TermEnv[Obj] =
  let
    val1 = dereference(t1, env, cb)
    val2 = dereference(t2, env, cb)
    k1 = cb.getKind(val1)
    k2 = cb.getKind(val2)

  if k1 == tkConstant and k2 == tkConstant:
    if val1 == val2:
      return env
    else:
      raise Failure(msg: "Unification failed: different constants")
  elif k1 == tkVariable:
    return bindTerm(val1, val2, env, cb)
  elif k2 == tkVariable:
    return bindTerm(val2, val1, env, cb)
  elif (k1, k2) in @[(tkConstant, tkFunctor), (tkFunctor, tkConstant)]:
    raise Failure(msg: "Cannot unify consant and functor")
  else:
    result = env
    if cb.getTSym(val1) != cb.getTSym(val2):
      raise Failure(
        msg: &"Cannot unify functors with different names '{t1}' and '{t2}'")

    for (arg1, arg2) in zip(cb.getSubt(val1), cb.getSubt(val2)):
      result = unif(arg1, arg2, cb, result)

# proc match*[Obj](t1, t2: Term): TermEnv[Obj] =
#   case t1.kind:
#     of tkPlaceholder:
#       return makeEnvironment()
#     of tkVariable:

iterator redexes*[Obj, Sym, Val](term: Obj, cb: TermImpl[Obj, Sym, Val]
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


proc varlist*[Obj, Sym, Val](
  term: Obj, cb: TermImpl[Obj, Sym, Val], path: TermPath = @[0]): seq[(Obj, TermPath)] =
  ## Output list of all variables in term
  case cb.getKind(term):
    of tkConstant, tkPlaceholder:
      return @[]
    of tkVariable:
      return @[(term, path)]
    of tkFunctor:
      for idx, sub in cb.getSubt(term):
        result &= sub.varlist(cb, path & @[idx])


proc setAtPath*[Obj, Sym, Val](
  term: var Obj, path: TermPath, value: Obj, cb: TermImpl[Obj, Sym, Val]): void =
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
      assert (path.len == 1)
      term = value
    of tkPlaceholder:
      assert false, "Cannot assign to placeholder: " & $term & " = " & $value
    of tkConstant:
      assert false, "Cannot assign to constant: " & $term & " = " & $value

proc substitute*[Obj, Sym, Val](
  term: Obj, env: TermEnv[Obj], cb: TermImpl[Obj, Sym, Val]): Obj =
  ## Substitute all variables in term with their values from environment
  result = term
  for (v, path) in term.varlist(cb):
    if env.isBound(v):
      result.setAtPath(path, v.dereference(env, cb), cb)


proc reduce*[Obj, Sym, Val](
  term: Obj,
  system: RedSystem[Obj],
  cb: TermImpl[Obj, Sym, Val]
                ): (Obj, bool) =
  var tmpTerm = term
  while true:
    var canReduce = false
    for (redex, path) in tmpTerm.redexes(cb):
      for lhs, rhs in system:
        try:
          let newEnv = unif(lhs, redex, cb)
          let tmpNew = rhs.substitute(newEnv, cb)
          # echo tmpTerm, " $ ", lhs, " -> ", rhs, " into ", tmpNew
          # echo "with: ", newEnv
          setAtPath(tmpTerm, path, tmpNew, cb)
          # tmpTerm[path] = tmpNew
          # tmpTerm = tmpNew
          if cb.getKind(tmpTerm) notin {tkVariable, tkConstant}:
            canReduce = true
            result[1] = true
          else:
            return (tmpTerm, true)
        except Failure:
          discard

    if not canReduce:
      result[0] = tmpTerm
      break