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

    getVName*: proc(self: Obj): Sym
    getTsym*: proc(self: Obj): Sym
    getSubt*: proc(self: Obj): seq[Obj]
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
      let deref = term.dereference(env)
      if cb.getKind(deref) == tkVariable:
        var newVar = term
        # inc newVar.genIdx
        var resEnv = bindTerm(deref, newVar, env, cb)
        return (newVar, resEnv)
      else:
        return (deref, inputEnv)

    of tkFunctor:
      var resEnv = env
      var resFunctor = cb.makeFunctor(cb.getTSym(term), @[])
      for arg in term.subt:
        let (tmpArg, tmpEnv) = arg.copy(resEnv, cb)
        resEnv = tmpEnv
        resFunctor.subt.add tmpArg

      return (resFunctor, resEnv)

    of tkPlaceholder:
      return (term, inputEnv)

proc bindTerm[Obj, Sym, Val](
  variable, value: Obj, env: TermEnv[Obj] ,
  cb: TermImpl[Obj, Sym, Val]): TermEnv[Obj] =
  ## Create environment where `variable` is bound to `value`
  result = env
  case value.kind:
    of tkConstant, tkVariable, tkPlaceholder:
      result[variable] = value
    of tkFunctor:
      let (newTerm, newEnv) = value.copy(env, cb)
      result = newEnv
      result[variable] = newTerm

proc dereference*[Obj](
  term: Obj, env: TermEnv[Obj]): Obj =
  ## Traverse binding chain in environment `env` and return value of
  ## the `term`
  result = term

  while isBound(env, result):
    let value = env[result]
    if value.kind == tkConstant or value == result:
      result = value
      break

    result = value

proc unif*[Obj, Sym, Val](
  t1, t2: Obj,
  cb: TermImpl[Obj, Sym, Val],
  env: TermEnv[Obj] = makeEnvironment[Obj]()
    ): TermEnv[Obj] =
  let
    val1 = dereference(t1, env)
    val2 = dereference(t2, env)

  if val1.kind == tkConstant and val2.kind == tkConstant:
    if val1 == val2:
      return env
    else:
      raise Failure(msg: "Unification failed: different constants")
  elif val1.kind == tkVariable:
    return bindTerm(val1, val2, env, cb)
  elif val2.kind == tkVariable:
    return bindTerm(val2, val1, env, cb)
  elif (val1.kind, val2.kind) in @[(tkConstant, tkFunctor), (tkFunctor, tkConstant)]:
    raise Failure(msg: "Cannot unify consant and functor")
  else:
    result = env
    if val1.sym != val2.sym:
      raise Failure(
        msg: &"Cannot unify functors with different names '{t1}' and '{t2}'")

    for (arg1, arg2) in zip(val1.subt, val2.subt):
      result = unif(arg1, arg2, cb, result)

# proc match*[Obj](t1, t2: Term): TermEnv[Obj] =
#   case t1.kind:
#     of tkPlaceholder:
#       return makeEnvironment()
#     of tkVariable:

iterator redexes*[Obj](
  term: Obj): tuple[red: Obj, path: TermPath] =
  ## Iterate over all redex in term
  var que: Deque[(Obj, TermPath)]
  que.addLast((term, @[0]))
  while que.len > 0:
    let (nowTerm, path) = que.popFirst()

    if nowTerm.kind == tkFunctor:
      for idx, subTerm in nowTerm.subt:
        que.addLast((subTerm, path & @[idx]))

    yield (red: nowTerm, path: path)


proc varlist*[Obj](term: Obj, path: TermPath = @[0]): seq[(Obj, TermPath)] =
  ## Output list of all variables in term
  case term.kind:
    of tkConstant, tkPlaceholder:
      return @[]
    of tkVariable:
      return @[(term, path)]
    of tkFunctor:
      for idx, sub in term.subt:
        result &= sub.varlist(path & @[idx])


proc `[]=`*[Obj](term: var Obj, path: TermPath, value: Obj): void =
  case term.kind:
    of tkFunctor:
      if path.len == 1:
        term = value
      else:
        term.subt[path[1]][path[1..^1]] = value
    of tkVariable:
      assert (path.len == 1)
      term = value
    of tkPlaceholder:
      assert false, "Cannot assign to placeholder"
    of tkConstant:
      assert false, "Cannot assign to constant"

proc substitute*[Obj](
  term: Obj, env: TermEnv[Obj]): Obj =
  ## Substitute all variables in term with their values from environment
  result = term
  for (v, path) in term.varlist():
    if env.isBound(v):
      result[path] = v.dereference(env)


proc reduce*[Obj, Sym, Val](
  term: Obj,
  system: RedSystem[Obj],
  cb: TermImpl[Obj, Sym, Val]
                ): (Obj, bool) =
  var tmpTerm = term
  while true:
    var canReduce = false
    for (redex, path) in tmpTerm.redexes():
      for lhs, rhs in system:
        try:
          let newEnv = unif(lhs, redex, cb)
          let tmpNew = rhs.substitute(newEnv)
          # echo tmpTerm, " $ ", lhs, " -> ", rhs, " into ", tmpNew
          # echo "with: ", newEnv
          tmpTerm[path] = tmpNew
          # tmpTerm = tmpNew
          if tmpTerm.kind notin {tkVariable, tkConstant}:
            canReduce = true
            result[1] = true
          else:
            return (tmpTerm, true)
        except Failure:
          discard

    if not canReduce:
      result[0] = tmpTerm
      break
