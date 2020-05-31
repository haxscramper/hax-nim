## Term algorithms

import hashes, sequtils, tables

type
  Failure* = ref object of CatchableError

  TermSymbol* = concept x
    $x is string

  TermValue* = concept x, y
    x == y is bool

  TermKind* = enum
    tkVariable
    tkFunctor
    tkConstant
    tkPlaceholder

  Term* = concept x
    x.name is TermSymbol
    x.sym is TermSymbol
    x.subt is seq[typeof(x)]
    x.kind is TermKind
    x.value is TermValue
    x.genIdx is int

    # makeFunctor(TermSymbol, seq[Term]) is Term
    # makeVariable(TermSymbol) is Term
    # makeConstant(TermValue) is Term

  TermEnv*[TImpl] = object
    values: Table[TImpl, TImpl]

proc makeFunctor[T: Term, S: TermSymbol](sym: S, subt: seq[T]): T =
  result = T(kind: tkFunctor)
  result.sym = sym
  result.subt = subt

proc isBound*[T: Term](env: TermEnv[T], term: T): bool =
  (term in env.values) and env[term] != term

proc `[]`*[T: Term](e: TermEnv[T], t: T): T = e[t]

proc `[]=`*[T: Term](env: var TermEnv[T], variable, value: T): void =
  env.values[variable] = value


proc hash*[T: Term](t: T): Hash =
  ## Hash for term.
  # XXXX NOTE if performance issues are encountered this might be the
  # first proc to optimize.
  var h: Hash = 0
  h = h !& int(t.kind)
  case t.kind:
    of tkVariable:
      h = h !& hash(t.name) !& hash(t.genIdx)
    of tkConstant:
      h = h !& hash(t.value)
    of tkFunctor:
      for arg in t.subt:
        h = h !& hash(arg)
    of tkPlaceholder:
      discard

  result = !$h

func `==`*[T: Term](t1, t2: T): bool =
  ## Check if two terms are **identical**, regardless of the
  ## environemtn value.
  if t1.kind != t2.kind:
    return false

  case t1.kind:
    of tkConstant:
      return t1.value == t2.value
    of tkVariable:
      return t1.name == t2.name
    of tkFunctor:
      if t1.sym == t2.sym and t1.subt.len() == t2.subt.len():
        for (arg1, arg2) in zip(t1.subt, t2.subt):
          if arg1 != arg2:
            return false

        return true
      else:
        return false

    of tkPlaceholder:
      return true # XXXX


proc bindTerm[T: Term, E: TermEnv](variable, value: T, env: E): E

proc copy*[T: Term, E: TermEnv](term: T, env: E): (T, E) =
  ## Create copy of a term. All variables are replaced with new ones.
  let inputEnv = env
  case term.kind:
    of tkConstant:
      return (term, inputEnv)
    of tkVariable:
      let deref = term.dereference(env)
      if deref.kind == tkVariable:
        var newVar = term
        inc newVar.genIdx
        var resEnv = bindTerm(deref, newVar, env)
        return (newVar, resEnv)
      else:
        return (deref, inputEnv)

    of tkFunctor:
      var resEnv = env
      var resFunctor = makeFunctor(term.sym, @[])
      for arg in term.subt:
        let (tmpArg, tmpEnv) = arg.copy(resEnv)
        resEnv = tmpEnv
        resFunctor.subt.add tmpArg

      return (resFunctor, resEnv)

    of tkPlaceholder:
      return (term, inputEnv)

proc bindTerm[T: Term, E: TermEnv](variable, value: T, env: E): E =
  ## Create environment where `variable` is bound to `value`
  result = env
  case value.kind:
    of tkConstant, tkVariable, tkPlaceholder:
      result[variable] = value
    of tkFunctor:
      let (newTerm, newEnv) = value.copy(env)
      result = newEnv
      result[variable] = newTerm

proc dereference[T: Term, E: TermEnv](term: T, env: E): T =
  ## Traverse binding chain in environment `env` and return value of
  ## the `term`
  result = term

  while isBound(env, result):
    let value = env[result]
    if value.kind == tkConstant or value == result:
      result = value
      break

    result = value

proc unif*[T: Term, E: TermEnv](t1, t2: T, env: E): E =
  let
    val1 = dereference(t1, env)
    val2 = dereference(t2, env)

  if val1.kind == tkConstant and val2.kind == tkConstant:
    if val1 == val2:
      return env
    else:
      raise Failure(msg: "Unification failed: different constants")
  elif val1.kind == tkVariable:
    return bindTerm(val1, val2, env)
  elif val2.kind == tkVariable:
    return bindTerm(val2, val1, env)
  else:
    result = env
    if val1.sym != val2.sym:
      raise Failure(
        msg: &"Cannot unify functors with different names '{t1}' and '{t2}'")

    for (arg1, arg2) in zip(val1.subt, val2.subt):
      result = unif(arg1, arg2, result)
