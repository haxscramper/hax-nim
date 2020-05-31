## Term algorithms

import hashes, sequtils, tables, strformat, strutils
import helpers

type
  Failure* = ref object of CatchableError
  TermKind* = enum
    tkVariable
    tkFunctor
    tkConstant
    tkPlaceholder

  Term*[Sym, Val] = object
    case kind: TermKind
      of tkVariable:
        name: Sym
        genIdx: int
      of tkFunctor:
        sym: Sym
        subt: seq[Term[Sym, Val]]
      of tkConstant:
        value: Val
      of tkPlaceholder:
        nil

    # makeFunctor(TermSymbol, seq[Term]) is Term
    # makeVariable(TermSymbol) is Term
    # makeConstant(TermValue) is Term

  TermEnv*[Sym, Val] = object
    values: Table[Term[Sym, Val], Term[Sym, Val]]

  RedSystem*[Sym, Val] = object
    rules: Table[Term[Sym, Val], Term[Sym, Val]]


proc makeEnvironment*[Sys, Val](
  values: seq[(Term[Sys, Val], Term[Sys, Val])] = @[]): TermEnv[Sys, Val] =
  ## Create new environment using `values` as initial binding values
  TermEnv[Sys, Val](values: values.toTable())

proc makeFunctor*[Sym, Val](
  sym: Sym, subt: seq[Term[Sym, Val]]): Term[Sym, Val] =
  Term[Sym, Val](kind: tkFunctor, subt: subt, sym: sym)

proc makeVariable*[Sym, Val](varName: Sym): Term[Sym, Val] =
  ## Create new variable
  Term[Sym, Val](kind: tkVariable, name: varName)

proc makeConstant*[Sym, Val](constValue: Val): Term[Sym, Val] =
  ## Create constant term
  Term[Sym, Val](kind: tkConstant, value: constValue)

proc makePlaceholder*[Sym, Val](): Term[Sym, Val] =
  Term[Sym, Val](kind: tkPlaceholder)

proc isBound*[Sym, Val](env: TermEnv[Sym, Val], term: Term[Sym, Val]): bool =
  (term in env.values) and env[term] != term

proc `[]`*[Sym, Val](
  e: TermEnv[Sym, Val], t: Term[Sym, Val]): Term[Sym, Val] = e[t]

proc `[]=`*[Sym, Val](
  system: var RedSystem[Sym, Val], lhs, rhs: Term[Sym, Val]): void =
  system.rules[lhs] = rhs

proc `[]=`*[Sym, Val](
  env: var TermEnv[Sym, Val], variable, value: Term[Sym, Val]): void =
  env.values[variable] = value

iterator pairs*[Sym, Val](system: RedSystem[Sym, Val]): tuple[lhs, rhs: Term[Sym, Val]] =
  for lhs, rhs in system.rules:
    yield (lhs, rhs)


iterator pairs*[Sym, Val](env: TermEnv[Sym, Val]): tuple[lhs, rhs: Term[Sym, Val]] =
  for lhs, rhs in env.values:
    yield (lhs, rhs)

proc `$`*[Sym, Val](term: Term[Sym, Val]): string =
  case term.kind:
    of tkConstant:
      "'" & $term.value & "'"
    of tkVariable:
      "_" & $term.name & "'".repeat(term.genIdx)
    of tkFunctor:
      if ($term.sym).validIdentifier():
        $term.sym & "(" & term.subt.mapIt($it).join(", ") & ")"
      else:
        case term.subt.len():
          of 1: &"{term.sym}({term.subt[0]})"
          of 2: &"({term.subt[0]} {term.sym} {term.subt[1]})"
          else:
            $term.sym & "(" & term.subt.mapIt($it).join(", ") & ")"
    of tkPlaceholder:
      "_"

proc `$`*[Sym, Val](env: TermEnv[Sym, Val]): string =
  "{" & toSeq(env.pairs()).mapIt(
    &"({it[0]} " & (
      (
        it[0].kind == tkVariable and
        it[1].kind == tkVariable
      ).tern("<->", "->")
    ) & &" {it[1]})"
  ).join(" ") & "}"



proc hash*[Sym, Val](t: Term[Sym, Val]): Hash =
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

func `==`*[Sym, Val](t1, t2: Term[Sym, Val]): bool =
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

proc copy*[Sym, Val](
  term: Term[Sym, Val], env: TermEnv[Sym, Val]
    ): (Term[Sym, Val], TermEnv[Sym, Val]) =
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
      var resFunctor = makeFunctor[Sym, Val](term.sym, @[])
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

proc dereference*[Sym, Val](
  term: Term[Sym, Val], env: TermEnv[Sym, Val]): Term[Sym, Val] =
  ## Traverse binding chain in environment `env` and return value of
  ## the `term`
  result = term

  while isBound(env, result):
    let value = env[result]
    if value.kind == tkConstant or value == result:
      result = value
      break

    result = value

proc unif*[Sym, Val](
  t1, t2: Term[Sym, Val], env: TermEnv[Sym, Val] = makeEnvironment[Sym, Val]()
    ): TermEnv[Sym, Val] =
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

proc reduce*[Sym, Val](
  term: Term[Sym, Val],
  system: RedSystem[Sym, Val]): (Term[Sym, Val], bool) =
  let tmpTerm = term
  for lhs, rhs in system:
    try:
      let newEnv = unif(lhs, tmpTerm)
      echo "term: ", term
      echo "can be reduced using: ", lhs
      echo "reduced into: ", rhs
      echo "subst. env: ", $newEnv
    except:
      echo "failed"
