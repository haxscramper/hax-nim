import macros, options

when false:
  # using `tuple` for type is not recommenede as it causes `nim object
  # constructor needs an object type` error

  type
    TermEnv*[T1] = object
      discard

    TermMatcher* = object
      discard

    RulePair*[VarSym, Obj] = object
      rule*: TermMatcher
      gen*: proc(env: TermEnv[VarSym]): Obj

  proc makeRulePair*[VarSym, Obj](
    rule: TermMatcher,
    gen: proc(env: TermEnv[VarSym]): Obj): RulePair[VarSym, Obj] =
    RulePair[VarSym, Obj](rule: rule, gen: gen)

  echo makeRulePair[int, float](
    rule = TermMatcher(),
    gen = proc(env: TermEnv[int]): float =
            discard
  )
