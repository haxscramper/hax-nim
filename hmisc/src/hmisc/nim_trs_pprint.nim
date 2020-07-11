import strutils, sequtils, strformat, sugar

import types/hdrawing
import helpers
import nim_trs

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

proc treeRepr*[V, F](val: V, cb: TermImpl[V, F], depth: int = 0): string =
  let ind = "  ".repeat(depth)
  if cb.isFunctor(val):
    return (
      @[ ind & "fun " & $(cb.getSym(val)) ] &
      cb.getSubt(val).mapIt(treeRepr(it, cb, depth + 1))
    ).join("\n")
  else:
    return cb.valStrGen(val)
      .split("\n").mapIt(ind & "cst " & it).join("\n")

proc exprRepr*(vs: VarSym): string = "_" & vs
proc exprRepr*[V, F](term: Term[V, F], cb: TermImpl[V, F]): string =
  case term.getKind():
    of tkConstant:
      "'" & cb.valStrGen(term.getValue()) & "'"
    of tkVariable:
      "_" & $term.getVName()
    of tkFunctor:
      if ($getSym(term)).validIdentifier():
        $getSym(term) & "(" & term.getSubt().mapIt(it.exprRepr(cb)).join(", ") & ")"
      else:
        let subt = term.getSubt()
        case subt.len():
          of 1: &"{term.getSym()}({subt[0]})"
          of 2: &"{subt[0]} {term.getSym()} {subt[1]}"
          else:
            $term.getSym() & "(" & subt.mapIt(it.exprRepr(cb)).join(", ") & ")"
    of tkPlaceholder:
      "_"

proc exprRepr*[V, F](matcher: TermMatcher[V, F], cb: TermImpl[V, F]): string =
  var top: seq[string] = case matcher.isPattern:
    of true: @[exprRepr(matcher.patt, cb)]
    of false: @["proc" ]

  for varn, subp in matcher.subpatts:
    top &= (@[@[varn, ": ", subp.exprRepr(cb)]]).toStringBlock()

  result = top.joinl


proc exprRepr*[V, F](env: TermEnv[V, F], cb: TermImpl[V, F]): string =
  "{" & env.mapPairs(
    &"({lhs.exprRepr()} -> {rhs.exprRepr(cb)})"
  ).join(" ") & "}"

proc exprRepr*[V, F](rule: RulePair[V, F], cb: TermImpl[V, F]): string =
  let rhs =
    case rule.gen.isPattern:
      of true: exprRepr(rule.gen.patt)
      of false: "proc"

  var matchers: seq[seq[string]] = collect(newSeq):
    for idx, match in rule.rules:
      let pref = if rule.rules.len == 1: "" else: $idx & ": "
      @[pref] & match.exprRepr(cb)

  @[@[
    matchers.toStringBlock().joinl(), " ~~> ", rhs
  ]].toStringBlock().joinl()

proc exprRepr*[V, F](sys: RedSystem[V, F], cb: TermImpl[V, F]): string =
  sys.mapPairs(@[$idx & ": ", rhs.exprRepr(cb)]).toStringBlock().joinl()
