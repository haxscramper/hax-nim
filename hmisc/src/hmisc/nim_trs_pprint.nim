import strutils, sequtils, strformat, sugar

import types/[hterm_buf, seq2d, hdrawing]
import helpers
import nim_trs
import hpprint

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

proc exprRepr*[V, F](matcher: TermMatcher[V, F], cb: TermImpl[V, F]): TermBuf =
  var tmp: Seq2D[TermBuf]
  tmp.appendRow(@[
     matcher.isPattern.tern(
       exprRepr(matcher.patt, cb), "proc"
     ).toTermBufFast()
  ], emptyTermBuf)

  for varn, subp in matcher.subpatts:
    tmp.appendRow(
      @[(varn & ": ").toTermBufFast(), subp.exprRepr(cb)],
      emptyTermBuf
    )

  result = tmp.toTermBuf()
  # echo "----- result: "
  # pprint result
  # echo "-----"


proc exprRepr*[V, F](env: TermEnv[V, F], cb: TermImpl[V, F]): string =
  "{" & env.mapPairs(
    &"({lhs.exprRepr()} -> {rhs.exprRepr(cb)})"
  ).join(" ") & "}"

proc exprReprImpl*[V, F](rule: RulePair[V, F], cb: TermImpl[V, F]): TermBuf =
  let rhs: TermBuf = rule.gen.isPattern.tern(
    exprRepr(rule.gen.patt),
    "proc"
  ).toTermBufFast()

  var matchers: Seq2D[TermBuf]
  for idx, match in rule.rules:
    let pref: string = if rule.rules.len == 1: "" else: $idx & ": "
    let bufs = @[pref.toTermBufFast(), match.exprRepr(cb)]
    # # pprint bufs
    # echo newTermGrid(
    #   (0,0),
    #   makeSeq2D(bufs),
    #   makeThinLineGridBorders()
    # ).toStringBlock().join("\n")

    matchers.appendRow(bufs, emptyTermBuf)


  @[
    matchers.toTermBuf(), (" ~~> ").toTermBufFast(), rhs
  ].toTermBuf()

proc exprRepr*[V, F](rule: RulePair[V, F], cb: TermImpl[V, F]): string =
  exprReprImpl(rule, cb).toString()

proc exprReprImpl*[V, F](sys: RedSystem[V, F], cb: TermImpl[V, F]): TermBuf =
  sys.mapPairs(@[
    ($idx & ": ").toTermBufFast(),
    rhs.exprReprImpl(cb)
  ]).toTermBuf()

proc exprRepr*[V, F](sys: RedSystem[V, F], cb: TermImpl[V, F]): string =
  exprReprImpl(sys, cb).toString()
