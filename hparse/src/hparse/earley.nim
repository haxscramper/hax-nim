import lexer, token, parse_tree, bnf_grammars, grammars
import hashes, sugar, options, algorithm, tables

import strformat, strutils, sequtils, sets
import hmisc/helpers

type
  EItem = object
    ruleId: RuleId
    startPos: int
    nextPos: int

  SItemId = object
    ruleId: RuleId
    finish: int

  State = seq[seq[EItem]]
  Chart = seq[seq[SItemId]]
  NullSet = object
    nulls: HashSet[BnfNterm]

  EarleyParser[C, L] = object
    start: BnfNterm
    grammar: BnfGrammar[C, L]

#*************************************************************************#
#**************************  Helper functions  ***************************#
#*************************************************************************#

func matches[C, L, I](sym: FlatBnf[C, L],
                      toks: TokStream[Token[C, L, I]],
                      pos: int): bool =
  discard


func nullableSymbols[C, L](gr: BnfGrammar[C, L]): NullSet =
  # TODO use null set construction from ll1 table parser
  discard

func nextSymbol[C, L](gr: BnfGrammar[C, L], item: EItem): Option[FlatBnf[C, L]] =
  if gr.ruleBody(item.ruleId).len > item.nextPos:
    some(gr.ruleBody(item.ruleId)[item.nextPos])
  else:
    none(FlatBnf[C, L])

#*************************************************************************#
#***************************  Pretty-printing  ***************************#
#*************************************************************************#


proc printChart[C, L](gr: BnfGrammar[C, L], state: Chart): void =
  echo "\e[31mCHART :\e[39m"
  for idx, stateset in state:
    echo fmt("\e[36mSTARTS:\e[39m {idx}")
    for item in stateset:
      var buf = fmt("{item.ruleId.exprRepr():<12}") & " ->"
      for idx, sym in gr.ruleBody(item.ruleId):
        if sym.isTerm:
          buf &= fmt(" {sym.terminal.lex:>8}")
        else:
          buf &= fmt(" {sym.nterm:>8}")

      buf = fmt("\e[32mEND   :\e[39m {item.finish} {buf:<60}")

      echo buf
    echo ""

proc printItems[C, L](gr: BnfGrammar[C, L], state: State, onlyFull: bool = false): void =
  for idx, stateset in state:
    echo fmt("   === {idx:^3} ===   ")
    for item in stateset:
      if (item.nextPos == gr.ruleBody(item.ruleId).len) or (not onlyFull):
        var buf = fmt("{gr.ruleName(item.ruleId):<12}") & " ->"
        for idx, sym in gr.ruleBody(item.ruleId):
          if idx == item.nextPos:
            buf &= " •"

          if sym.isTerm:
            buf &= " " & sym.terminal.lex
          else:
            buf &= " " & sym.nterm

        if item.nextPos == gr.ruleBody(item.ruleId).len:
          buf = fmt("{buf:<60} \e[4m#\e[24m ({item.startPos})")
        else:
          buf = fmt("{buf:<60}   ({item.startPos})")

        echo buf

func printTreeRepr[C, L, I](pt: ParseTree[C, L, I], level: int = 0): string =
  let pref = "  ".repeat(level)
  if pt.isToken:
   echo "[*]" & pref & $pt.token
  else:
    let rulestr = "" # TODO  pt.ruleId.exprRepr()
    echo fmt("[{pt.subnodes.len}] {pref}{rulestr}")
    for sub in pt.subnodes:
      printTreeRepr(sub, level + 1)

#*************************************************************************#
#**************************  Item construction  **************************#
#*************************************************************************#

func predict[C, L](state: var State,
                   i, j: int,
                   nullable: NullSet,
                   symbol: FlatBnf[C, L],
                   gr: BnfGrammar[C, L]): void =
  discard

func scan[C, L, I](state: var State,
                   i, j: int,
                   symbol: FlatBnf[C, L],
                   gr: BnfGrammar[C, L],
                   toks: TokStream[Token[C, L, I]]): void =
  if symbol.matches(toks, i):
    if state.len - 1 <= i:
      state.add @[]

    state[i + 1].add state[i][j].withIt do:
      inc it.nextPos

func complete[C, L, I](state: var State,
                       i, j: int,
                       gr: BnfGrammar[C, L],
                       toks: TokStream[Token[C, L, I]]): void =
  discard

func buildItems[C, L, I](parser: EarleyParser[C, L],
                         toks: TokStream[Token[C, L, I]]): State =
  let nullable = nullableSymbols(parser.grammar)
  var state: State
  for ruleId in parser.grammar.iterrules(parser.start):
    state.add @[EItem(ruleId: ruleId, startPos: 0, nextPos: 0)]

  var itemset = 0
  while itemset < state.len:
    var j = 0
    while j < state[itemset].len:
      let next: Option[FlatBnf[C, L]] = parser.grammar.nextSymbol(
        state[itemset][j])

      if next.isNone():
        complete(state, itemset, j, parser.grammar, toks)
      else:
        let sym: FlatBnf[C, L] = next.get()
        if sym.isTerm:
          scan(state, itemset, j, sym, parser.grammar, toks)
        else:
          predict(state, itemset, j, nullable, sym, parser.grammar)
      inc j
    inc itemset


func chartOfItems[C, L](grammar: BnfGrammar[C, L],
                        state: State): Chart =
  result = state.mapIt(newSeqWith(0, SItemId()))
  for idx, itemset in state:
    for item in itemset:
      let sym: Option[FlatBnf[C, L]] = grammar.nextSymbol(item)
      if sym.isSome():
        discard
      else:
        result[item.startPos].add SItemId(
          ruleId: item.ruleId,
          finish: idx
        )

  for edgeset in mitems(result):
    edgeset.sort do(e2, e1: SItemId) -> int:
      if e1.ruleId == e2.ruleId:
        e2.finish - e1.finish
      else:
        # e2.ruleId - e1.ruleId # FIXME comparison of rule id items
        # does not have direct meaning if they are not indexed
        # globally.
        -1


#*************************************************************************#
#***********************  Parse tree construction  ***********************#
#*************************************************************************#


type
  TryParams = object
    start: int
    rule: RuleId

func hash*(pr: TryParams): Hash = !$(pr.start !& hash(pr.rule))

func parseTree[C, L, I](gr: BnfGrammar[C, L],
                        toks: TokStream[Token[C, L, I]],
                        chart: Chart): seq[ParseTree[C, L, I]] =

  var tried: Table[TryParams, Option[ParseTree[C, L, I]]]
  proc aux(start, finish: int, name: BnfNterm): Option[ParseTree[C, L, I]] =
    let alts: seq[RuleId] = collect(newSeq):
      for rule in chart[start]:
        let params = TryParams(start: start, rule: rule.ruleId)
        if (params in tried) and tried[params].isSome():
            return tried[params]

        if (rule.ruleId.head == name) and (params notin tried):
          rule.ruleId

    for alt in alts:
      let params = TryParams(start: start, rule: alt)
      tried[params] = none(ParseTree[C, L, I])
      var currpos: int = start
      var matchOk: bool = true

      block ruleTry:
        let symbols = gr.ruleBody(alt)
        let singletok = (symbols.len == 1) and (symbols[0].isTerm)
        if not singletok:
          result = some(ParseTree[C, L, I](
            kind: ptkNTerm, subnodes: @[], start: currpos
            # ruleId: alt, # TODO store rule name
          ))

        for idx, sym in gr.ruleBody(alt):
          if sym.isTerm:
            if sym.matches(toks, currpos):
              let tree = ParseTree[C, L, I](
                kind: ptkTerm,
                start: currpos,
                finish: currpos + 1,
                tok: toks[currpos]
              )

              if singletok:
                return some(tree)
              else:
                result.get().subnodes.add tree
                inc result.get().finish

              inc currpos
            else:
              matchOk = false
              break ruleTry
          else:
            let res = aux(currpos, finish, sym.nterm)
            if res.isSome():
              currpos = res.get().finish
              result.get().subnodes.add res.get()
              result.get().finish = currpos
            else:
              matchOk = false
              break ruleTry

      if matchOk:
        tried[params] = result
        return

  for ssetItem in chart[0]:
    if ssetItem.finish == (chart.len - 1) and
      ssetItem.ruleId.head == gr.start:
      let tree = aux(0, chart.len - 1, gr.start)
      if tree.isSome():
        return @[ tree.get() ]

#*************************************************************************#
#*********************************  API  *********************************#
#*************************************************************************#

func newEarleyParser*[C, L](grammar: Grammar[C, L]): EarleyParser[C, L] =
  let bnfg = grammar.toBNF()
  result.grammar = bnfg
  result.start = bnfg.start


func parse*[C, L, I](parser: EarleyParser[C, L],
                     toks: TokStream[Token[C, L, I]]): seq[ParseTree[C, L, I]] =
  let state = buildItems(parser, toks)
  {.noSideEffect.}:
    parser.grammar.printItems(state)

  let chart = chartOfItems(parser.grammar, state)
  {.noSideEffect.}:
    parser.grammar.printChart(chart)
  return parseTree(parser.grammar, toks, chart)
