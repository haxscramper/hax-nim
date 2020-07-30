import lexer, token, parse_tree, bnf_grammars, options, algorithm, tables
import hashes, sugar

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
#**************************  Item construction  **************************#
#*************************************************************************#

func predict[C, L, I](state: var State,
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
  let nullable = nullableSymbols(gr: BnfGrammar[C, L])
  var state: State
  for ruleId, patt in parser.grammar.iterrules(parser.start):
    state.add @[EItem(ruleId: RuleId, startPos: 0, nextPos: 0)]

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
        case sym.kind:
          of fbkTerm:
            scan(state, itemset, j, sym, parser.grammar, toks)
          of fbkNterm:
            predict(state, itemset, j, nullable, sym, parser.grammar)
          else:
            # REFACTOR remove `fbkEmpty`
            discard


func chartOfItems[C, L, I](grammar: BnfGrammar[C, L],
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
        e2.ruleId - e1.ruleId


#*************************************************************************#
#***********************  Parse tree construction  ***********************#
#*************************************************************************#


type
  TryParams = object
    start: int
    altId: int
    name: string

func hash(pr: TryParams): Hash = !$(pr.start !& pr.altId !& hash(pr.name))

func parseTree[C, L, I](gr: BnfGrammar[C, L],
                        toks: TokStream[Token[C, L, I]],
                        chart: Chart): seq[ParseTree[C, L, I]] =
  var tried: Table[TryParams, Option[ParseTree[C, L, I]]]
  proc aux(start, finish: int, name: BnfNterm): Option[ParseTree[C, L, I]] =
    let alts: seq[RuleId] = collect(newSeq):
      for rule in chart[start]:
        let params = TryParams(start: start, altId: rule.ruleId, name: name)
        if (params in tried):
          if tried[params].isSome():
            return tried[params]

        if (rule.ruleId.head == name) and (params notin tried):
          rule.ruleId

    for alt in alts:
      let params = TryParams(start: start, altId: alt, name: name)
      tried[params] = none(ParseTree[C])
      var currpos: int = start
      var matchOk: bool = true

      block ruleTry:
        let symbols = gr.ruleBody(alt)
        let singletok = (symbols.len == 1) and (symbols[0].isTerm)
        if not singletok:
          result = some(ParseTree[C](
            isToken: false,
            ruleId: alt,
            subnodes: @[],
            start: currpos))

        for idx, sym in gr.ruleBody(alt):
          if sym.isTerm:
            if sym.matches(toks, currpos):
              let tree = ParseTree[C](
                isToken: true,
                start: currpos,
                finish: currpos + 1,
                token: toks[currpos]
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
        triedTable[params] = result
        return 

  for ssetItem in chart[0]: 
    if ssetItem.finish == (chart.len - 1) and
      ssetItem.ruleId.head == gr.start:
      let tree = aux(0, chart.len - 1, gr.start, 0)
      if tree.isSome():
        return tree

#*************************************************************************#
#*********************************  API  *********************************#
#*************************************************************************#


func parse[C, L, I](parser: EarleyParser[C, L],
                    toks: TokStream[Token[C, L, I]]): seq[ParseTree[C, L, I]] =
  let state = buildItems(parser, toks)
  let chart = chartOfItems(parser.grammar, toks)
  return parseTree(parser.grammar, toks)
