import tables, hashes, sugar, sequtils, strformat, options, colors
import strutils
export tables
import hmisc/helpers
import hmisc/types/[graphviz_ast, hvariant]
import hmisc/algo/[halgorithm, htree_mapping, hseq_mapping]
import lexer

import parse_primitives


type
  Patt*[TKind] = ref object
    ## Ebnf grammar pattern. `Tok` is a type for token object.
    # head*: NTermSym ## Nonterminal symbol
    action*: TreeAct
    case kind*: PattKind
      of pkNterm:
        nterm*: NTermSym ## Nonterminal to parse
      of pkTerm:
        tok*: TKind ## Single token to match literally
      of pkAlternative, pkConcat:
        patts*: seq[Patt[TKind]]
      of pkOptional, pkZeroOrMore, pkOneOrMore:
        opt*: Patt[TKind] ## Single instance that will be repeated
        ## [0..1], [0..n] or [1..n] times respectively

  Rule*[TKind] = object
    nterm*: NTermSym
    patts*: Patt[TKind]

  Grammar*[TKind] = object
    start*: NtermSym
    rules*: seq[Rule[TKind]]

#============================  Constructors  =============================#

proc toGrammar*[TKind](
  table: openarray[(string, Patt[TKind])]): Grammar[TKind] =
  result.rules = table.mapPairs(Rule[TKind](nterm: lhs, patts: rhs))
  result.start = result.rules[0].nterm

#==============================  Accessors  ==============================#

func addAction*[TKind](patt: Patt[TKind], act: TreeAct): Patt[TKind] =
  result = patt
  result.action = act

#====================  generic pattern construction  =====================#

func rule*[Tk](name: string, patt: Patt[Tk]): Rule[Tk] =
  Rule[Tk](nterm: name, patts: patt)

func zeroP*[TKind](patt: Patt[TKind]): Patt[TKind] =
  Patt[TKind](kind: pkZeroOrMore, opt: patt)

func oneP*[TKind](patt: Patt[TKind]): Patt[TKind] =
  Patt[TKind](kind: pkOneOrMore, opt: patt)

func optP*[TKind](patt: Patt[TKind]): Patt[TKind] =
  Patt[TKind](kind: pkOptional, opt: patt)

func andP*[TKind](patts: varargs[Patt[TKind]]): Patt[TKind] =
  Patt[TKind](kind: pkConcat, patts: toSeq(patts))

func orP*[TKind](patts: varargs[Patt[TKind]]): Patt[TKind] =
  Patt[TKind](kind: pkAlternative, patts: toSeq(patts))

func tok*[TKind](tok: TKind): Patt[TKind] =
  Patt[TKind](kind: pkTerm, tok: tok)

func nterm*[TKind](nterm: string): Patt[TKind] =
  Patt[TKind](kind: pkNTerm, nterm: nterm)


import strutils

#*************************************************************************#
#***************************  pretty-printing  ***************************#
#*************************************************************************#
func tokKindStr*[TKind](tkind: TKind, prefStr: string): string =
  result = $tkind
  if result.startsWith(prefStr):
    result = result[prefStr.len .. ^1]

#=======================  grammar representation  ========================#

type
  GrammarPrintConf* = object
    emptyProd*: string
    prodArrow*: string
    concatSep*: string
    alternSep*: string
    ntermWrap*: (string, string)
    termWrap*: (string, string)
    normalizeNterms*: bool

const defaultGrammarPrintConf*: GrammarPrintConf = GrammarPrintConf(
  emptyProd: "ε",
  prodArrow: "::=",
  concatSep: " & ",
  alternSep: " | ",
  ntermWrap: ("<", ">"),
  termWrap: ("'", "'")
)

func exprRepr*[TKind](
  patt: Patt[TKind],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  case patt.kind:
    of pkTerm:
      ($patt.tok).wrap(conf.termWrap)
    of pkNTerm:
      ($patt.nterm).wrap(conf.ntermWrap)
    of pkAlternative, pkConcat:
      patt.patts.mapIt(exprRepr(it, conf)).join(
        (patt.kind == pkConcat).tern(conf.concatSep, conf.alternSep)
      ).wrap("{  }")
    of pkOptional, pkZeroOrMore, pkOneOrMore:
      let suff =
        case patt.kind:
          of pkOptional: "?"
          of pkZeroOrMore: "*"
          of pkOneOrMore: "+"
          else:
            ""

      fmt("( {patt.opt.exprRepr(conf)} ){suff}")


func exprRepr*[Tk](
  grammar: Grammar[Tk],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  grammar.rules.mapIt(exprRepr(it, conf)).joinl()
