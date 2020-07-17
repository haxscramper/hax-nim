import unittest, sequtils, options
import hmisc/hpprint
import hmisc/algo/[halgorithm, htree_mapping]
import hmisc/types/graphviz_ast
import hparse/[ll1_gen, grammars], macros

#========================  token type definition  ========================#

type
  TokenKind = enum
    tkOpBrace
    tkCloseBrace
    tkIdent
    tkComma

  Token = object
    case kind: TokenKind
      of tkIdent:
        strVal: string
      else:
        nil

  TPatt = Patt[TokenKind]
  PTree = ParseTree[Token]

func `$`(tok: Token): string =
  case tok.kind:
    of tkIdent: tok.strVal
    of tkOpBrace: "["
    of tkCloseBrace: "]"
    of tkComma: ","


func `==`(lhs, rhs: Token): bool =
  lhs.kind == rhs.kind and (
    case lhs.kind:
      of tkIdent:
        lhs.strVal == rhs.strVal
      else:
        true
  )

#====================  generic pattern construction  =====================#

func zeroP[TKind](patt: Patt[TKind]): Patt[TKind] =
  Patt[TKind](kind: pkZeroOrMore, opt: patt)

func oneP[TKind](patt: Patt[TKind]): Patt[TKind] =
  Patt[TKind](kind: pkOneOrMore, opt: patt)

func optP[TKind](patt: Patt[TKind]): Patt[TKind] =
  Patt[TKind](kind: pkOptional, opt: patt)

func andP[TKind](patts: varargs[Patt[TKind]]): Patt[TKind] =
  Patt[TKind](kind: pkConcat, patts: toSeq(patts))

func orP[TKind](patts: varargs[Patt[TKind]]): Patt[TKind] =
  Patt[TKind](kind: pkAlternative, patts: toSeq(patts))

func tok[TKind](tok: TKind): Patt[TKind] =
  Patt[TKind](kind: pkTerm, tok: tok)

func nterm[TKind](sym: string): Patt[TKind] =
  Patt[TKind](kind: pkNTerm, sym: sym)

#======================  grammar parser generation  ======================#

template makeGrammarParser(body: untyped): untyped =
  # Trillion IQ hack
  macro buildGrammar(): untyped =
    let grammar = toGrammar(body)
    let compGrammar = computeGrammar(grammar)
    # pprint compGrammar
    result = makeGrammarParser(compGrammar)
    colorPrint result

  buildGrammar()

#======================  dummy value construction  =======================#

proc pe(kind: PattKind, args: varargs[PTree]): PTree =
  newTree(kind, toSeq(args))

proc pt(tok: TokenKind): PTree =
  newTree(Token(kind: tok))

proc pn(name: NTermSym, args: varargs[PTree]): PTree =
  newTree(name, toSeq(args))

func mapString(s: string): seq[Token] =
  s.mapIt(
    case it:
      of '[': Token(kind: tkOpBrace)
      of ']': Token(kind: tkCloseBrace)
      of ',': Token(kind: tkComma)
      else: Token(kind: tkIdent, strVal: $it)
  )

proc `$`(a: PTree): string = pstring(a)

proc tokensBFS(tree: PTree): seq[Token] =
  tree.mapItBFStoSeq(
    toSeq(it.subnodes()),
    if it.kind == pkTerm: some(it.tok) else: none(Token),
    it.kind != pkTerm
  )

proc parseToplevel[Tok](
  toks: seq[Tok],
  parseCb: proc(
    toks: var TokStream[Tok],
    tree: var ParseTree[Tok])): ParseTree[Tok] =
  var root = ParseTree[Token](kind: pkNTerm)
  var stream = makeStream(toks)
  parseCb(stream, root)
  return root

proc toTokSeq(inseq: seq[TokenKind]): seq[Token] =
  inseq.mapIt(Token(kind: it))

# proc parseTokens[Tok, TokKind](
#   toks: seq[TokKind],
#   parseCb: proc(
#     toks: var TokStream[Tok],
#     tree: var ParseTree[Tok])): ParseTree[Token] =

#   var root = ParseTree[Token](kind: pkNTerm)
#   var stream = makeStream(toks.mapIt(Token(kind: it)))
#   parseList(stream, root)
#   return root



suite "LL(1) parser simple":
  const nt = nterm[TokenKind]
  makeGrammarParser({
      # list ::= '[' <elements> ']'
      "list" : andP(
        tok(tkOpBrace),
        nt("elements"),
        tok(tkCloseBrace)
      ),
      # elements ::= <element> (',' <element>)*
      "elements" : andP(
        nt("element"),
        zeroP(andP(
          tok(tkComma),
          nt("element")
        ))
      ),
      # element ::= 'ident' | <list>
      "element" : orP(
        tok(tkIdent),
        nt("list")
      )
    })

  test "Parse simple list":
    let tree = parseTopLevel(@[
      tkOpBrace,
      tkIdent,
      tkComma,
      tkIdent,
      tkComma,
      tkIdent,
      tkCloseBrace
    ].toTokSeq(), parseList)

    assert tree.tokensBFS() == pe(
      pkConcat,
      pt(tkOpBrace),
      pkConcat.pe(
        pn("element", pt(tkIdent)),
        pkZeroOrMore.pe(
          pkConcat.pe(pt(tkComma), pn("element", pt(tkIdent))),
          pkConcat.pe(pt(tkComma), pn("element", pt(tkIdent)))
        )
      ),
      pt(tkCloseBrace)
    ).tokensBFS()

  test "Parse nested list":
    let tree = parseTopLevel(@[
      tkOpBrace,
        tkOpBrace,
          tkIdent,
        tkCloseBrace,
      tkCloseBrace
    ].toTokSeq(), parseList)

    assert tree.tokensBFS() == pkConcat.pe(
      pt(tkOpBrace),
      pkConcat.pe(
        pt(tkOpBrace),
        pkConcat.pe(pt(tkIdent)),
        pt(tkCloseBrace)
      ),
      pt(tkCloseBrace)
    ).tokensBFS()

  test "Deeply nested list with idents":
    # TODO unit test error for unfinished input
    # TODO test erorr for incorrect token expected
    let tree = parseTopLevel(
      mapString("[a,[b]]"),
      parseList
    )
    echo tree.treeRepr("tk")

    # ERROR `index out of bounds, the container is empty`
    let graph = tree.toDotGraph()
    # echo graph
    graph.topng("/tmp/image.png")

  test "Map parse tree to ast":
    let root = parseTopLevel(@[
      tkOpBrace,
      tkIdent,
      tkComma,
      tkIdent,
      tkComma,
      tkIdent,
      tkCloseBrace
    ].toTokSeq(), parseList)

    type
      Ast = object
        case isList: bool
          of true:
            subnodes: seq[Ast]
          else:
            ident: string

    proc getSubnodes[Tok](node: ParseTree[Tok]): seq[ParseTree[Tok]] =
      if node.kind == pkTerm:
        return @[]
      else:
        return toSeq(node.subnodes())

    let res = root.mapItDFS(
      it.getSubnodes,
      Ast,
      (it.kind == pkTerm).tern(
        (Ast(isList: false, ident: "ze")),
        (Ast(isList: true, subnodes: subt))
      )
    )

suite "LL(1) parser tree actions":
  const nt = nterm[TokenKind]
  makeGrammarParser({
      # list ::= '[' <elements> ']'
      "list" : andP(
        tok(tkOpBrace),
        nt("elements"),
        tok(tkCloseBrace)
      ),
      # elements ::= <element> (',' <element>)*
      "elements" : andP(
        nt("element"),
        zeroP(andP(
          tok(tkComma),
          nt("element")
        ))
      ),
      # element ::= 'ident' | <list>
      "element" : orP(
        tok(tkIdent),
        nt("list")
      )
    })

  test "Drop rule":
    discard
