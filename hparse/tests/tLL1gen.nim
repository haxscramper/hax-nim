import unittest, sequtils, options
import hmisc/hpprint
import hmisc/algo/[halgorithm, htree_mapping]
import hmisc/types/graphviz_ast
import hparse/[ll1_gen, grammars], macros

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


macro grammarTest(): untyped =
  let grammar = {
    # list ::= '[' <elements> ']'
    "list" : TPatt(kind: pkConcat, patts: @[
      TPatt(kind: pkTerm, tok: tkOpBrace),
      TPatt(kind: pkNTerm, sym: "elements"),
      TPatt(kind: pkTerm, tok: tkCloseBrace)
    ]),
    # elements ::= <element> (',' <element>)*
    "elements" : TPatt(kind: pkConcat, patts: @[
      TPatt(kind: pkNterm, sym: "element"),
      TPatt(kind: pkZeroOrMore, opt: TPatt(kind: pkConcat, patts: @[
        TPatt(kind: pkTerm, tok: tkComma),
        TPatt(kind: pkNTerm, sym: "element")
      ]))
    ]),
    # element ::= 'ident' | <list>
    "element" :  TPatt(kind: pkAlternative, patts: @[
      TPatt(kind: pkTerm, tok: tkIdent),
      TPatt(kind: pkNTerm, sym: "list")
    ])
  }.toGrammar()

  let compGrammar = computeGrammar(grammar)
  # pprint compGrammar
  result = makeGrammarParser(compGrammar)
  # colorPrint result

grammarTest()

proc parseTokens(toks: seq[Token]): ParseTree[Token] =
  var root = ParseTree[Token](kind: pkNTerm)
  var stream = makeStream(toks)
  parseList(stream, root)
  return root


proc parseTokens(toks: seq[TokenKind]): ParseTree[Token] =
  var root = ParseTree[Token](kind: pkNTerm)
  var stream = makeStream(toks.mapIt(Token(kind: it)))
  parseList(stream, root)
  return root

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


suite "LL(1) parser simple":
  test "Parse simple list":
    let tree = parseTokens(@[
      tkOpBrace,
      tkIdent,
      tkComma,
      tkIdent,
      tkComma,
      tkIdent,
      tkCloseBrace
    ])

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
    let tree = parseTokens(@[
      tkOpBrace,
        tkOpBrace,
          tkIdent,
        tkCloseBrace,
      tkCloseBrace
    ])

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
    let tree = parseTokens(mapString("[a,[b],[c,d,[e,z,e]]]"))
    echo tree.treeRepr()

    # ERROR `index out of bounds, the container is empty`
    let graph = tree.toDotGraph()
    # echo graph
    graph.topng("/tmp/image.png")

  test "Map parse tree to ast":
    let root = parseTokens(@[
      tkOpBrace,
      tkIdent,
      tkComma,
      tkIdent,
      tkComma,
      tkIdent,
      tkCloseBrace
    ])

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
