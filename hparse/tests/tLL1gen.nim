import unittest, sequtils, options
import hmisc/hpprint
import hmisc/algo/[halgorithm, htree_mapping]
import hmisc/types/graphviz_ast
import hparse/[ll1_gen, grammars], macros
include test_grammar

#========================  token type definition  ========================#


#======================  grammar parser generation  ======================#

template makeGrammarParser(body: untyped): untyped =
  # Trillion IQ hack
  macro buildGrammar(): untyped =
    let grammar = toGrammar(body)
    let compGrammar = computeGrammar(grammar)
    # pprint compGrammar
    result = makeGrammarParser(compGrammar)
    # colorPrint result

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
    it.subnodes,
    if it.kind == pkTerm: some(it.tok) else: none(Token),
    it.kind != pkTerm
  )

proc parseToplevel[Tok](
  toks: seq[Tok],
  parseCb: proc(
    toks: var TokStream[Tok]): ParseTree[Tok]): ParseTree[Tok] =
  var stream = makeStream(toks)
  return parseCb(stream)

proc toTokSeq(inseq: seq[TokenKind]): seq[Token] =
  inseq.mapIt(Token(kind: it))

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
      mapString("[[c,z,d,[e,d]],[e,d,f]]"),
      parseList
    )
    echo tree.treeRepr("tk")
    # echo tree.lispRepr("tk")

    # ERROR `index out of bounds, the container is empty`
    let graph = tree.toDotGraph("tk", false)
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
        return node.subnodes

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
        tok(tkOpBrace).addAction(taDrop),
        nt("elements").addAction(taSpliceDiscard),
        tok(tkCloseBrace).addAction(taDrop)
      ),
      # elements ::= <element> (',' <element>)*
      "elements" : andP(
        nt("element").addAction(taSpliceDiscard),
        zeroP(andP(
          tok(tkComma).addAction(taDrop),
          nt("element").addAction(taSpliceDiscard)
        ).addAction(taSpliceDiscard)).addAction(taSpliceDiscard)
      ),
      # element ::= 'ident' | <list>
      "element" : orP(
        tok(tkIdent).addAction(taPromote),
        nt("list").addAction(taPromote)
      )
    })

  test "Drop rule":
    let tree = parseTopLevel(
      mapString("[[c,z,d,[e,d]],[e,d,f]]"),
      parseList
    )

    echo "--- FINAL ---"
    echo tree.treeRepr("tk")

    tree.topng("/tmp/image.png", "tk", bottomTokens = true)

import hparse/ll1_table

suite "Predictive LL(1)":
  const nt = nterm[TokenKind]
  test "Simple grammar":
    let parser = newLL1TableParser({
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
    }.toGrammar())

    var stream = mapString("[a]").makeStream()
    let tree = parser.parse(stream)
