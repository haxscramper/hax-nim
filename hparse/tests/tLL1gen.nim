import unittest, sequtils
import hmisc/[halgorithm, hpprint]
import hparse/[ll1_gen, grammars], macros

type
  TokenKind = enum
    tkOpBrace
    tkCloseBrace
    tkIdent
    tkComma

  Token = object
    kind: TokenKind

  TPatt = Patt[TokenKind]


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
  let impl = makeGrammarParser(compGrammar)

  result = impl
  colorPrint(result)

grammarTest()

var testStream = makeStream(@[
  Token(kind: tkOpBrace),
  Token(kind: tkIdent),
  Token(kind: tkComma),
  Token(kind: tkIdent),
  Token(kind: tkComma),
  Token(kind: tkIdent),
  Token(kind: tkCloseBrace)
])

var root = ParseTree[Token](kind: pkNTerm)
parseList(testStream, root)

suite "LL(1) parser simple":
  test "Parse no-nested list":
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

    let res = root.mapItTreeDFS(
      it.getSubnodes, Ast,
      (it.kind == pkTerm).tern(
        (Ast(isList: false, ident: "ze")),
        (Ast(isList: true, subnodes: subt))
      )
    )

    pprint res
