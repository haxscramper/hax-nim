import unittest, sequtils, options, random, strutils
import typetraits
import hmisc/hpprint
import hmisc/algo/[halgorithm, htree_mapping]
import hmisc/types/graphviz_ast
import hparse/[ll1_gen, grammars, parse_primitives, lexer, token], macros
include test_grammar

import hparse/bnf_grammars # Unless I import this one explicitly I get
                           # error with `hashes`.
import hashes, sets, tables

template withResIt*(val, body: untyped): untyped =
  block:
    var it {.inject.} = val
    body

#======================  grammar parser generation  ======================#
template newLL1RecursiveParser[C, L, I](
  body: typed,
  standalone: bool = false): untyped =
  # Trillion IQ hack
  macro buildParser(): untyped =
    let grammar = toGrammar(body)
    let compGrammar = computeGrammar(grammar)
    let cbName = grammar.start.makeParserName()
    result = newStmtList(
      makeGrammarParser(compGrammar),
      nnkLetSection.newTree(
        nnkIdentDefs.newTree(
          newIdentNode("cb"),
          newEmptyNode(),
          nnkBracketExpr.newTree(
            newIdentNode(cbName),
            newIdentNode($(typeof I))
          )
        )
      ),
      if standalone:
        nnkLetSection.newTree(
          nnkIdentDefs.newTree(
            newIdentNode("parser"),
            newEmptyNode(),
            nnkCall.newTree(
              newIdentNode("newLL1RecursiveDescent"),
              newIdentNode("cb"))))
      else:
        newCall("newLL1RecursiveDescent", ident "cb")
    )

    colorPrint(result, doPrint = false)

  buildParser()

#======================  dummy value construction  =======================#

# proc pe(args: varargs[PTree]): PTree = newTree(toSeq(args))
# proc pt(tok: TokenKind): PTree = newTree(Token(kind: tok))
# proc pn(name: NTermSym, args: varargs[PTree]): PTree =
#   newTree(name, toSeq(args))

# proc `$`(a: PTree): string = pstring(a)

# proc tokensBFS(tree: PTree): seq[Token] =
#   tree.mapItBFStoSeq(
#     it.getSubnodes(),
#     if it.kind == ptkTerm: some(it.tok) else: none(Token)
#   )

# proc parseToplevel[Tok](
#   toks: seq[Tok],
#   parseCb: proc(
#     toks: var TokStream[Tok]): ParseTree[Tok]): ParseTree[Tok] =
#   var stream = makeStream(toks)
#   return parseCb(stream)

# proc toTokSeq(inseq: seq[TokenKind]): seq[Token] =
#   inseq.mapIt(Token(kind: it))

suite "LL(1) parser simple":
  const nt = nterm[TokenKind, string]
  proc tok(k: TokenKind): auto = tok[TokenKind, string](k)
  let parser = newLL1RecursiveParser[Token, string, void]({
      # list ::= '[' <elements> ']'
      "list" : andP(
        tok(tkPunct, "["),
        nt("elements"),
        tok(tkPunct, "]")
      ),
      # elements ::= <element> (',' <element>)*
      "elements" : andP(
        nt("element"),
        zeroP(andP(
          tok(tkPunct, "["),
          nt("element")
        ))
      ),
      # element ::= 'ident' | <list>
      "element" : orP(
        tok(tkIdent),
        nt("list")
      )
    })

suite "LL(1) parser tree actions":
  const nt = nterm[TokenKind, string]
  proc tok(k: TokenKind): auto = tok[TokenKind, string](k)
  newLL1RecursiveParser[Token, string, void]({
      # list ::= '[' <elements> ']'
      "list" : andP(
        tok(tkPunct, "[").addAction(taDrop),
        nt("elements").addAction(taSpliceDiscard),
        tok(tkPunct, "]").addAction(taDrop)
      ),
      # elements ::= <element> (',' <element>)*
      "elements" : andP(
        nt("element").addAction(taSpliceDiscard),
        zeroP(andP(
          tok(tkPunct, ",").addAction(taDrop),
          nt("element").addAction(taSpliceDiscard)
        ).addAction(taSpliceDiscard)).addAction(taSpliceDiscard)
      ),
      # element ::= 'ident' | <list>
      "element" : orP(
        tok(tkIdent).addAction(taPromote),
        nt("list").addAction(taPromote)
      )
    }, standalone = true)

  test "Drop rule":
    let text = "[[c,z,d,[e,d]],[e,d,f]]"
    # let text = "[a,b]"
    var toks = mapString(text).makeStream(
      # nextTokCb = (
      #   proc(tok: LTok, pos: int) = echo "Reading ", tok.exprRepr(), " @ ", pos
      # )
    )
    let tree = parser.parse(toks)

    echo "--- FINAL ---"
    echo tree.treeRepr("tk")
    tree.topng("/tmp/image.png", "tk", bottomTokens = true)
