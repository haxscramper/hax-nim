import sugar, strutils, sequtils, strformat, sets, random
import hmisc/types/graphviz_ast


#===========================  implementation  ============================#

import hparse/[
  ll1_table,
  ll1_gen,
  grammars,
  bnf_grammars, # FIXME HACK not necessary but removeing it generates
                # `rule cannot be called` compilation error.
  token,
  lexer
]

import hmisc/algo/hseq_mapping # HACK fixes `undeclared field 'nthType1'`

include test_grammar

#================================  tests  ================================#

import unittest

suite "Predictive LL(1)":
  let nt = nterm[TokenKind, string]
  proc tok(k: TokenKind): auto = tok[TokenKind, string](k)
  test "Simple grammar":
    let nte = nt("element")
    let grammar = {
      # list ::= '[' <elements> ']'
      "list" : andP(
        tok(tkPunct, "["),
        nt("elements"),
        tok(tkPunct, "]")
      ),
      # elements ::= <element> (',' <element>)*
      "elements" : andP(
        nte,
        zeroP(andP(tok(tkPunct, ","), nt("element")))
      ),
      # element ::= 'ident' | <list>
      "element" : orP(tok(tkIdent), nt("list"))
    }.toGrammar()

    let tableParser = newLL1TableParser(grammar, retainGenerated = false)

    var stream = mapString("[a,b,c,d,e]").makeStream()
    let tree = tableParser.parse(stream)
    tree.topng("/tmp/tree.png")

import hparse/ll1_table


suite "Table-driven vs recursive descent":
  test "Image generation":
    const nt = nterm[TokenKind, string]
    proc tok(k: TokenKind): auto = tok[TokenKind, string](k)
    const grammarConst = {
      # list ::= '[' <elements> ']'
      "list" : andP(
        tok(tkPunct, "["),
        nt("elements"),
        tok(tkPunct, "]")
      ),
      # elements ::= <element> (',' <element>)*
      "elements" : andP(
        nt("element"),
        zeroP(andP(tok(tkPunct, ","), nt("element")))
      ),
      # element ::= 'ident' | <list>
      "element" : orP(tok(tkIdent), nt("list"))
    }

    let
      grammarVal = grammarConst
      recursiveParser = newLL1RecursiveParser[
        TokenKind, string, void](grammarConst)

      tableParser = newLL1TableParser[TokenKind, string](
        grammarVal.toGrammar(), retainGenerated = false)
      testInput = "[a,b,e,e,z,e]"

    let recursiveTree = mapString(testInput).makeStream().withResIt:
      recursiveParser.parse(it)

    let tableTree = mapString(testInput).makeStream().withResIt:
      tableParser.parse(it)


    var resultGraph: Graph
    block:
      var tree = recursiveTree.toDotGraph()
      tree.isCluster = true
      tree.name = "recursive"
      tree.topNodes.add:
        withIt makeNode(
          toNodeId(rand(100000)), grammarVal.toGrammar().exprRepr()):
          it.width = 10
          it.labelAlign = nlaLeft
          it.labelLeftPad = " ".repeat(10)

      tree.topNodes.add:
        withIt makeNode(
          toNodeId(rand(100000)),
          "Input string: " & testInput,
          shape = nsaNone
        ):
          it.width = 10

      resultGraph.addSubgraph(tree)

    block:
      var tree = tableTree.toDotGraph()
      tree.isCluster = true
      tree.name = "table"
      tree.topNodes.add:
        withIt makeNode(
          toNodeId(rand(100000)),
          tableParser.getGrammar().exprRepr(true)
        ):
          it.width = 10
          it.labelAlign = nlaLeft
          it.labelLeftPad = " ".repeat(10)


      tree.topNodes.add:
        withIt makeNode(
          toNodeId(rand(100000)),
          "Input string: " & testInput,
          shape = nsaNone
        ):
          it.width = 10
          # it.shape = nsaNone

      resultGraph.addSubgraph(tree)

    resultGraph.styleNode.fontname = "Consolas"
    resultGraph.toPng("/tmp/combined.png")
