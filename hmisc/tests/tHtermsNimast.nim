import hmisc/hterms_nimast

import unittest

suite "Hterms nim ast":
  test "DSL to declare rewriting system":
    macro rewriteTest(body: untyped): untyped =
      let rewrite = makeNodeRewriteSystem:
        rule:
          patt: Call(Ident("hello"), [[other]])
          outp:
            let exprStr = ($other.toStrLit()).newLit()
            quote do:
              echo "calling proc hello with one argument"
              echo "expr: ", `exprStr`
              echo "argument value: ", `other`
              hello(`other`)

      let term = body.toTerm()
      let nodeTree = proc(n: NimNode): string = n.treeRepr()

      let reduced = reduce(term, rewrite)
      if reduced.ok:
        echo reduced.term.treeRepr()
        result = reduced.term.fromTerm()

    proc hello(param: int) = echo param

    rewriteTest:
      hello(12 + 999)
