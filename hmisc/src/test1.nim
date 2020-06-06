import macros, options

when false:
  # using `tuple` for type is not recommenede as it causes `nim object
  # constructor needs an object type` error

  type
    TermEnv*[T1] = object
      discard

    TermMatcher* = object
      discard

    RulePair*[VarSym, Obj] = object
      rule*: TermMatcher
      gen*: proc(env: TermEnv[VarSym]): Obj

  proc makeRulePair*[VarSym, Obj](
    rule: TermMatcher,
    gen: proc(env: TermEnv[VarSym]): Obj): RulePair[VarSym, Obj] =
    RulePair[VarSym, Obj](rule: rule, gen: gen)

  echo makeRulePair[int, float](
    rule = TermMatcher(),
    gen = proc(env: TermEnv[int]): float =
            discard
  )

type
  Tree = object
    val: string
    sub: seq[Tree]
when false:
  import deques
  import strutils

  iterator nodes(tree: Tree): tuple[node: Tree, path: seq[int]] =
    var que: Deque[(Tree, seq[int])]
    que.addLast((tree, @[0]))
    while que.len > 0:
      let (nowTerm, path) = que.popFirst()
      for idx, subTerm in nowTerm.sub:
        que.addLast((subTerm, path & @[idx]))

      yield (node: nowTerm, path: path)

  let test = Tree(val: "12", sub: @[
    Tree(val: "222", sub: @[Tree(val: "(())")]),
    Tree(val: "222", sub: @[Tree(val: "(()00)")]),
    Tree(val: "222", sub: @[Tree(val: "((--))")])
  ])

  for (node, path) in test.nodes():
    echo path.join(" "), " ", node.val
import intsets, options

type
  Box[V] = object
    f1: Option[V]

proc `[]`[V](self: Box[V], key: int): V = self.f1.get()
proc `[]`[V](self: var Box[V], key: int): var V =
  var curr: ptr Box[V] = addr self
  if curr.f1.isSome():
    return curr.f1.get()
  else:
    assert false

var box: Box[IntSet]

echo (12 in box[1])
