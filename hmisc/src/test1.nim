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

import tables, options

type
  Trie*[Key, Val] = object
    subn: Table[Key, Trie[Key, Val]]
    value: Option[Val]

proc `[]`*[Key, Val](trie: Trie[Key, Val], path: openarray[Key]): Val =
  ## Get value at path
  var curr = trie
  for key in path:
    if key in curr.subn:
      curr = curr.subn[key]
    else:
      raise newException(
        KeyError, "Trie key not found: " & $path)

  if curr.value.isNone():
    raise newException(
      KeyError, "Trie key not found: " & $path)
  else:
    curr.value.get()

proc `[]=`*[Key, Val](
  trie: var Trie[Key, Val], path: openarray[Key], val: Val) =
  ## Set value at path
  var curr: ptr Trie[Key, Val] = addr trie
  for key in path:
    if key notin curr.subn:
      curr.subn[key] = Trie[Key, Val]()

    curr = addr curr.subn[key]

  curr.value = some(val)

var tr = Trie[int, string]()

tr[[0, 2]] = "12"
assert tr[[0, 2]] == "12"
