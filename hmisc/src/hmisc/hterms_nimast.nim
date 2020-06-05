import macros

import hmisc/[hterms_callback, hterms_tree, halgorithm]

const constantNodes =
  {
    nnkNone, nnkEmpty, nnkNilLit, # Empty node
    nnkCharLit..nnkUInt64Lit, # Int literal
    nnkFloatLit..nnkFloat64Lit, # Float literal
    nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym # Str lit
  }

const functorNodes = { low(NimNodeKind) .. high(NimNodeKind) } - constantNodes

proc makeNimNode(kind: NimNodeKind, sons: seq[NimNode]): NimNode =
  return newTree(kind, sons)


defineTermSystemFor[NimNode, NimNodeKind](
  kindField = kind,
  sonsField = children,
  implName = nimAstImpl,
  treeMaker = makeNimNode,
  functorKinds = functorNodes,
  constantKinds = constantNodes
)
