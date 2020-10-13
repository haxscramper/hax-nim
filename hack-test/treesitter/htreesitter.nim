{.push header: "<tree_sitter/api.h>"}

{.pragma: cstruct, importc, incompleteStruct.}
{.pragma: cproc, importc, cdecl.}

type
  TSTree* {.cstruct.} = object
  PtsTree* = ptr TSTree

  TSLanguage* {.cstruct.} = object
  PtsLanguage* = ptr TSLanguage

  TSParser* {.cstruct.} = object
  PtsParser* = ptr TSParser

  TSInput* {.cstruct.} = object
  PtsInput* = ptr TSInput

  TSNode* {.cstruct.} = object
    context*: array[4, uint32]
    id*: pointer
    tree*: PtsTree

proc ts_parser_parse(
  self: PtsParser, oldTree: PtsTree, input: PtsInput): PtsTree {.cproc.}

proc ts_node_type*(a1: TSNode): cstring {.cproc.}

proc ts_parser_new*(): PtsParser {.cproc.}
proc ts_parser_set_language*(self: PtsParser, lang: PtsLanguage) {.cproc.}
proc ts_parser_parse_string*(
  self: PtsParser, tree: PtsTree,
  input: cstring, length: uint32): PtsTree {.cproc.}

proc ts_node_child*(node: TSNode; idx: uint32): TSNode {.cproc.}
proc ts_node_child_count*(node: TSNode): uint32 {.cproc.}

proc ts_node_named_child*(node: TSNode; idx: uint32): TSNode {.cproc.}
proc ts_node_named_child_count*(a1: TSNode): uint32 {.cproc.}

proc ts_tree_root_node*(self: PTSTree): TSNode {.cproc.}

# proc nodeType*(node: TSNode): string = $node.tsNodeType()
