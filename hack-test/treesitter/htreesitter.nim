{.push header: "<tree_sitter/api.h>"}

{.pragma: cstruct, importc, incompleteStruct.}
{.pragma: cproc, importc, cdecl, dynlib: "libclang.so".}

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

proc ts_node_type(a1: TSNode): cstring {.cproc.}

proc nodeType*(node: TSNode): string = $node.tsNodeType()
