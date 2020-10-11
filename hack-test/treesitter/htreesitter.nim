{.push header: "<tree_sitter/api.h>"}

{.pragma: cstruct, importc, incompleteStruct.}
{.pragma: cproc, importc, cdecl, dynlib: "libclang.so".}

type
  TSTree* {.cstruct.} = object
  PtsTree = ptr TSTree

  TSParser* {.cstruct.} = object
  PtsParser = ptr TSParser

  TSInput* {.cstruct.} = object
  PtsInput = ptr TSInput

proc parse*(
  self: PtsParser, oldTree: PtsTree, input: PtsInput): PtsTree {.cproc.}
