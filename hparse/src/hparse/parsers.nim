## `Parser` object definition

type
  Parser* = ref object of RootObj

method parse*[Tok](parser: Parser, toks: var TokStream[Tok]): ParseTree[Tok] =
  raiseAssert("No implementation for base parser class")
