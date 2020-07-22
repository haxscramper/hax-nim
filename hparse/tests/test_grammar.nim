import hparse/parse_tree

type
  TokenKind = enum
    tkOpBrace
    tkCloseBrace
    tkIdent
    tkComma

  Token* = object
    case kind: TokenKind
      of tkIdent:
        strVal: string
      else:
        nil

  TPatt = Patt[TokenKind]
  PTree = ParseTree[Token]

func `$`(tok: Token): string =
  case tok.kind:
    of tkIdent: tok.strVal
    of tkOpBrace: "["
    of tkCloseBrace: "]"
    of tkComma: ","


func `==`(lhs, rhs: Token): bool =
  lhs.kind == rhs.kind and (
    case lhs.kind:
      of tkIdent:
        lhs.strVal == rhs.strVal
      else:
        true
  )
