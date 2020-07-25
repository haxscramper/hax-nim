import hparse/[parse_tree, token]

type
  TokenKind = enum
    tkPunct
    tkIdent

  LTok = Token[TokenKind, string, void]
  TPatt = Patt[TokenKind, string]
  PTree = ParseTree[TokenKind, string, void]

func `$`(tok: LTok): string = tok.lex
func `==`(lhs, rhs: Token): bool =
  lhs.kind == rhs.kind and (
    case lhs.kind:
      of tkIdent:
        lhs.strVal == rhs.strVal
      else:
        true
  )

func mapString(s: string): seq[Token] =
  s.mapIt(
    case it:
      of '[': Token(kind: tkOpBrace)
      of ']': Token(kind: tkCloseBrace)
      of ',': Token(kind: tkComma)
      else: Token(kind: tkIdent, strVal: $it)
  )
