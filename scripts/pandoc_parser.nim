import shell

type
  ListStyleKind = enum
    lskDefaultStyle
    lskExample
    lskDecimal
    lskLowerRoman
    lskUpperRoman
    lskLowerAlpha
    lskUpperAlpha

  ListDelimiterKind = enum
    ldkDefaultDelim
    ldkPeriod
    ldkOneParen
    ldkTwoParens

  ListAttributes = object
    startNumber: int
    numberStyle: ListStyleKind
    delimStyle: ListDelimiterKind

type
  DocFormat = string
  PlainText = string
  DocAttr = object
    identifier: string
    classes: seq[string]
    kvPairs: seq[(string, string)]

  DocLink = object
    url: string
    target: string

  DocCitationMode = enum
    dckAuthorInText
    dckSuppressAuthor
    dckNormalCitation

  DocCitation = object
    id: string
    prefix: FormattedText
    suffix: FormattedText
    mode: DocCitationMode
    noteNum: int
    hash: int

  DocInlineKind = enum
    inkStr #  Text
    inkEmph #  [Inline]
    inkStrong #  [Inline]
    inkStrikeout #  [Inline]
    inkSuperscript #  [Inline]
    inkSubscript #  [Inline]
    inkSmallCaps #  [Inline]
    inkQuoted #  QuoteType [Inline]
    inkCite #  [Citation]  [Inline]
    inkCode #  Attr Text
    inkSpace #
    inkSoftBreak #
    inkLineBreak #
    inkMath #  MathType Text
    inkRawInline #  Format Text
    inkLink #  Attr [Inline] Target
    inkImage #  Attr [Inline] Target
    inkNote #  [Block]
    inkSpan #  Attr [Inline]

  DocInline = object
    case kind: DocInlineKind:
      of inkStr: #  Text
        text: string
      of inkEmph, inkStrong, inkStrikeout,
         inkSuperscript, inkSubscript, inkSmallCaps:
           #  [Inline]
        content: FormattedText
      of inkQuoted: #  QuoteType [Inline]
        singleQuote: bool ## Single/double quote
        qtContent: FormattedText
      of inkCite: #  [Citation]  [Inline]
        citation: DocCitation
        citComment: FormattedText
      of inkCode: #  Attr Text
        cAttr: DocAttr
        source: PlainText ## Source code
      of inkSpace, inkSoftBreak, inkLineBreak: #
        discard
      of inkMath: #  MathType Text
        latex: PlainText
        isInline: bool
      of inkRawInline: #  Format Text
        format: DocFormat
        inline: PlainText
      of inkLink: #  Attr [Inline] Target
        lAttr: DocAttr
        lInline: seq[DocInline]
        lTarget: DocLink
      of inkImage: #  Attr [Inline] Target
        iAttr: DocAttr
        iInline: seq[DocInline]
        iTarget: DocLink
      of inkNote: #  [Block]
        blocks: seq[DocBlock]
      of inkSpan: #  Attr [Inline]
        sAttr: DocAttr
        sInline: seq[DocInline]

  FormattedText = seq[DocInline]

  TableAlignKind = enum
    takAlignLeft
    takAlignRight
    takAlignCenter
    takAlignDefault

  DocBlockKind = enum
    dbkPlain # [Inline]
    dbkPara # [Inline]
    dbkLineBlock # [[Inline]]
    dbkCodeBlock # Attr Text
    dbkRawBlock # Format Text
    dbkBlockQuote # [Block]
    dbkOrderedList # ListAttributes [[Block]]
    dbkBulletList # [[Block]]
    dbkDefinitionList # [([Inline],[[Block]])]
    dbkHeader # Int Attr [Inline]
    dbkHorizontalRule #
    dbkTable # [Inline] [Alignment] [Double] [TableCell] [[TableCell]]
    dbkDiv # Attr [Block]
    dbkNull #

  TableCell = seq[DocBlock]

  DocBlock = object
    case kind*: DocBlockKind:
      of dbkPlain, dbkPara: # [Inline]
        plaintext: seq[DocInline]
      of dbkLineBlock: # [[Inline]]
        lineBlocs: seq[seq[DocInline]]
      of dbkCodeBlock: # Attr Text
        cAttr: DocAttr
        source: string
      of dbkRawBlock: # Format Text
        rAttr: DocAttr
        rSource: string
      of dbkBlockQuote: # [Block]
        quoteBlocsk: seq[DocBlock]
      of dbkOrderedList: # ListAttributes [[Block]]
        lAttr: ListAttributes
        ordBlocks: seq[seq[DocBlock]]
      of dbkBulletList: # [[Block]]
        bullBlocks: seq[seq[DocBlock]]
      of dbkDefinitionList: # [([Inline],[[Block]])]
        definitions: seq[tuple[
          name: seq[DocInline],
          blocks: seq[seq[DocBlock]]
        ]]
      of dbkHeader: # Int Attr [Inline]
        hLevel: int
        hAttr: DocAttr
        hName: FormattedText
      of dbkHorizontalRule, dbkNull: #
        nil
      of dbkTable:
        # [Inline] [Alignment] [Double] [TableCell] [[TableCell]]
        tCaption: FormattedText
        tColAlign: TableAlignKind
        relWidths: float
        headers: TableCell
        rows: seq[seq[TableCell]]
      of dbkDiv: # Attr [Block]
        divAttr: DocAttr
        divBlocks: seq[DocBlock]

let (res, err, code) = shellVerboseErr {dokCommand}:
  pipe:
    pandoc -t json test.md
    jq

echo res
