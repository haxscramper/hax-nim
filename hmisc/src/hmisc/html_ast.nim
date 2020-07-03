##[

Statically typed ast for subset of HTML

Originally implemented to generate graphviz html-like labels.

]##

import Colors

type
  HtmlElemKind* = enum
    hekTable
    hekRow
    hekCell

    hekImage
    hekOther
    hekText

  HtmlTextProp* = enum
    htpNone
    htpBold
    htpUnderline
    htpItalic


  HtmlElem* = object
    elements*: seq[HtmlElem]
    case kind*: HtmlElemKind
      of hekTable:
        border*: int
        bgcolor*: Color
        height*: int
        width*: int
      of hekText:
        prop*: HtmlTextProp
        textStr*: string
      of hekCell:
        cellColor*: Color
        cellBgColor*: Color
      else:
        discard
