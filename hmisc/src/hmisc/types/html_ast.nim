##[

Statically typed ast for subset of HTML

Originally implemented to generate graphviz html-like labels.

]##

import colors, xmltree, strformat

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
        prop*: set[HtmlTextProp]
        textStr*: string
      of hekCell:
        cellColor*: Color
        cellBgColor*: Color
      else:
        discard

func wrap(xml: XmlNode, tag: string): XmlNode =
  result = newElement(tag)
  result.add xml

func toXml(html: HtmlElem): XmlNode =
  case html.kind:
    of hekTable:
      result = newElement("table")
    of hekRow:
      result = newElement("tr")
    of hekCell:
      result = newElement("td")
    of hekText:
      result = newText(html.textStr)
      for prop in html.prop:
        result =
          case prop:
            of htpBold: result.wrap("b")
            of htpUnderline: result.wrap("u")
            of htpItalic: result.wrap("i")
            of htpNone: result
    else:
      discard



  for row in html.elements:
    result.add row.toXml()

func `$`*(html: HtmlElem): string = $html.toXml()
