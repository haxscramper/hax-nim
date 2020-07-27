##[

Statically typed ast for subset of HTML

Originally implemented to generate graphviz html-like labels.

]##

import colors, xmltree, strformat, strutils, strtabs
import hprimitives

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
        cellSize*: ArrSize
      else:
        discard

func `[]`*(html: HtmlElem, idx: int): HtmlElem =
  html.elements[idx]

func `[]`*(html: var HtmlElem, idx: int): var HtmlElem =
  html.elements[idx]

func `[]=`*(html: var HtmlElem, idx: int, other: HtmlElem): void =
  html.elements[idx] = other

func add*(html: var HtmlElem, other: HtmlElem): void =
  assert html.kind notin {hekText}
  html.elements.add other

func len*(html: HtmlElem): int = html.elements.len


func toHtmlCell*(content: HtmlElem): HtmlElem =
  HtmlElem(kind: hekCell, elements: @[content])

func toHtmlCell*(strbl: string): HtmlElem =
  HtmlElem(kind: hekCell,
           elements: @[HtmlElem(
             kind: hekText,
             textStr: strbl)])


func toHtmlCell*(strbl: StrBlock): HtmlElem =
  toHtmlCell(strbl.join("\n"))

func setOrAddCell*(table: var HtmlElem, pos: ArrPos, cell: HtmlElem): void =
  assert table.kind == hekTable
  if table.elements.len <= pos.row:
    for _ in table.elements.len .. pos.row:
      table.elements.add HtmlElem(kind: hekRow)

  if table.elements[pos.row].len <= pos.col:
    for _ in table.elements[pos.row].len .. pos.col:
      table.elements[pos.row].add HtmlElem(kind: hekCell)

  table[pos.row][pos.col] = cell


func wrap(xml: XmlNode, tag: string): XmlNode =
  result = newElement(tag)
  result.add xml

func `[]=`*(xml: var XmlNode, attrname: string, attrval: string): void =
  var attrs = xml.attrs()
  if attrs != nil:
    attrs[attrname] = attrval
    xml.attrs = attrs
  else:
    xml.attrs = {attrname : attrval}.toXmlAttributes()

func toXml(html: HtmlElem): XmlNode =
  case html.kind:
    of hekTable:
      result = newElement("table")
      if html.border != 0:
        result["border"] = $html.border
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

func toHtmlDoc*(html: HtmlElem): string =
  $html.toXml().wrap("body").wrap("html")
