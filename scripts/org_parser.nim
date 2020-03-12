import options
import algorithm
import terminal
import sequtils, strformat, strutils
import pegs
import colechopkg/types

type
  MarkupTextKind = enum
    mtkPlain ## Text
    mtkLaTeX ## Text
    mtkVerbatim ## Text
    mtkCode ## Text
    mtkBold ## [MarkupText]
    mtkItalic ## [MarkupText]
    mtkUnderLine ## [MarkupText]
    mtkStrikethrough ## [MarkupText]



  PlainText = string

  MarkupText = object
    case kind: MarkupTextKind

    of mtkPlain, mtkLaTeX, mtkVerbatim, mtkCode:
      content: PlainText
    of mtkBold, mtkItalic, mtkUnderLine, mtkStrikethrough:
      inner: seq[PlainText]

type
  OrgNode = ref object of RootObj
    contentsBegin: int
    contentsEnd: int
    hbegin: int
    hend: int
    postBlank: int

  OrgNodeProperty = ref object of OrgNode
    key: string
    value: string

  OrgPropertyDrawer = ref object of OrgNode
    properties: seq[OrgNodeProperty]

  OrgContentDrawer = ref object of OrgNode
    drawerName: string
    content: seq[OrgSection]

  OrgTimestamp = ref object of OrgNode
    nil

  OrgContentKind = enum
    ockList

  OrgContent = ref object of OrgNode
    # case kind*: OrgContentKind
    # of ockList:
    nil

  # OrgEntity = ref object of OrgNode


  OrgExampleBlock = ref object of OrgNode
    ## Element
    labelFmt: string
    language: string
    numberLines: bool
    options: string
    parameters: string
    preserveIndent: bool
    switches: string
    useLabels: bool
    content: string

  OrgExportBlock = ref object of OrgNode
    ## Element
    backendName: string
    content: string


  OrgSection = ref object of OrgNode
    contenst: seq[OrgContent]

  OrgHeadline = ref object of OrgNode
    sections: seq[OrgSection]

    isArchived: bool
    isCommented: bool
    isFootnote: bool
    isQuoted: bool

    level: int
    preblank: int
    priority: char

    closed: Option[OrgTimestamp]
    deadline: Option[OrgTimestamp]
    scheduled: Option[OrgTimestamp]
    rawHeadline: string
    tags: seq[string]
    todoKeyword: string
    isTodoKwd: bool

    # :title
    # Parsed headline's text, without the stars and the tags (secondary string).

let pegAst = """
sexp <- list / integer / string / ident
list <- '(' sexp+ ')'

ident <- ws (':' / \w / '-')+ ws

string <- ws '"' {_} '"'
integer <- ws \d+ ws
ws <- (\s)*
""".peg

type
  SexpNodeKind = enum
    snkRoot
    snkIdent
    snkList
    snkStrLiteral
    snkInteger
    snkCons

  SexpNode = ref object
    idx: int
    case kind*: SexpNodeKind
    of snkIdent:
      name: string
    of snkList, snkRoot:
      elements: seq[SexpNode]
    of snkStrLiteral:
      content: string
    of snkInteger:
      value: string
    of snkCons:
      car: string
      cdr: SexpNode

# proc nodeIndex(): iterator(): int =
#   return

let nodeIndex = iterator(): int =
    ## Generage sequence of increasing numbers. Increment value on
    ## each evaluation
    var res = 0
    while true:
      yield res
      inc res

const indentation = "   "

proc print(node: SexpNode, ind: int = 0): void =
  ## Pretty-pring node starting with indentation `ind`
  let prefix = "  " & indentation.repeat(ind)
  case node.kind:
    of snkIdent: echo prefix, node.name.toWhite({styleDim})
    of snkStrLiteral: echo prefix, node.name.toYellow()
    of snkRoot, snkList:
      echo prefix, "[ ", node.kind, " ", node.idx
      for item in node.elements:
        item.print(ind + 1)
      echo prefix, "]"
    of snkCons:
      echo prefix, node.car.toGreen()
      node.cdr.print(ind + 1)
    of snkInteger:
      echo prefix, $node.value



func pegToSexpKind(name: string): SexpNodeKind =
  ## COnvert peg name to `SexpNodeKind`
  case name:
    of "ident": snkIdent
    of "list": snkList
    else: raise newException(AssertionError, &"Ivalid peg item name: {name}")

let text = "(org-data (a b) 12 (:sdf 2))" # Test text

var depth = 0 ## Current depth of the printing
let ignored = @["ws"] ## Skipped tokes
let parseHelpers = @["sexp"] ## Nodes only present in parse tree but
                             ## not in syntax tree

proc debug(
  enter: static char,
  status: static char,
  name: string, args: varargs[string, `$`]): void =
  ## Pretty-pring parsing states
  if name notin ignored:
    let prefix = if enter == 'e': "-> " else: "-< "
    stdout.write(indentation.repeat(depth) & prefix)
    stdout.styledWrite(
      case status:
        of '+': fgGreen
        of '-': fgRed
        of '=': fgDefault
        else: fgDefault,
      name
    )

    echo args.join(" ")

proc debug(str: string): void =
  ## Print string with correct indentation
  echo indentation.repeat(depth), "~ ", str

proc toSexp(kind: string, value: string): SexpNode =
  ## Create s-expr based on `kind`
  case kind:
    of "ident": SexpNode(
      name: value, kind: snkIdent, idx: nodeIndex())
    of "string": SexpNode(
      content: value, kind: snkStrLiteral, idx: nodeIndex())
    of "list": SexpNode(
      kind: snkList, idx: nodeIndex())
    of "integer": SexpNode(
      kind: snkInteger, value: value, idx: nodeIndex())
    else: raise newException(AssertionError, &"Ivalid peg item name: {kind}")

var tree = SexpNode(kind: snkRoot, idx: nodeIndex()) ## Resulting tree
var stack: seq[SexpNode] = @[tree] ## Stack of lists
var lastNode: SexpNode = tree ## Reference to last node in stack/tree
proc startItem(kind: string): void =
  ## Add new item to stack of necessary
  if (kind in ignored) or (kind in parseHelpers): return

  if kind == "list":
    debug "started list"
    let new =  SexpNode(kind: snkList, idx: nodeIndex())
    # lastNode.elements.add new
    stack.add new
    lastNode = new

template last(s: untyped): untyped =
  ## Syntactic sugar for s[^1]
  s[^1]


proc finishItem(kind: string, value: string): void =
  ## Finish parsing of the item: add item to the tree and pop it from
  ## stack if necessary
  if (kind in ignored) or (kind in parseHelpers): return
  if kind == "list":
    debug "finished list"
    let bot = stack.pop
    if stack.len > 0:
      lastNode = stack.last
    else:
      debug "finished parsing"

    debug "Finished parsing, last stack item content is:"
    print stack.last, depth
    debug "Popped stack item:"
    print bot, depth
    stack.last.elements.add bot
  else:
    let stlast = stack.last
    debug &"Finalized item. kind: {kind}, value: {value}"
    stlast.elements.add toSexp(kind, value)
    debug "After adding, content of the last item is:"
    print stlast, depth

  debug "Full tree content:"
  print tree, depth


template findIt(s: untyped, predicate: untyped): int =
  ## Find item for which `predicate` evaluates as true.
  var res = -1
  for idx, it {.inject.} in s:
    if predicate:
      res = idx

  res

proc failItem(kind: string): void =
  if kind == "list":
    let bot = stack.pop
    debug "Failed to parse list item, discarding node"
    debug "Node content:"
    print bot, depth
    # TODO REVIEW
    # let idx = stack.last.elements.findIt(
    #   it.kind == snkList and it == bot
    # )

    # if idx != -1:
    #   stack.last.elements.delete idx

    debug "Full tree content:"
    print tree, depth
    # quit 0


# proc addItem(kind: string, value: string): void =
#   if last.isNil: last = tree

#   let new = toSexp(kind, value)
#   last.elements.add new
#   if new.kind in @[snkList, snkRoot]:
#     last = new

# proc popItem(): void =
#   discard last.elements.pop

let parseSexp = pegAst.eventParser:
  pkNonTerminal:
    enter:
      inc depth
      # debug 'e', '=', p.nt.name
      startItem p.nt.name
    leave:
      if length > 0:
        let match = s.substr(start, start + length - 1)
        # debug 'l', '+', match
        finishItem(p.nt.name, match)
      else:
        # debug 'l', '-', p.nt.name
        failItem(p.nt.name)

      dec depth

  # pkChar:
  #   leave:
  #     dec depth
  #     debug "<", s[start]
  #   enter:
  #     inc depth
  #     debug ">", s[start]


let plen = parseSexp(text)

echo "-".repeat(terminalWidth())
tree.print()
echo "-".repeat(terminalWidth())

if plen == text.len:
  echo "all ok"
