import hargparse
import re
import strutils
import shell
import hmisc/[helpers, defensive]
import colechopkg/lib
import strformat
import os
import parsetoml
import sequtils
import json
import base64
import md5


const
  testing = true
  defaultTemp = "/tmp/ipynb_exporter"
  defaultInput =
    when testing: defaultTemp
    else: "input"




type
  NBCellKind = enum
    nbcMarkdown
    nbcCode

  NBCellDataKind = enum
    nboTextPlain
    nboImagePng
    nboTextHtml

  NBCellOutput = object
    data*: seq[tuple[text: string, kind: NBCellDataKind]]

  NBCell = object
    case kind*: NBCellKind
    of nbcMarkdown:
      text*: string
    of nbcCode:
      source*: string
      outputs*: seq[NBCellOutput]

  NBDocument = object
    cells*: seq[NBCell]


type
  LatexNodeKind = enum
    lnkPlaintext
    lnkMacroUse
    lnkEnviron
    lnkComment

  LatexNode = object
    case kind*: LatexNodeKind
    of lnkPlaintext, lnkComment:
      text: string ## Plaintext string
    of lnkMacroUse, lnkEnviron:
      name: string ## Macro name
      optargs: seq[LatexNode] ## Optional arguments
      gargs: seq[LatexNode] ## Braced arguments
      body: seq[LatexNode] ## Body of the environment. Not used for macro

  LatexDocument = object
    preamble*: seq[LatexNode]
    content*: seq[LatexNode]

func wrapBrace(str: string, brace: char, padding: string = ""): string =
  let closeb: char =
    case brace:
      of '[': ']'
      of '{': '}'
      of '(': ')'
      of '<': '>'
      else: brace

  return brace & padding & str & padding & closeb

func toString(node: LatexNode): string =
  case node.kind:
    of lnkPlainText: node.text
    of lnkComment: "% " & node.text
    of lnkEnviron, lnkMacroUse:
      let optargs = (node.optargs.len > 0).tern(
        node.optargs.mapIt(it.toString()).join(",").wrapBrace('['),
        "")

      let gargs = (node.gargs.len > 0).tern(
        node.gargs.mapIt(it.toString().wrapBrace('{')).join(""),
        "")

      if node.kind == lnkEnviron:
        let body = node.body.mapIt(it.tostring()).join("\n")
        """
  \begin{{{node.name}}}{optargs}{gargs}
  {body}
  \end{{{node.name}}}
  """
      else:
        """\{node.name}{optargs}{gargs}""""

func toString(doc: LatexDocument): string =
  result &= doc.preamble.mapIt(it.toString()).join("\n")
  result &= "\\begin{document}\n"
  result &= doc.preamble.mapIt(it.toString()).join("\n")
  result &= "\\end{document}\n"

func makeLtxPlaintext(text: string): LatexNode =
  LatexNode(
    kind: lnkPlaintext,
    text: text
  )

func makeLtxMacro(name: string, gargs, optargs: seq[string] = @[]): LatexNode =
  LatexNode(
    kind: lnkMacroUse,
    name: name,
    gargs: gargs.map(makeLtxPlaintext),
    optargs: optargs.map(makeLtxPlaintext)
  )

func makeLtxEnviron(
  name: string,
  body: seq[LatexNode],
  gargs, optargs: seq[string] = @[]): LatexNode =

  LatexNode(
    kind: lnkEnviron,
    name: name,
    body: body,
    gargs: gargs.map(makeLtxPlaintext),
    optargs: optargs.map(makeLtxPlaintext)
  )

func toLatex(cell: NBCell): LatexNode =
  case cell.kind:
    of nbcMarkdown: makeLtxPlaintext(cell.text) # TODO export using pandoc
    of nbcCode:
      makeLtxEnviron(
        name = "minted",
        gargs = @["python"],
        body = @[makeLtxPlaintext(cell.source)]
      )

func toLatexDocument(notebook: NBDocument, title, author: string): LatexDocument =
  result.preamble.add makeLtxMacro("documentclass", gargs = @["article"])

  let packages: seq[(seq[string], string)] = @[
    (@["T1", "T2A"], "fontenc"),
    (@["utf8"], "inputenc"),
    (@["english", "russian"], "babel"),
    (@[], "amsmath"),
    (@[], "amssymb"),
    (@[], "minted"),
    (@[], "graphicx"),
    (@[], "grffile"),
    (@[], "longtable"),
    (@[], "fancyhdr"),
    (@["margin=2cm"], "geometry")
  ]

  result.preamble.add packages.mapIt(
    makeLtxMacro(
      name = "usepackage",
      gargs = @[it[1]],
      optargs = it[0]
    ))

  let styleconf: seq[(string, string)] = @[
    ("pagestyle", "fancy"),
    ("fancyhf", ""),
    ("lhead", &"{title} {author}"),
    ("rhead", "\\today"),
    ("rfoot", "\\thepage"),
    ("author", &"{title} {author}"),
    ("title", &"{author}")
  ]

  result.preamble.add styleconf.mapIt(
    makeLtxMacro(it[0], gargs = @[it[1]])
  )

  result.content.add makeLtxMacro("maketitle")

  result.content.add notebook.cells.map(toLatex)

parseArgs:
  opt:
    name: "temp-dir"
    opt: ["--temp-dir", "+takes_values"]
    help: """Specify temporary directory.
             If not specified {defaultTemp} will be used"""
  opt:
    name: "input-dir"
    opt: ["--in-dir", "--input", "+takes_values"]
    help: """Specify input directory. By default {defaultInput}
             is used"""

let tempDir = "temp-dir".kp().tern(
  "temp-dir".k.toStr(), defaultTemp
)

let inputDir = "input-dir".kp().tern(
  "input-dir".k.toStr(), defaultInput
).absolutePath()

ceUserLog0(&"Input directory '{inputDir}'")
ceUserLog0(&"Temp directory '{tempDir}'")

if not dirExists(inputDir):
  ceUserError0("Input directory does not exist")
  die()
else:
  ceUserInfo0("Input directory exists")
  setCurrentDir(inputDir)

let notebook = findFirstFile("*.ipynb", "notebook")
let configuration =  findFirstFile("*.toml", "configuration file")

ceUserInfo0(&"Input notebook: {notebook}")
ceUserInfo0(&"Input configuration: {configuration}")


proc getConfOrDie(conf: TomlValueRef, name: string): string =
  if not conf.hasKey(name):
    ceUserError0(&"Configuration file is missing {name}")
    die()
  else:
    conf[name].getStr()

let config = parsetoml.parseFile(configuration)
let author = config.getConfOrDie("name")
let group = config.getConfOrDie("group")
let task = config.getConfOrDie("task")


proc anything(input: string, argument: var string, start: int): int =
  let diff = input.len - start
  argument = input[start..^1]
  return diff

proc exportCellOutput(outp, cell: JsonNode): string =
  let outtype = outp["output_type"].getStr()
  let execcount = cell["execution_count"].getInt()
  # ceUserLog0(&"Processing output #{execcount}", 2)
  if outtype == "stream":
    result = """
% ---
\begin{verbatim}
$1
\end{verbatim}
% ---""" % [outp["text"].getStr()]
  elif outtype == "execute_result" or outtype == "display_data":
    let data = outp["data"]
    if data.hasKey("image/png"):
      let base64 = data["image/png"].getStr()
      let hash = toMD5(base64)
      let outf = "sfdf" & $hash & ".png"
      let file = outf.open(fmWrite)
      file.write(decode(base64))
      file.close()

      let (res, _, code) = shellVerboseErr:
        identify ($outf)

      var
        height: int
        width: int

      if res =~ re""".+ .+ (\d+)x(\d+).*""":
        width = matches[0].parseInt()
        height = matches[1].parseInt()
      else:
        echo res
        echo "not ok"
        die()


      result = """
\begin{center}
\includegraphics[width=$1in]{$2}
\end{center}
""" % [$(width / 100), getCurrentDir().joinPath(outf)]
#     elif data.hasKey("text/html"):
#       let body = data["text/html"].getStr()
#       "tmp.html".writeFile(body)
#       let (latex, err, code) = shellVerboseErr {dokCommand}:
#         pandoc -f html -t latex "tmp.html" -o "table.tex"

#       result = """
# % ---
# \begin{center}
# $1
# \end{center}
# % ---
# """ % ["table.tex".readFile()]
    else:
      result = """
% ---
\begin{verbatim}
$1
\end{verbatim}
% ---""" % [outp["data"]["text/plain"].getStr()] #TODO export html tables?

proc exportCodeCell(cell: JsonNode): string =
  let body = cell["source"].getElems().mapIt(it.getStr()).join("")
  result.add """
% ---
\begin{minted}{python}
$1
\end{minted}
% ---""" % [body]

  for res in cell["outputs"].getElems():
    result.add "\n"
    result.add res.exportCellOutput(cell)

proc shellConfig(): set[DebugOutputKind] = {}


proc exportMarkdownCell(cell: JsonNode): string =
  let body = cell["source"].getElems().mapIt(it.getStr()).join("")
  "file.md".writeFile(body)
  let (res, err, code) = shellVerboseErr shellConfig():
    pandoc -f markdown -t latex -o "file.tex" "file.md"

  if code != 0:
    ceUserError0("Error while converting to markdown")
    echo err
    die()

  result = "file.tex".readFile().string()


let outName = "result_tex.tex"
ceUserLog0(&"Writing result to file {outName}")
let outFile = outName.open(fmWrite)

outFile.write """
% Created 2020-01-29 Wed 18:16
% Intended LaTeX compiler: pdflatex
\documentclass{article}

\usepackage{hyperref}

\hypersetup{colorlinks=true,linkcolor=blue}
\usepackage[T1, T2A]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage[english,russian]{babel}

% \usepackage{cmap}

\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{minted}
\usepackage{graphicx}
\usepackage{grffile}
\usepackage{longtable}

\usepackage{fancyhdr}

\usepackage[margin=2cm]{geometry}
\pagestyle{fancy}
\fancyhf{}
\lhead{$1 $2}
\rhead{\today}
\rfoot{\thepage}

\author{$1 $2}
\title{$2}

\begin{document}

\maketitle
""" % [author, group, task]


let nbJson = json.parseFile(notebook)

for cell in nbJson["cells"].getElems():
  if cell["cell_type"].getStr() == "code":
    outFile.write exportCodeCell(cell)
  elif cell["cell_type"].getStr() == "markdown":
    outfile.write exportMarkdownCell(cell)

outFile.write """
% ---
\end{document}
"""

outFile.close()

# let (res, err, code) = shellVerboseErr {dokCommand}:
#   latexmk "-pdf -latexoption=-shell-escape --interaction=nonstopmode" ($outName)

# if code != 0:
#   ceUserError0("Error while compiling result document")
#   echo err
# else:
#   ceUserInfo0("Compilation ok")
#   copyFile(outName, &"{author}_{group}_{task}.pdf")
