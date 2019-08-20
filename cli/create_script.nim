import parsetoml, sequtils, parseutils, strutils, os, macros
import moustachu
import argparse
import helpers, uprompt

proc getContext*(): Context =
  newContext()

proc parseTemplates(
  confFile: string,
  context: Context,
  langExt: string):
    seq[tuple[name: string, body: string]] =

    let conf = parseFile(confFile)
    for templ in conf["templ"].getElems():
      if not templ.hasKey("lang_ext") or
         templ["lang_ext"].getStr() == langExt:
        result.add((
          name: templ["name"].getStr(),
          body: templ["body"].getStr().render(context)))


func getLastExt*(file: string): string =
  file.splitFile()[2].split(".")[^1]

proc templateFromExt*(langExt: string): tuple[name, body: string] =
  let context = getContext()
  let templates = parseTemplates(
    confFile =
    "~/.config/hax-config/config/script_templates.toml"
    .expandTilde(),
    context = context,
    langExt = langExt)

  var selected: int =
    if templates.len > 1:
      templates
      .enumerate()
      .mapIt(($it[0], it[1].name))
      .printTwoColumns()
      getInt("Select", (0, templates.len - 1))
    else:
      0

  result = templates[selected]


when isMainModule:
  parseArgs:
    opt:
      name: "t-name"
      opt: ["--name", "+takes_value"]
      help: "File name"
    opt:
      name: "t-ext"
      opt: ["--ext", "+takes_value"]
      help: "File extension"
    opt:
      name: "f-ext"
      opt: ["--f-ext", "+takes_value"]
      help: "Extract extension from file name"
    opt:
      name: "out-target"
      opt: ["--out", "+takes_value"]
      help: """If equal to 'stdout' write template result to stdout.
  Otherwise write to file. If option is missing write to stdout.
  """

  let langExt =
    if "t-ext".kp: "t-ext".k.toStr()
    else: "f-ext".k.toStr.getLastExt()

  let templ = templateFromExt(langExt)
  argParsed[0].writeFile(templ.body)
