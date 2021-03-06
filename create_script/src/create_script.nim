import parsetoml, sequtils, parseutils, strutils, os, macros
import moustachu
import hargparse
import hmisc/helpers
import hmisc/uprompt
import posix
import colechopkg/lib
import strformat



proc addUExec*(file: string): void =
  ## Add user execution permission to file
  let pathnames: cstringArray = allocCStringArray(@[file])
  let pathname: cstring = pathnames[0]

  var statRes: Stat
  discard stat(pathname, statRes)
  let newMode: Mode = statRes.stMode or S_IXUSR
  discard chmod(file, newMode)

  deallocCstringArray(pathnames)



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

proc getExtTemplates(langExt: string): seq[string] =
  ## Find all templates in ~/.config/create-script
  @[]

proc templateFromExt*(langExt: string): tuple[name, body: string] =
  let context = getContext()

  # let conf = getCallPath().splitPath()[0] & "/config/script_templates.toml"
  let conf = ""

  let templates = parseTemplates(
    confFile = conf,
    context = context,
    langExt = langExt)

  var selected: int =
    if templates.len > 1: # Select single build option by default
      templates
      .enumerate()
      .mapIt(($it[0], it[1].name))
      .printTwoColumns()
      getInt("Select", (0, templates.len - 1))
    else:
      0

  if templates.len == 0:
    ceUserError0(fmt"""
No templates found for extension {langExt}. Make sure
that *at least one* template with extension '{langExt}'
exists in configuraion file and can be parsed without errors
(see previous messages for error clarifications).
""")
    quit (1)

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

  let fileName = argParsed[0]

  let langExt =
    if "t-ext".kp: "t-ext".k.toStr()
    elif "f-ext".kp: "f-ext".k.toStr.getLastExt()
    else: fileName.getLastExt()

  let templ = templateFromExt(langExt)
  fileName.writeFile(templ.body)
  ceUserInfo2("Created script file")
  fileName.addUExec()
  ceUserInfo2("Added execution permission")
