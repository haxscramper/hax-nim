import common

import std/[httpclient, strformat, uri, strutils]
import compiler/[nimeval, llstream, ast, renderer]
import hmisc/other/[oswrap, hjson]
import fusion/matching
import hpprint

let url = "https://raw.githubusercontent.com/nim-lang/packages/master/packages.json"
let file = RelFile("packages.json")

let client = newHttpClient()

if not exists(file):
  client.downloadFile(url, file.string)

for pack in parseJson(file):
  if { "name" : (asStr: @name), "web" : (asStr: @web) } ?= pack:
    let nimble = &"{web}/{name}.nimble"
    var raw = parseUri(nimble)

    if raw.hostname != "github.com":
      continue


    raw.hostname = "raw.githubusercontent.com"
    let spl = split(raw.path, "/")
    raw.path = join(spl[0 .. 2] & @["master"] & spl[3 ..^ 1], "/")
    # pprint raw

    # echo name, " ", raw

    try:
      let content = client.getContent($raw)
      echo content
      echo "-------------"

      let intr = createInterpreter(
        &"{name}.nimble",
        toSeqString(@[
          stdlib,
          stdlib / "pure",
          stdlib / "core",
          stdlib / "pure" / "collections"
      ]))

      let prefix = """
var version*: string
var requiresList*: seq[string]

proc requires(args: varargs[string]) =
  requiresList.add args

"""

      intr.evalScript(llStreamOpen(
        prefix &
        content
      ))

      echo intr.getGlobalValue(intr.selectUniqueSymbol("version"))
      echo intr.getGlobalValue(intr.selectUniqueSymbol("requiresList"))

    except HttpRequestError:
      discard

    sleep(1000)
