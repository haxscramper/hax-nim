import macros
import helpers
import sequtils
import strutils
import options
import deques

type
  Opt = object
    name: string
    conf: seq[string]

  Arg = object
    name: string

  SubTree = ref object
    name: string
    opts: seq[Opt]
    args: seq[Arg]
    subs: seq[SubTree]


proc getItem(
  topNode: NimNode,
  itemName: string): Option[NimNode] =
    for s in topNode:
      if s[0] == ident(itemName):
        return s[1]


proc getItemStr(
  topNode: NimNode,
  itemName: string):
    Option[string] =
  let s = getItem(topNode, itemName)
  result =
    if s.isSome:
      some(s.get().toStrLit().strVal[2..^2])
    else:
      none(string)


proc checkSubItems(node: NimNode, allowed: seq[string]): void =
  for child in node:
    if child.kind == nnkCall and child[0].kind == nnkIdent:
      doAssert(
        allowed.find(child[0].strVal) != -1,
        "Found unexpected child name: " &
          child[0].strVal & ". Expected values are: " & $allowed)

proc extractOpts(opts: NimNode): seq[Opt] =
  opts.checkSubItems(@["opt"])
  for node in opts:
    result.add(Opt(name: node[1].getItemStr("name").get()))


proc extractArgs(args: NimNode): seq[Arg] =
  args.checkSubItems(@["arg"])
  for node in args:
    discard


proc getSubTree(body: NimNode): SubTree =
  result = SubTree(
    name: body.getItemStr("name").get(""))
  body.checkSubItems(@["name", "opts", "args", "subs"])

  for node in body:
    case node[0].strVal:
      of "opts": result.opts.add(node[1].extractOpts())
      of "args": result.args.add(node[1].extractArgs())
      of "subs":
        for sub in node[1]:
          result.subs.add(sub[1].getSubTree())
      of "name": discard
      else:
        assert(false, "invalid identifier name")


proc toEnumField(str: string, prefix: string): string =
  prefix & str.replace('-', '_').capitalizeAscii()


proc generateEnum(
  subName: string,
  opts: seq[string],
  prefix: string = ""
     ): NimNode =
  ## Generate enum for each option in subcommand
  result = newEnum(
    name = ident(subName.capitalizeAscii()),
    fields = opts.mapIt(ident(it.toEnumField(prefix))),
    public = true,
    pure = true)


proc generateSubEnums(sub: SubTree, prefix = "Main"): NimNode =
  result = newStmtList(
    sub.mapItBFStoSeq(
      subs,
      generateEnum(
        subName =
        tern(lv == 0, prefix, "Sub".repeat(lv)) &
          it.name.capitalizeAscii(),
        opts = it.opts.mapIt(it.name),
        prefix = "opt")))

macro parseArgsTyped(body: untyped): untyped =
  defer:
    "argparse2.nim.tmp".writeFile($result.toStrLit())

  let subTree = getSubTree(body)

  result = quoteDoInterpolStmt:
    echo "test"
    `"generateSubEnums(subTree)"`

    proc parseArguments(args: string): string =
      "TMP"




when isMainModule:
  parseArgsTyped:
    opts:
      opt:
        name: "test"
        conf: ["--test", "+takes_value"]
      opt:
        name: "wtest"
        conf: ["--wtest", "+takes_value"]
    args:
      arg:
        name: "FILE"
        conf: ["+required"]
    subs:
      sub:
        name: "firstSub"
        opts:
          opt:
            name: "sub-opt"
            conf: ["--sub-opt"]
