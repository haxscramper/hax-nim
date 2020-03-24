import macros
import re
import strformat
import result
import hmisc/helpers
import sequtils
import strutils
import options
import deques
import typeinfo
import typetraits
import tables
import macroutils
import hmisc/termformat
import ast_pattern_matching
import colechopkg/types

template setOk[V, E](res: var Result[V, E], val: V): untyped =
  ## Set ok value for result type and immediately return
  res.ok val
  return res

template setErr[V, E](res: var Result[V, E], val: E): untyped =
  ## Set error value for result type and immediately return
  res.err val
  return res


type
  OptDescBase* = ref object of RootObj
    ## Data structure that will be used to generate command/options
    ## tree. Configuration is first parsed into this structure and
    ## only then used to generate tree. This part of the type is
    ## avalable both at runtime and compile time.
    name*: string ## name of the command, used when acessing it in code
    help*: string ## Help string
    docstr*: string ## Longer string, used to generate manpages or html
                   ## documentation.
    pfix*: seq[string] ## Possible prefixes to set given option (--opt)

    maxValues*: int ## Maximum number of times option can be set. If
                   ## this equals zero option type will be `bool`. One
                   ## for `T`. Two or more will create `seq[T]`.

  OptDesc* = ref object of OptDescBase
    exclRules*: Option[NimNode] ## Boolean expression to check whether
                               ## or not it is possible to use command
                               ## (is it mutally exclisive with some
                               ## other command or not)

    parsecheck*: Option[NimNode] ## Code that will be evaluated to
                                ## check validity of the argument.

    parseto*: Option[NimNode] ## Type that *single* use of the option
                              ## can be parsed to. This is `T` in
                              ## `maxValues`

  ArgDescBase* = ref object of RootObj
    ## Description of the command argument.
    name*: string ## name of the command, used when acessing it in code
    variadic*: bool ## Whether or not argument might accept unlimited
                   ## number of arguments. Variadic options **must**
                   ## be placed last.
    help*: string ## Help string, displayed when `--help` is invoked.
    docstr*: string ## Longer string, used to generate manpages or html
                   ## documentation.

  ArgDesc* = ref object of ArgDescBase
    parsecheck*: Option[NimNode] ## Code that will be evaluated to
                                ## check validty of the argument.
    parseto*: Option[NimNode] ## Type that *single* use of the
                          ## argument can be parsed into. If
                          ## argument is `variadic` than each
                          ## entry will be checked.

  CommandDescrBase* = ref object of RootObj
    ## Description of the utility command. This is a base type,
    ## available at both runtime and compile-time. Due to limitations
    ## imposed by use of `NimNode` in `AragDesc` some fields (list of
    ## subcommands, arguments and options) have to be copied only as
    ## `*Base` types: `CommandDescr` uses `args: seq[ArgDesc]` at
    ## compile-time, but only `argsbase: seq[ArgDescBase]` at runtime

    name*: string ## Name of the command
    help*: string ## Help message to be displayed in help page
    dosctr*: string ## Longer string, used to generate manpages or html
                     ## documentation.

    argsbase*: seq[ArgDescBase] ## Descriptions of the command argument
                               ## that is available at both runtime
                               ## and compile-time

    subsbase*: seq[CommandDescrBase] ## Description of the subcommand
                                    ## that are available at both
                                    ## runtime and compile-time.

    optsbase*: seq[OptDescBase] ## Options for the command that are
                                ## available at both runtime and
                                ## compile-time.

  CommandDescr* = ref object of CommandDescrBase
    args*: seq[ArgDesc] ## Arguments for the command. Arguments are
                       ## tried after subcommands - in case of
                       ## non-disjoint sets of values subcommand
                       ## always takes precedence.
    subs*: seq[CommandDescr] ## Subcommands.
    opts*: seq[OptDesc] ## Option related to the command.

type
  RuntimeDesc = CommandDescrBase


func toBase(sub: ArgDesc): ArgDescBase = sub.ArgDescBase
func toBase(sub: OptDesc): OptDescBase = sub.OptDescBase

func toBase(sub: CommandDescr): CommandDescrBase =
  ## Convert command description to base recursively. Map all `*Desc`
  ## fields to `*DescBase` (OptDesc -> OptDescBase)
  result = sub.CommandDescrBase
  result.argsbase = sub.args.map(toBase)
  result.optsbase = sub.opts.map(toBase)
  result.subsbase = sub.subs.map(toBase)

var descriptionsComptime {.compileTime.}: Table[string, CommandDescr]

func toBase(tbl: Table[string, CommandDescr]
           ): Table[string, CommandDescrBase] =
  ## Convert table of compile-time descriptions to runtime command
  ## descriptions.
  toSeq(pairs(tbl)).mapIt((it[0], it[1].toBase())).toTable()

type
  CodegenConfig = object
    enableLogging: bool
    verboseLogging: bool

type
  ParseErrorKind* = enum
    ## Kind of command parse error
    pekUnknownOption
    pekInvalidValue
    pekTooMuchValues
    pekMissingArgument
    pekTokenParseError

  ParseError* = object
    ## Command parse error
    message*: string
    kind*: ParseErrorKind

type
  CommandTokenKind* = enum
    ## Type of command line token
    ctkLongFlag
    ctkShortFlag

    ctkLongOption
    ctkShortOption

    ctkArgument
    ctkCommand
    ctkInvalid

  PossibleParse* = seq[CommandTokenKind]
    ## Possible parses for ambigout command line options (i.e.
    ## `-hello` might be a mistyped long flag, short flag with option
    ## or multiple short flags with last one havin argumnet (`-h`
    ## `-e=llo` is a possibilty))

  ParsedToken* = object
    ## Single, unambigously parsed token entry
    kind: CommandTokenKind
    key: string
    value: string

func classifyToken(tok: string): PossibleParse =
  debugEcho tok
  if tok =~ re"--(\w|-|_){2,}":
    @[ctkLongFlag]
  elif tok =~ re"--(\w|-|_){2,}[:=](.*)":
    @[ctkLongOption]
  else:
    @[ctkInvalid]

func isUnambiguous(optParse: PossibleParse): bool =
  optParse.len == 1

func parseToken(tok: string, kind: CommandTokenKind, comm: RuntimeDesc
               ): Result[ParsedToken, ParseError] =
  ## Parse single unambigous token
  var res = ParsedToken(kind: kind)
  case kind:
    of ctkArgument: res.value = tok
    of ctkLongFlag:
      if tok.startsWith("--"):
        res.key = tok[2..^1]
        result.setOk res
      else:
        result.setErr ParseError(
          kind: pekTokenParseError,
          message: &"Cannot parse '{tok}' as long flag, missing '--'"
        )
    of ctkShortFlag:
      if tok.startsWith("-") and tok.len == 2:
        res.key = tok[1..^1]
        result.setOk res
      else:
        result.setErr ParseError(
          kind: pekTokenParseError,
          message: &"Cannot parse '{tok}' as short flag")
    else:
      result.setErr ParseError(
        kind: pekTokenParseError,
        message: &"Unhandled token kind: {kind}"
      )

func separateTokens(
  tok: string, comm: RuntimeDesc
     ): Result[seq[ParsedToken], ParseError] =
  ## Separate token into set of parsed tokens according to
  ## specification.
  let classification = tok.classifyToken()
  if classification.isUnambiguous():
    let res = tok.parseToken(classification[0], comm)
    if res.isOk():
      result.ok @[res.get]
    else:
      result.err res.error
  else:
    debugEcho: "Ambigous token"
    if tok.startsWith("-"):
      discard

func toNimIdentName(name: string): string =
  ## Convert name of the command/option/argument to string that might
  ## be used as nim identifier name (remote dashes, whitespaces and
  ## any other characters not allowed in nim variable names)
  name.replace("-", "_")

func makeNimType(name: string, flds: seq[NimNode]): NimNode =
  ## Create new nim object type
  nnkStmtList.newTree(
    nnkTypeSection.newTree(
      nnkTypeDef.newTree(
        newIdentNode(name),
        newEmptyNode(),
        nnkObjectTy.newTree(
          newEmptyNode(),
          newEmptyNode(),
          nnkRecList.newTree(flds)))))

func joinTypeNames*(names: seq[string], comm: CommandDescr): string =
  ## Join type names to use in generated types
  (names & @[comm.name]).map(toNimIdentName).map(capitalizeAscii).join("")

func makeOptTypeName(names: seq[string], comm: CommandDescr): string =
  ## Get name of the option type for given list of parent command
  ## `names`
  "opts" & joinTypeNames(names, comm)

func makeCommandTypeName(names: seq[string], comm: CommandDescr): string =
  ## Get name of the command type for given list of parent command
  ## `names` (should include final command)
  "command" & joinTypeNames(names, comm)

func makeCommandParserName(names: seq[string], comm: CommandDescr): string =
  ## Get name of the command type for given list of parent command
  ## `names` (should include final command)
  "parse" & joinTypeNames(names, comm)

func makeCommandSetterName(names: seq[string], comm: CommandDescr): string =
  ## Get command value setter proc name
  "setvalFor" & joinTypeNames(names, comm)

func makeCommandOptionsType(
  comm: CommandDescr, parentComms: seq[string], conf: CodegenConfig
     ): NimNode =
  ## Generate type describind possible options for the command
  ## description. This is what CLI options will be parsed **into**.
  let name = makeOptTypeName(parentComms, comm)
  let flds = comm.opts.mapIt(
    block:
      nnkIdentDefs.newTree(
        newIdentNode(it.name.toNimIdentName()),
        it.parseto.get(newIdentNode("string")),
        newEmptyNode()
      )
  )
  result = makeNimType(name, flds)

func makeCommandType(
  comm: CommandDescr, parentComms: seq[string], conf: CodegenConfig
     ): NimNode =
  ## Generate type describing cli command
  let name = makeCommandTypeName(parentComms, comm)
  let optName = makeOptTypeName(parentComms, comm)
  result = makeNimType(name, @[
    nnkIdentDefs.newTree(
      ident("options"),
      ident(optName),
      newEmptyNode()
    )
  ])

func makeCommandSetter(
  comm: CommandDescr, parentComms: seq[string], conf: CodegenConfig
     ): NimNode =
  ## Create procedure for settings arguments to the command. The
  ## procedure modifies value of the command (`comm` argument)
  ## in-place. Second argument is single, unambigously parsed token
  ## entry. Generated procedure performs necessary checks and might
  ## generate error in parsing. Error returned as `err` value for
  ## `Result`.
  let name = makeCommandSetterName(parentComms, comm)
  let comm = makeCommandTypeName(parentComms, comm)

  let setLog =
    if conf.enableLogging:
      superquote do:
        echo "setting value of '", tok, "' for command ", `newStrLitNode(comm)`
    else:
      quote do:
        discard


  result = superquote do:
    proc `name.ident()`(
      comm: var `comm.ident()`,
      tok: ParsedToken
                     ): Result[bool, ParseError] =
      let comm {.inject.} = comm
      let tok {.inject.} = tok
      `setLog`

template makeVerboseLog(body: untyped): untyped =
  ## Return body if logging is enable and verblose logging is enabled.
  ## Otherwise return `discard` WARNING: expects to find variable
  ## `conf` of type `CodegenConfig` in surrounding scope.
  static:
    assert declared conf
    assert conf is CodegenConfig

  if conf.enableLogging and conf.verboseLogging:
    superQuote do:
      body
  else:
    quote do:
      discard

template makeRegularLog(body: untyped): untyped =
  ## Return body if logging is enabled. Otherwise return `discard`.
  ## WARNING: expects to find variable `conf` of type `CodegenConfig`
  ## in surrounding scope.
  static:
    assert declared conf
    assert conf is CodegenConfig

  if conf.enableLogging:
    superQuote do:
      body
  else:
    quote do:
      discard


proc echoMulti(id: string, padTo: int, m: varargs[string, `$`]): void =
  ## Print list of strings on multiple lines
  echo id.alignLeft(padTo, '_'), ": ", m[0]
  if m.len > 1:
    echo m[1..^1].mapIt(" ".repeat(padTo + 2) & it).join("\n")

proc printError(err: ParseError): void =
  ## Print parse error
  let msgWidth = 10
  echoMulti("error", msgWidth, err.kind)
  echoMulti("message", msgWidth, err.message.justifyFitTerminal((msgWidth, 10)))


func makeCommandParser(
  comm: CommandDescr, parentComms: seq[string], conf: CodegenConfig
     ): NimNode =
  ## Generate function for parsing input string to command
  let parserStrName = makeCommandParserName(parentComms, comm)
  let commResType = newIdentNode(makeCommandTypeName(parentComms, comm))
  let parserName = newIdentNode(parserStrName)
  let setterName = newIdentNode(makeCommandSetterName(parentComms, comm))
  let commName = makeCommandTypeName(parentComms, comm)
  let commLit = newStrLitNode(commName)

  let parseLogStart = makeRegularLog:
    echo "Parsing command '", `newStrLitNode(parserStrName)`, "'"

  let tokenFound = makeVerboseLog:
    echo "Found token '", tok, "'"

  let tokenSepLog = makeVerboseLog:
    echo "token separation result is ok: ", separate.isOk()
    if not separate.isOk():
      printError(separate.error)

  result = superquote do:
    proc `parserName`(tokens: seq[string]): Result[`commResType`, ParseError] =
      # TODO get description without raising side-effects
      `parseLogStart`
      let descr = descriptions[`commLit`]
      var res: `commResType`
      for tok {.inject.} in tokens:
        `tokenFound`
        let separate {.inject.} = tok.separateTokens(descr)
        `tokenSepLog`
        if separate.isOk():
          for stok in separate.get():
            let setres = `setterName`(res, stok)
            if setres.isErr():
              result.setErr setres.error
        else:
          result.setErr separate.error




proc makeCommandDeclaration(
  comm: CommandDescr, parentComm: seq[string], conf: CodegenConfig
     ): NimNode =
  ## Generate types for command `comm`
  let optType = makeCommandOptionsType(comm, parentComm, conf)
  let commType = makeCommandType(comm, parentComm, conf)
  let parser = makeCommandParser(comm, parentComm, conf)
  let setter = makeCommandSetter(comm, parentComm, conf)
  descriptionsComptime[makeCommandTypeName(parentComm, comm)] = comm
  result = quote do:
    `optType`
    `commType`
    `setter`
    `parser`


# [T:
# # char   | int     | int8   | int16 |
# # int32  | int64   | uint   | uint8 |
# # uint16 | uint32  | uint64 | bool  |
# # string | float32 | float64
#              ]

proc initCodegen[T](opt: Option[T]): NimNode {.discardable.}
proc initCodegen[T](arr: seq[T]): NimNode {.discardable.}
proc initCodegen(val: char | int | bool | string): NimNode {.discardable.}

proc initCodegenObject[T: object | tuple | ref object](obj: T): NimNode {.discardable.} =
  let isObj = (obj is object) or (obj is ref object)
  var fieldInit: seq[NimNode]
  if isObj:
    fieldInit.add newIdentNode($typeof(T))

  # NOTE using operator [] allows to treat reference object as regular one
  for name, value in (when obj is ref object: obj[] else: obj).fieldPairs:
    when isNamedTuple(typeof obj):
      fieldInit.add initCodegen(value)
    else:
      fieldInit.add nnkExprColonExpr.newTree(
        ident(name), initCodegen(value)
      )

  if isObj:
    nnkObjConstr.newTree(fieldInit)
  else:
    nnkPar.newTree(fieldInit)

proc initCodegen(obj: object | tuple | ref object): NimNode {.discardable.} =
  initCodegenObject(obj)

proc initCodegen(val: char | int | bool | string): NimNode {.discardable.} =
  when val is object:
    initCodegenObject(val)
  else:
    when val is string: newStrLitNode(val)
    elif val is int: newIntLitNode(val)
    elif val is bool: ident(val.tern("true", "false"))
    else: initCodegenObject(val)



proc initCodegen(nd: NimNode): NimNode {.discardable.} =
  discard

proc initCodegen[T](arr: seq[T]): NimNode {.discardable.} =
  nnkPrefix.newTree(
    newIdentNode("@"),
    nnkBracket.newTree(arr.mapIt(it.initCodegen())))

proc initCodegen[T](opt: Option[T]): NimNode {.discardable.} =
  if opt.isSome(): initCodegen(opt.get())
  else: quote do: none(`T`)

proc initCodegen[K, V](tbl: Table[K, V]): NimNode {.discardable.} =
  var fieldInit: seq[NimNode]
  for key, val in tbl:
    fieldInit.add nnkPar.newTree(
      initCodegen[K](key),
      initCodegen[V](val)
    )

  nnkCall.newTree(
    newIdentNode("toTable"),
    nnkBracket.newTree(fieldInit)
  )


type
  TokenKind = enum
    tkKeyword
    tkVarname
    tkType

proc colorize(node: string, kind: TokenKind): ColoredString =
  var fg = fgDefault
  var bg = bgDefault
  var style: set[Style]

  case kind:
    of tkKeyword: fg = fgBlue
    of tkVarname: style.incl styleDim
    of tkType: fg = fgRed; style.incl styleDim

  ColoredString(fg: fg, bg: bg, str: node, style: style)

proc getColored(node: NimNode): ColoredString =
  ColoredString(str: $node.toStrLit())

proc colorPrint(node: NimNode, ind: int = 0): void =
  proc pr(msg: varargs[string, `$`]) = echo "  ".repeat(ind), msg.join(" ")

  matchAst(node, matchErrors):
  of nnkProcDef(
    `name`,
    _,
    `typeParam`,
    `paramList`,
    `pragmas`,
    _,
    `body`
  ):
    pr "matches body"
  of nnkStmtList:
    for sub in node:
      sub.colorPrint(ind + 1)

  of nnkLetSection(nnkIdentDefs(`varname`, `vartype`, `varrhs`)):
    pr colorize("let", tkKeyword),
     colorize($varname.toStrLit(), tkVarname),
     ":", colorize($vartype.toStrLit(), tkType),
     "=", getColored(varrhs)

  else:
    echo "ast had mo match ", node.kind
    # for err in matchErrors:
    #   echo err

macro test() =
  let conf = CodegenConfig(enableLogging: true, verboseLogging: true)
  let commTest = CommandDescr(
    opts: @[OptDesc(
      name: "test",
      parseto: some(quote do: int)
    )],
    name: "77",
  )

  let commDecls = makeCommandDeclaration(commTest, @[], conf)
  let descrInit = initCodegen(descriptionsComptime.toBase())
  result = quote do:
    let descriptions {.inject.}: Table[string, CommandDescrBase] = `descrInit`
    `commDecls`

  descriptionsComptime.clear()
  # echo result.toStrLit()
  "ast.tmp.nim".writeFile($result.toStrLit())
  echo staticExec("pygmentize -f terminal ast.tmp.nim ")
  # result.colorPrint()


test()

let parsed = parse77(@["--test:12"])
