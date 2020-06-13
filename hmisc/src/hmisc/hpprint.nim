# TODO account for control codes in stings

import hmisc/helpers
import tables, sequtils, math, strutils, strformat, sugar

type
  ObjKind = enum
    okConstant
    okSequence
    okTable
    okComposed

  Field = object
    fldType: string
    case isKind: bool
      of true:
        subfields: seq[tuple[name: string, fld: Field]]
      of false:
        name: string
        value: ObjTree

  ObjTree = object
    case kind: ObjKind
      of okConstant:
        constType: string
        strlit: string
      of okSequence:
        itemType: string
        valItems: seq[ObjTree]
      of okTable:
        keyType: string
        valType: string
        valPairs: seq[tuple[key: string, val: ObjTree]]
      of okComposed:
        anonymous: bool
        name: string
        case sectioned: bool
          of false:
            # Simpler representation for object tree without
            # sectioning on different blocks depending on `kind`
            # fields: everything is put into single key-value
            # sequence.
            fldPairs: seq[tuple[name: string, value: ObjTree]]
          of true:
            # Most of the case objects have one `kind` field named
            # 'kind' but this should account for cases with multiple
            # case fields as well as nested ones
            kindBlocks: seq[Field]


proc toSimpleTree[Obj](entry: Obj): ObjTree =
  when not (
      (entry is seq) or
      (entry is array) or
      (entry is openarray) or
      (entry is string)
    ) and compiles(for k, v in pairs(entry): discard):
    result = ObjTree(
      kind: okTable,
      keyType: $typeof((pairs(entry).nthType1)),
      valType: $typeof((pairs(entry).nthType2))
    )


    static:
      echo "entry is ", typeof(entry)

    for key, val in pairs(entry):
      result.valPairs.add(($key, toSimpleTree(val)))

  elif not (
      (entry is string)
    ) and (compiles(for i in items(entry): discard)):
    result = ObjTree(
      kind: okSequence,
      itemType: $typeof(items(entry))
    )

    for it in items(entry):
      result.valItems.add(toSimpleTree(it))

  elif (entry is object) or (entry is tuple):
    when entry is object:
      result = ObjTree(kind: okComposed, name: $typeof(Obj), sectioned: false)
    else:
      result = ObjTree(kind: okComposed, anonymous: true, sectioned: false)

    for name, value in entry.fieldPairs():
      result.fldPairs.add((name, toSimpleTree(value)))
  else:
    when entry is string:
      let val = "\"" & entry & "\""
    else:
      let val = $entry

    result = ObjTree(
      kind: okConstant,
      constType: $typeof(Obj),
      strLit: val
    )


type
  PPrintConf = object
    maxWidth: int
    identStr: string
    wrapLargerThan: int

    kvSeparator: string
    objWrapper: (string, string)

    seqSeparator: string
    seqPrefix: string
    seqWrapper: (string, string)

  Chunk = object
    content: seq[string]
    maxWidth: int
    ident: int

proc multiline(chunk: Chunk): bool = chunk.content.len > 1

proc makeChunk(content: seq[string], ident: int): Chunk =
  Chunk(
    ident: ident,
    content: content,
    maxWidth: content.mapIt(it.len()).max()
  )

proc makeChunk(other: seq[Chunk]): Chunk =
  result = Chunk(
    ident: other[0].ident,
    content: other.mapIt(it.content).concat()
  )

  result.maxWidth = result.content.mapIt(it.len()).max()


proc pstringRecursive(
  current: ObjTree, conf: PPrintCOnf, ident: int = 0): seq[Chunk]

proc arrangeKVPairs(
  input: seq[tuple[name: string, val: Chunk]],
  conf: PPrintConf, current: ObjTree, ident: int): seq[Chunk] =
  let maxFld = input.mapIt(it.name.len()).max()
  if not input.anyOfIt(it.val.multiline()):
    # No multiline chunks present, can theoretically fit on a single line
    let singleLine = &"{current.name}{conf.objWrapper[0]}" &
      input.mapIt(&"{it.name}{conf.kvSeparator}{it.val.content[0]}").join(
        conf.seqSeparator
      ) &
      &"{conf.objWrapper[1]}"

    if singleLine.len < (conf.maxWidth - ident):
      return @[
          makeChunk(
          ident = ident,
          content = @[singleLine]
        )
      ]

  if maxFld > conf.wrapLargerThan:
    # Putting field value on next line
    discard
  else:
    # Putting field value on the same line
    discard


proc pstringRecursive(
  current: ObjTree, conf: PPrintCOnf, ident: int = 0): seq[Chunk] =
  case current.kind:
    of okConstant:
      return @[makeChunk(content = @[ current.strLit ], ident = ident)]
    of okComposed:
      echo "composed entry ", current.name
      if not current.sectioned:
        let maxFld = current.fldPairs.mapIt(it.name.len()).max()
        return current.fldPairs.mapIt(
          (it.name,
           pstringRecursive(it.value, conf, maxFld + ident).makeChunk())
        ).arrangeKVPairs(conf, current, ident)

    of okSequence:
      let values: seq[Chunk] = current.valItems.mapIt(
        pstringRecursive(it, conf, ident + conf.identStr.len())).concat()

      let sumWidth = values.mapIt(it.content.len()).sum() +
        conf.seqSeparator.len() * (values.len() - 1) +
        (conf.seqWrapper[0].len() + conf.seqWrapper[1].len())

      if values.anyOfIt(it.multiline) or (sumWidth > conf.maxWidth - ident):
        let padding = " ".repeat(conf.seqPrefix.len())
        for item in values:
          result.add makeChunk(
            content = @[conf.seqPrefix & item.content[0]] &
              item.content[1..^1].mapIt(padding & it),
            ident = ident
          )
      else:
        result = @[makeChunk(
          ident = ident,
          content = @[
              conf.seqWrapper[0] &
              values.join(conf.seqSeparator) &
              conf.seqWrapper[1]
        ])]

    else:
      discard

proc prettyString(tree: ObjTree, conf: PPrintConf, ident: int = 0): string =
  ## Convert object tree to pretty-printed string
  for chunk in pstringRecursive(tree, conf):
    for line in chunk.content:
      echo chunk.ident, line

type
  Obj1 = object
    f1: int
    f2: seq[int]
    f3: Table[int, string]

let conf = PPrintConf(
  maxWidth: 80,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "-",
  seqWrapper: ("[", "]"),
  objWrapper: ("(", ")"),
  kvSeparator: ": "
)

let tree = toSimpleTree(Obj1(
  f1: 123123,
  f2: @[12,23,3,4,222,5],
  f3: {12 : "hello", 22 : "q32a"}.toTable()
))

echo tree
echo tree.prettyString(conf)
