import strutils, sequtils, strformat
import ../types/hnim_ast
import ../algo/[halgorithm, hseq_mapping]

func pptConst*(val: string): ValObjTree =
  ValObjTree(kind: okConstant, strlit: val)

func pptSeq*(vals: varargs[ValObjTree]): ValObjTree =
  ValObjTree(kind: okSequence, valItems: toSeq(vals))

func pptObj*(name: string,
             flds: varargs[tuple[
               name: string,
               value: ValObjTree]]): ValObjTree =

  ValObjTree(kind: okComposed,
             sectioned: false,
             namedObject: true,
             namedFields: true,
             name: name,
             fldPairs: toSeq(flds))

func pptObj*(name: string,
             flds: varargs[ValObjTree]): ValObjTree =
  ValObjTree(kind: okComposed,
             sectioned: false,
             namedObject: true,
             namedFields: false,
             name: name,
             fldPairs: flds.mapIt(("", it)))

type
  TreeReprParams* = object
    maxDepth: int
    outWidth: int

#==============================  Lisp repr  ==============================#

func lispReprImpl*(tree: ValObjTree,
                   params: TreeReprParams,
                   level: int): string =

  if level >= params.maxDepth:
    return "..."

  case tree.kind:
    of okConstant:
      return tree.strLit
    of okSequence:
      return tree.valItems.
        mapIt(lispReprImpl(it, params, level + 1)).
        joinw().wrap(("'(", ")"))
    of okTable:
      return tree.valPairs.
        mapPairs(fmt("(:{lhs} {rhs.lispReprImpl(params, level)})")).
        joinw().wrap("()")
    of okComposed:
      case tree.sectioned:
        of true:
          raiseAssert("#[ IMPLEMENT ]#")
        of false:
          return tree.fldPairs.
            mapPairs(
              tree.namedFields.tern(&":{lhs} ", "") &
              rhs.lispReprImpl(params, level + 1)).
            joinw().
            wrap((tree.namedObject.tern(&"({tree.name} ", "("), ")"))

func lispRepr*(tree: ValObjTree, maxlevel: int = 60): string =
  lispReprImpl(tree, TreeReprParams(
    maxDepth: maxlevel,
  ), level = 0)

#==============================  Tree repr  ==============================#
func treeReprImpl*(tree: ValObjTree,
                   params: TreeReprParams,
                   pref: seq[bool],
                   parentMaxIdx, currIdx: int): seq[string] =
  let arrow =
    case tree.kind:
      of okComposed: "+-> "
      of okConstant: "+-> "
      of okSequence: "+-- "
      of okTable: "+-> "

  let prefStr =
    pref.mapIt(it.tern("|   ", "    ")).join("") &
    (pref.len > 0).tern(arrow, "")

  case tree.kind:
    of okConstant:
      return @[prefStr & tree.strLit]
    of okSequence:
      for idx, item in tree.valItems:
        result &= treeReprImpl(
          item,
          params,
          pref & @[currIdx != parentMaxIdx],
          parentMaxIdx = tree.valItems.len - 1,
          currIdx = idx
        )
    of okTable:
      raiseAssert("#[ IMPLEMENT ]#")
    of okComposed:
      if tree.sectioned:
        raiseAssert("#[ IMPLEMENT ]#")
      else:
        result &= prefStr & tree.name
        result &= concat mapPairs(tree.fldPairs) do:
          tree.namedFields.tern(@[prefStr & lhs], @[]) &
          treeReprImpl(
            rhs,
            params,
            pref & @[currIdx != parentMaxIdx],
            parentMaxIdx = tree.fldPairs.len - 1,
            currIdx = idx
          )

func treeRepr*(tree: ValObjTree,
               maxlevel: int = 60): string =
  treeReprImpl(
    tree,
    TreeReprParams(
      maxDepth: maxlevel
    ),
    @[], 0, 0).join("\n")
