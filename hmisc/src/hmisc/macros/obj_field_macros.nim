import macroutils
import strutils, strformat, macros, sequtils

import ../algo/halgorithm
import ../helpers
import ../types/hnim_ast

proc getFields*(node: NimNode): seq[Field[NimNode]]

proc getBranches(node: NimNode): seq[FieldBranch[NimNode]] =
  assert node.kind == nnkRecCase, &"Cannot get branches from node kind {node.kind}"
  let caseType = $node[0][1]
  for branch in node[1..^1]:
    case branch.kind:
      of nnkOfBranch:
        result.add FieldBranch[NimNode](
          ofValue: (newTree(nnkCurly, branch[0..^2])).normalizeSet(),
          flds: branch[^1].getFields(),
          isElse: false
        )
      of nnkElse:
        result.add FieldBranch[NimNode](
          flds: branch[0].getFields(), isElse: true
        )
      else:
        raiseAssert(&"Unexpected branch kind {branch.kind}")


proc getFieldDescription(node: NimNode): tuple[name, fldType: string] =
  case node.kind:
    of nnkIdentDefs:
      return (
        name: $node[0],
        fldType: $(node[1].toStrLit)
      )
    of nnkRecCase:
      return getFieldDescription(node[0])

    else:
      raiseAssert(
        &"Cannot get field description from node of kind {node.kind}")

proc getFields*(node: NimNode): seq[Field[NimNode]] =
  case node.kind:
    of nnkObjConstr:
      # echo node.treeRepr()
      return getFields(node[0])
    of nnkSym, nnkCall, nnkDotExpr:
      let kind = node.getTypeImpl().kind
      case kind:
        of nnkBracketExpr:
          let typeSym = node.getTypeImpl()[1]
          # echo "Type symbol: ", typeSym.treeRepr()
          # echo "Impl: ", typeSym.getTypeImpl().treeRepr()
          result = getFields(typeSym.getTypeImpl())
        of nnkObjectTy, nnkRefTy:
          result = getFields(node.getTypeImpl())
        else:
          raiseAssert("Unknown parameter kind: " & $kind)
    of nnkObjectTy:
      return node[2].getFields()
    of nnkRecList:
      for elem in node:
        let descr = getFieldDescription(elem)
        case elem.kind:
          of nnkRecCase: # Case field
            result.add Field[NimNode](
              isKind: true,
              branches: getBranches(elem),
              name: descr.name,
              fldType: descr.fldType
            )

          of nnkIdentDefs: # Regular field definition
            result.add getFields(elem)[0]

          else:
            discard

    of nnkIdentDefs:
      let descr = getFieldDescription(node)
      result.add Field[NimNode](
        isKind: false,
        name: descr.name,
        fldType: descr.fldType
      )
    # of nnkIntLit:
    #   let descr = getFieldDescription(node)
    #   result.add Field[NimNode](
    #     isKind: false,
    #     name: descr.name,
    #     fldType: descr.fldType
    #   )
    else:
      raiseAssert(
        &"Unexpected node kind in `getFields` {node.kind}. Code: `{$node.toStrLit()}`, object repr: " &
        $node.getTypeImpl().toStrLit()
      )

proc getKindFields*[Node](flds: seq[Field[Node]]): seq[Field[Node]] =
  for fld in flds:
    if fld.isKind:
      result.add Field[Node](
        isKind: true,
        name: fld.name,
        value: fld.value,
        fldType: fld.fldType,
        branches: fld.branches.mapIt(
          FieldBranch[Node](
            value: it.value,
            isElse: it.isElse,
            flds: it.flds.getKindFields()
          )
        ).filterIt(it.flds.len > 0)
      )

proc discardNimNode(input: seq[Field[NimNode]]): seq[ValField] =
  for fld in input:
    case fld.isKind:
      of true:
        result.add ValField(
          isKind: true,
          name: fld.name,
          fldType: fld.fldType,
          selected: fld.selected,
          branches: fld.branches.mapIt(
            ValFieldBranch(
              value: ValObjTree(
                kind: okConstant,
                constType: (it.isElse).tern("", fld.fldType),
                strLit: (it.isElse).tern("", $it.ofValue.toStrLit())
              ),
              isElse: it.isElse,
              flds: it.flds.discardNimNode()
            )
          )
        )

      of false:
        result.add ValField(
          isKind: false,
          name: fld.name,
          fldType: fld.fldType
        )

macro makeFieldsLiteral*(node: typed): seq[ValField] =
  result = newLit(node.getFields().discardNimNode)


proc unrollFieldLoop(
  flds: seq[Field[NimNode]],
  body: NimNode,
  fldIdx: int,
  genParam: tuple[
    lhsObj, rhsObj, lhsName, rhsName, idxName, isKindName, fldName: string]
     ): tuple[node: NimNode, fldIdx: int] =

  result.node = newStmtList()
  var fldIdx: int = fldIdx
  for fld in flds:
    var tmpRes = newStmtList()
    # echo &"Fld idx: {fldIdx} for {fld.name}"
    let lhsId = ident(genParam.lhsName)
    let rhsId = ident(genParam.rhsName)
    let fldId = ident(fld.name)
    tmpRes.add superquote do:
      const `ident(genParam.isKindName)`: bool = `newLit(fld.isKind)`
      let `ident(genParam.idxName)`: int = `newLit(fldIdx)`
      let `lhsId` = `ident(genParam.lhsObj)`.`fldId`
      let `rhsId` = `ident(genParam.rhsObj)`.`fldId`
      let `ident(genParam.fldName)`: string = `newLit(fld.name)`
      block:
        `body`

    inc fldIdx
    if fld.isKind:
      # TODO check if two field kinds are identical
      var caseBlock = nnkCaseStmt.newTree(
        newDotExpr(ident genParam.lhsObj, ident fld.name)
      )

      for branch in fld.branches:
        let (branchBody, lastIdx) =
          branch.flds.unrollFieldLoop(body, fldIdx, genParam)

        fldIdx = lastIdx
        caseBlock.add nnkOfBranch.newTree(
          branch.ofValue,
          newStmtList(
            branchBody
          )
        )

      tmpRes.add superquote do:
        if `ident(genParam.lhsObj)`.`fldId` == `ident(genParam.rhsObj)`.`fldId`:
          `caseBlock`

    result.node.add quote do:
      block:
        `tmpRes`


  result.node = newBlockStmt(result.node)
  result.fldIdx = fldIdx
  # echo result.node.toStrLit()


macro parallelFieldPairs*(lhsObj, rhsObj: typed, body: untyped): untyped =
  ##[

Iterate two objects in parallel. Works for case objects.

Similar to parallel `fieldPairs` but also works for case objects.
Allows to iterate two objects at once, while keeping track of `kind`
fields for each type. The body is unrolled and variables are injected
for each field.

## Injected variables

:name: name of the current field
:lhs, rhs: value of current fields
:fldIdx: int. Index of current field in the object.
:lshObj, rhsObj: Original objects being iterated. [1]_
:isKind:
  bool. Whether or not current field is used as case parameter for object


[1] Useful when iterating over results of expression

  ]##

  let genParams = (
    lhsObj: "lhsObj",
    rhsObj: "rhsObj",
    lhsName: "lhs",
    rhsName: "rhs",
    idxName: "fldIdx",
    isKindName: "isKind",
    fldName: "name"
  )

  let (unrolled, _) = getFields(lhsObj).unrollFieldLoop(body, 0, genParams)

  result = superquote do:
    block:
      let `ident(genParams.lhsObj)` = `lhsObj`
      let `ident(genParams.rhsObj)` = `rhsObj`
      `unrolled`

 # echo result.toStrLit()
