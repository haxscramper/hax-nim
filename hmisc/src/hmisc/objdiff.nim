# {.push warning[UnusedImport] = off.}


import sugar, strutils, sequtils, strformat

## Diff two objects

import typeinfo

template objKind(o: untyped): AnyKind =
  when o is bool:
    akBool                  #  a ``bool``
  elif o is char:
    akChar                  #  a ``char``
  elif o is enum:
      akEnum                  #  an enum
  elif o is array:
    akArray                 #  an array
  elif o is object:
    akObject                #  an object
  elif o is tuple:
    akTuple                 #  a tuple
  elif o is set:
    akSet                   #  a set
  elif o is range:
    akRange                 #  a range
  elif o is ptr: # Nim pointer type
    akPtr                   #  a ptr
  elif o is ref:
    akRef                   #  a ref
  elif o is seq:
    akSequence              #  a sequence
  elif (o is proc):
    akProc                  #  a proc
  elif o is pointer: # Opaque pointer to data
    akPointer               #  a pointer
  elif o is string:
    akString                #  a string
  elif o is string:
    akCString               #  a cstring
  elif o is int:
    akInt                   #  an int
  elif o is int8:
    akInt8                  #  an int8
  elif o is int16:
    akInt16                 #  an int16
  elif o is int32:
    akInt32                 #  an int32
  elif o is int64:
    akInt64                 #  an int64
  elif o is float:
    akFloat                 #  a float
  elif o is float32:
    akFloat32               #  a float32
  elif o is float64:
    akFloat64               #  a float64
  # elif o is float128:
  #   akFloat128              #  a float128
  elif o is uint:
    akUInt                  #  an unsigned int
  elif o is uint8:
    akUInt8                 #  an unsigned int8
  elif o is uint16:
    akUInt16                #  an unsigned in16
  elif o is uint32:
    akUInt32                #  an unsigned int32
  elif o is uint64:
    akUInt64                #  an unsigned int64
  else:
    akNone                   ## invalid any

import macroutils, ast_pattern_matching, macros


import hmisc/hterms_nimast

proc parseEnumSet[Enum](node: NimNode): set[Enum] =
  case node.kind:
    of nnkIdent:
      return {parseEnum[Enum]($node)}
    of nnkInfix:
      assert node[0] == ident("..")
      return {parseEnum[Enum]($node[1]) .. parseEnum[Enum]($node[2])}
    of nnkCurly:
      for subnode in node.children:
        result.incl parseEnumSet[Enum](subnode)

    else:
      discard

import hmisc/helpers

macro switchType(expr, body: untyped): untyped =
  # echo body.treeRepr()
  var branchSets {.global, compiletime.}: set[AnyKind]
  proc registerSet(node: NimNode, anchor: NimNode): void =
    let parsed = parseEnumSet[AnyKind](node)
    let diff = branchSets * parsed
    if diff.len > 0:
      raiseAssert(
        "Wrong type match: expression " & posString(anchor) &
          " is not disjoint from previous branches. Overlaps: " &
          $(diff)
      )
    else:
      branchSets.incl parsed


  let rewrite = makeNodeRewriteSystem:
    rule:
      patt: Call([[setExpr]], [[caseBody]])
      outp:
        let setLiteral =
          case setExpr.kind:
            of nnkIdent: nnkCurly.newTree(setExpr)
            else: setExpr

        registerSet(setLiteral, setExpr)
        nnkElifExpr.newTree(
          nnkInfix.newTree(ident "in", ident "objType", setLiteral),
          caseBody
        )
    rule:
      patt: Infix(Ident(".."), [[start]], [[final]], [[caseBody]])
      outp:
        let setLiteral = nnkCurly.newTree(
            nnkInfix.newTree(ident "..", start, final)
        )

        registerSet(setLiteral, start)
        nnkElifExpr.newTree(
          nnkInfix.newTree(ident "in", ident "objType", setLiteral),
          caseBody
        )


  let term = body.toTerm()
  let reduced = reduce(term, rewrite, nimAstImpl)

  var kindSet: set[AnyKind]
  for val in disjointIter(AnyKind):
    kindSet.incl val


  if reduced.ok:
    let diff = (kindSet - branchSets)
    if diff.len > 0:
      raiseAssert("Not all cases are covered in type match " &
        posString(expr) & ". Missing " & $diff)

    result = nnkWhenStmt.newTree(
      toSeq(reduced.term.fromTerm().children()) & @[
        nnkElse.newTree(
          nnkStmtList.newTree(
            nnkDiscardStmt.newTree(
              newEmptyNode())))])

    echo result.toStrLit()
    let kindId = ident "objType"
    result =
      quote do:
        const `kindId`: AnyKind = objKind(`expr`)
        `result`


switchType(12):
  akNone:
    discard
