import hnimast
import hmisc/other/[oswrap]
import std/[tables, strutils]
import nimblepkg/[version, packageinfo, cli]
import nimblepkg/options as nimble_options
import hpprint
import haxdoc



const nkWithSubnodeKinds*: set[TNodeKind] = {
  nkStmtList, nkWhenStmt, nkElifBranch,
  nkPrefix, nkConstSection, nkVarSection, nkLetSection,
  nkConstDef, nkAsgn, nkInfix, nkDotExpr, nkIfStmt,
  nkForStmt, nkWhileStmt, nkDiscardStmt, nkIdentDefs,
  nkCurly,

  nkNone, nkSym, nkType, nkNilLit, nkComesFrom, nkDotCall, nkCallStrLit,
  nkPostfix, nkHiddenCallConv, nkExprEqExpr, nkExprColonExpr, nkVarTuple,
  nkPar, nkObjConstr, nkCurlyExpr, nkBracket, nkBracketExpr, nkPragmaExpr,
  nkRange, nkCheckedFieldExpr, nkDerefExpr, nkIfExpr, nkElifExpr,
  nkElseExpr, nkLambda, nkDo, nkAccQuoted, nkTableConstr, nkBind,
  nkClosedSymChoice, nkOpenSymChoice, nkHiddenStdConv, nkHiddenSubConv,
  nkConv, nkCast, nkStaticExpr, nkAddr, nkHiddenAddr, nkHiddenDeref,
  nkObjDownConv, nkObjUpConv, nkChckRangeF, nkChckRange64, nkChckRange,
  nkStringToCString, nkCStringToString, nkFastAsgn, nkGenericParams,
  nkFormalParams, nkOfInherit, nkImportAs, nkOfBranch, nkExceptBranch,
  nkElse, nkAsmStmt, nkPragma, nkPragmaBlock, nkParForStmt, nkCaseStmt,
  nkTypeDef, nkYieldStmt, nkDefer, nkTryStmt, nkFinally, nkRaiseStmt,
  nkReturnStmt, nkBreakStmt, nkContinueStmt, nkBlockStmt, nkStaticStmt,
  nkImportExceptStmt, nkExportStmt, nkExportExceptStmt, nkIncludeStmt,
  nkBindStmt, nkMixinStmt, nkUsingStmt, nkCommentStmt, nkStmtListExpr,
  nkBlockExpr, nkStmtListType, nkBlockType, nkWith, nkWithout,
  nkTypeOfExpr, nkObjectTy, nkTupleTy, nkTupleClassTy, nkTypeClassTy,
  nkStaticTy, nkRecList, nkRecCase, nkRecWhen, nkRefTy, nkPtrTy, nkVarTy,
  nkConstTy, nkMutableTy, nkDistinctTy, nkProcTy, nkIteratorTy, nkSharedTy,
  nkEnumTy, nkEnumFieldDef, nkArgList, nkPattern, nkHiddenTryStmt,
  nkClosure, nkGotoState, nkState, nkBreakState, nkTupleConstr
}


const ignoreKinds = nkLiteralKinds + {
  nkImportStmt, nkFromStmt,
  nkIdent, nkEmpty, nkTypeSection
}

var rewriteTable: Table[string, PNode]
proc rewriteSemcheck(node: PNode): PNode =
  case node.kind:
    of nkWithSubnodeKinds - (ignoreKinds + {nkCall, nkCommand, nkProcDef}),
       (nkProcDeclKinds - {nkProcDef})
      :

      result = node
      for idx in 0 ..< node.len:
        result[idx] = rewriteSemcheck(node[idx])

    of ignoreKinds:
      result = node

    # of nkImportStmt:
    #   result = node
    #   # for idx in 0 ..< node.len:
    #   #   result[idx] = nnkInfix.newPTree(
    #   #     newPIdent("/"),
    #   #     newPIdent("compiler"),
    #   #     result[idx]
    #   # )

    of nkProcDef:
      result = node

      if result[6].kind != nnkEmpty:
        result[6] = rewriteSemcheck(result[6])

        let (exported, name) = parseIdentName(result[0])
        let name2 = name.getStrVal()
        if name2.startsWith("sem") and
           name2 notin ["semAfterMacroCall"]
          :
          result[6].add newPCall(
            "recordSemAction",
            newPIdent("c"),
            newPLit(name2),
            newPIdent("n"),
            newPIdent("result")
          )



    of nkCall, nkCommand:
      result = node

      for idx in 0 ..< node.len:
        result[idx] = rewriteSemcheck(node[idx])


var targetFiles: Table[string, AbsFile]
let targetNames = [
  "sem", "semtypes", "semtempl", "semgnrc", "semstmts", "semexprs",
  "seminst", "semcall"
]

setVerbosity(SilentPriority)

var options = initDefaultOptions()
let compilerPkg = findPackage("compiler", newVRany(), options).get()
let compilerSrcDir = AbsDir(compilerPkg.getRealDir()) / "compiler"

echo compilerSrcDir
import std/[strformat]

for file in listFiles(compilerSrcDir):
  if file.name in targetNames:
    targetFiles[file.name] = file

pprint targetFiles

for name in targetNames:
  rewriteTable[name] = rewriteSemcheck(
    parsePNodeStr(targetFiles[name].readFile()))

writeFile(
  "semhack.nim",
  "import semhack_logging\n" & $rewriteTable["sem"]
)
