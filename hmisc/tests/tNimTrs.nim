import sugar, strutils, sequtils, strformat
import hmisc/[nim_trs, helpers, halgorithm]

#===========================  implementation  ============================#

type
  TrmKind = enum
    tmkF
    tmkC

  Trm = object
    case kind: TrmKind:
      of tmkF:
        subt: seq[Trm]
      of tmkC:
        val: int

func nT(sub: varargs[Trm]): Trm = Trm(kind: tmkF, subt: toSeq(sub))
func nT(val: int): Trm = Trm(kind: tmkC, val: val)
func `==`(lhs, rhs: Trm): bool =
  lhs.kind == rhs.kind and (
    case lhs.kind:
      of tmkC:
        lhs.val == rhs.val
      of tmkF:
        subnodesEq(lhs, rhs, subt)
  )

#======================  case term implementation  =======================#

type
  TrmTerm = Term[Trm, TrmKind]
  TrmEnv = TermEnv[Trm, TrmKind]

func nOp(subt: varargs[TrmTerm]): TrmTerm =
  makeFunctor[Trm, TrmKind](tmkF, toSeq(subt))

func nVar(n: string): TrmTerm =
  makeVariable[Trm, TrmKind](n)

func nConst(n: Trm): TrmTerm =
  makeConstant(n, n.kind)

func nConst(val: int): TrmTerm = nConst(nT(val))
func nConst(subt: varargs[Trm]): TrmTerm = nConst(nT(subt))

func mkEnv(vals: varargs[tuple[vname: string, val: TrmTerm]]): TrmEnv =
  for (name, val) in vals:
    result[name] = val

const trmImpl* = TermImpl[Trm, TrmKind](
  getSym: (proc(n: Trm): TrmKind = n.kind),
  isFunctorSym: (proc(kind: TrmKind): bool = kind == tmkF),
  makeFunctor: (proc(op: TrmKind, sub: seq[Trm]): Trm = nT(sub)),
  getSubt: (proc(n: Trm): seq[Trm] = n.subt),
  valStrGen: (proc(n: Trm): string = $n),
)

proc fromTerm(term: TrmTerm): Trm = term.fromTerm(trmImpl)
proc treeRepr(val: Trm): string = treeRepr(val, trmImpl)
proc treeRepr(val: TrmTerm): string = treeRepr(val, trmImpl)

#================================  tests  ================================#

import unittest

proc cmpTerm(term: TrmTerm | Trm, val: Trm | TrmTerm): void =
  let ok =
    (when term is TrmTerm: term.fromTerm() else: term) ==
    (when val is TrmTerm: val.fromTerm() else: val)

  if not ok:
    echo "Found:"
    echo treeRepr(term)
    echo "Expected:"
    echo treeRepr(val)
    fail()

suite "Nim trs primitives":
  test "To-from term convesion":
    let t = nT(nT(12), nT(2))
    assert t.toTerm(trmImpl).fromTerm(trmImpl) == t

  test "Variable substitution in env":
    cmpTerm nVar("ii").substitute(mkEnv({
      "ii" : nConst(nT(90))
    })), nT(90)

    cmpTerm nOp(nVar("ii"), nConst(nT(12))).substitute(mkEnv({
      "ii" : nConst(nT(90))
    })), nT(nT(90), nT(12))

    cmpTerm nOp(nOp(nOp(nVar("ii")))).substitute(mkEnv({
      "ii" : nConst(nT(120))
    })), nT(nT(nT(nT(120))))

    cmpTerm nOp(nVar("i1"), nVar("i2"), nVar("i3")).substitute(mkEnv({
      "i1" : nConst(nT(10)),
      "i2" : nConst(nT(20)),
      "i3" : nConst(nT(30)),
    })), nT(nT(10), nT(20), nT(30))

    cmpTerm nOp(nVar("ii"), nOp(nVar("ii"))).substitute(mkEnv({
      "ii" : nConst(nT(10))
    })), nT(nT(10), nT(nT(10)))

  test "{fromTerm} exception":
    try:
      discard nOp(nOp(nVar("ii"))).fromTerm()
      fail()
    except GenException[SubstitutionErrorInfo]:
      let e = getGEx[SubstitutionErrorInfo]
      assertEq e.info.path, @[0, 0, 0]
      assertEq e.info.vname, "ii"

  test "{unif} term unification tests":
     block:
       let res = unif(nVar("ii"), nConst(nT(12))).get()
       cmpTerm res["ii"], nConst(nT(12))

     block:
       let res = unif(nOp(nVar("ii")), nOp(nConst(nT(12)))).get()
       cmpTerm res["ii"], nConst(nT(12))

     block:
       let res = unif(
         nOp(nVar("ii"), nVar("ii")),
         nOp(nConst(nT(12)), nConst(nT(12)))
       ).get()

       cmpTerm res["ii"], nConst(nT(12))

     block:
       let res = unif(
         nOp(nVar("ii"), nConst(nT(12))),
         nOp(nConst(nT(12)), nVar("ii"))
       ).get()

       cmpTerm res["ii"], nConst(nT(12))

     block:
       let res = unif(
         nOp(nVar("ii"), nConst(nT(12)), nVar("zz")),
         nOp(nConst(nT(22)), nVar("qq"), nConst(nT(90)))
       ).get()

       cmpTerm res["ii"], nConst(22)
       cmpTerm res["qq"], nConst(12)
       cmpTerm res["zz"], nConst(90)

     block:
       let res = unif(
         nOp(nVar("ii"), nOp(nVar("io"), nConst(90)), nConst(90)),
         nOp(nConst(90), nOp(nConst(8), nConst(90)), nVar("ii"))
       ).get()

       cmpTerm res["ii"], nConst(90)
       cmpTerm res["io"], nConst(8)

# suite "Nim trs reduction rule search":