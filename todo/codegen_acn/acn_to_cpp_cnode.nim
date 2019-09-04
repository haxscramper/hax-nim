import acn_creator
import code_types
import strutils
from sequtils import map, mapIt, concat, toSeq
import strformat
import type_templating

proc cnode_to_string*(cnode: CNode): string =
  let head = if cnode.code != "":
               cnode.code & " "
             else:
               ""

  let rest = join(map(cnode.under, cnode_to_string), "\n")

  return head & rest


proc acn_to_cnode*(acn: Acn, spec: TypeTemplates): CNode


proc type_to_string*(t: Type, spec: TypeTemplates): string =
  var res = case t.kind:
    of int_t:
      "Int"
    of string_t:
      "String"
    of enum_t, other_t:
      t.name
    of vec_t:
      "Vec< $# >" % typeToString(t.subtypes[0], spec)
    of hash_t:
      "Hash< $#, $# >" % [
        typeToString(t.subtypes[0], spec),
        typeToString(t.subtypes[1], spec)]


  res = if ptr_t in t.spec: res & "*" else: res
  res = if const_t in t.spec: "const " & res else: res

  return res

proc varToString(t: Var, spec: TypeTemplates): string =
  realizeType(t.vtyp, spec) & " " & t.name



proc cls_section_to_cnode(sect: ClsSection, spec: TypeTemplates): CNode =
  let sect_comm = if sect.comm != "":
                    "//#=== " & sect.comm & "\n"
                  else: ""

  let acs_type = case sect.acsType:
                   of acsPublic: "public:"
                   of acsPrivate: "private:"
                   of acsProtected: "protected:"

  CNode(
    code: sect_comm & acs_type,
    under: sect.body.mapIt(
      acn_to_cnode(it[], spec)))


proc acn_class_to_cnode(acn: Acn, spec: TypeTemplates): CNode =
  let parent_cnodes =
    if acn.parents.len == 0:
      @[CNode(code: "{")]
    else:
      @[CNode(
        code: " : " &
          join(
            map(
              acn.parents,
              proc(par: (string, string)): string =
                par[0] & " " & par[1] & " "),
            " , ")),
        CNode(code: "{")]

  let section_cnodes = acn.sections.mapIt(
    cls_section_to_cnode(it, spec))

  let body_cnodes = acn.body.mapIt(acn_to_cnode(it, spec))


  CNode(
    code: &"class {acn.name}",
    under: concat(
      parent_cnodes,
      body_cnodes,
      section_cnodes,
      @[CNode(code: "};\n\n")]))


proc acn_enum_to_cnode(acn: Acn): CNode =
  CNode(
    code: "enum class $# {" % acn.name,
    under: concat(map(
      acn.eFields,
      proc (eVar: Var): CNode =
        CNode(code: eVar.name & ", ")), @[CNode(code: "};\n")]))


proc acn_function_to_cnode(acn: Acn, spec: TypeTemplates): CNode =
  CNode(
    under: @[
      CNode(code: acn.restype),
      CNode(code: acn.name),
      CNode(code: "("),
      CNode(code: acn
        .args
        .mapIt(var_to_string(it, spec))
        .join(",")),
      CNode(code: ") {\n"),
      CNode(under: acn.body.mapIt(acn_to_cnode(it, spec))),
      CNode(code: "}")
  ])

proc acn_pred_to_cnode(acn: Acn): CNode =
  CNode(code: acn.code)


proc body_to_cnodes(
  body: seq[Acn],
  spec: TypeTemplates,
  closing: string = "}"): seq[CNode] =

  concat(
    body.mapIt(acn_to_cnode(it, spec)),
    @[CNode(code: closing)])

proc acn_if_stmt_to_cnode(acn: Acn, spec: TypeTemplates): CNode =
  let comm = if acn.comm == "": "" else: "\n//" & acn.comm & "\n"

  CNode(
    code: comm &
      "if ( $# ) {" % cnode_to_string(acn_pred_to_cnode(acn.cond)),
    under: body_to_cnodes(acn.body, spec))


proc acn_else_if_stmt_to_cnode(acn: Acn, spec: TypeTemplates): CNode =
  let comm = if acn.comm == "": "" else: "//" & acn.comm & "\n"

  CNode(
    code: comm & "else if ( $# ) {" %
    cnode_to_string(
      acn_pred_to_cnode(acn.cond)),
    under: body_to_cnodes(acn.body, spec))

proc acn_else_stmt_to_cnode(acn: Acn, spec: TypeTemplates): CNode =
  CNode(code: "else {", under: body_to_cnodes(acn.body, spec))

proc acn_code_to_cnode(acn: Acn): CNode =
  CNode(code: acn.code)



proc acn_switch_to_cnode(acn: Acn, spec: TypeTemplates): CNode =
  proc make_one_case(
    cs: tuple[
      case_var: string,
      action: Acn]): CNode =


    CNode(
      code:
      "case $#: { $# } break;" % [
        cs.case_var,
        cnode_to_string(acn_to_cnode(cs.action, spec))])

  CNode(
    code: "switch ($#) {" % acn.swVar.name,
    under: map(acn.swCases, make_one_case))

proc acn_field_to_cnode(acn: Acn, spec: TypeTemplates): CNode =
  CNode(code: var_to_string(acn.val, spec) & ";")

proc acn_while_to_cnode(acn: Acn, spec: TypeTemplates): CNode =
  CNode(
    code: "while ( $# ) {" % cnode_to_string(
      acn_pred_to_cnode(acn.cond)),
    under: body_to_cnodes(
      acn.body, spec))

proc acn_to_cnode(acn: Acn, spec: TypeTemplates): CNode =
  CNode(
    code: "",
    under: @[
      case acn.kind:
        of acnCode:       acn_code_to_cnode         acn
        of acnEnum:       acn_enum_to_cnode         acn
        of acnClass:      acn_class_to_cnode        acn, spec
        of acnFunction:   acn_function_to_cnode     acn, spec
        of acnPredicate:  acn_function_to_cnode     acn, spec
        of acnIfStmt:     acn_if_stmt_to_cnode      acn, spec
        of acnElseIfStmt: acn_else_if_stmt_to_cnode acn, spec
        of acnElseStmt:   acn_else_stmt_to_cnode    acn, spec
        of acnSwitch:     acn_switch_to_cnode       acn, spec
        of acnField:      acn_field_to_cnode        acn, spec
        of acnWhile:      acn_while_to_cnode        acn, spec
  ])



proc call_function_to_str(
  function: string,
  spec: TypeTemplates,
  args: seq[Acn] = @[]): string =
    function &
      "(" &
      args
      .mapIt(acn_to_cnode(it, spec))
      .map(cnode_to_string)
      .join(", ") &
        ")"


proc call_function(
  meth: string,
  spec: TypeTemplates,
  args: seq[Acn] = @[]): Acn =
    make_acn_code(
      call_function_to_str(
        meth, spec, args))


proc call_method_of(
  obj: Var,
  meth: string,
  spec: TypeTemplates,
  args: seq[Acn] = @[]): Acn =
    let call_operator =
      if ptr_t in obj.vtyp.spec: "->"
      else:                      "."

    let code: string = obj.name &
      call_operator &
      call_function_to_str(meth, spec, args)

    return make_acn_code(code)



proc make_var_reader(
  v: Var,
  cls: Acn,
  spec: TypeTemplates,
  stream_name: string = "xmlStream->name()",
  read_next_elem: string = "xmlStream->readNextStartElement()",
  else_skip = make_acn_else("xmlStream->skipCurrentElement();")
     ): Acn =

  let stream = Var(
    name: "xmlStream",
    vtyp: make_ptr_type("QXmlStreamReader"))
  let target = Var(name: "target", vtyp: make_ptr_type(cls.name))
  let tags_name = cls.name.toLowerAscii()

  return case v.vtyp.kind:
    of vec_t:
      make_acn_while(
        cond = read_next_elem,
        body = @[
          make_comm("Read single item from " & v.name),
          make_acn_if(
            make_acn_predicate(
              stream_name &
                " == " &
                tags_name &
                "." &
                v.name &
                "_item"),
            @[]),
          else_skip
      ])

    of enum_t:
      call_method_of(
        target,
        "set" & v.name.capitalizeAscii,
        spec,
        @[call_function(
          "string_to_" & v.vtyp.name,
          spec,
          @[call_method_of(stream, "readElementText", spec)])])

    else:
      call_method_of(
        target,
        "set" & v.name.capitalizeAscii,
        spec,
        @[call_method_of(stream, "readElementText", spec)]
      )



iterator make_field_reader(
  clFields: seq[Var],
  cls: Acn,
  spec: TypeTemplates,
  else_skip: Acn = make_acn_else("xmlStream->skipCurrentElement();"),
  stream_name: string = "xmlStream->name()"): Acn {.inline.} =

  for i in 0..<len(clFields):
    let field: Var = clFields[i]
    let cond = stream_name &
      " == tags->" &
      cls.name.toLowerAscii &
      "." &
      field.name

    let code = make_var_reader(field, cls, spec)
    let comm = "Matches " & cls.name & "::" & field.name

    if i == 0:
      yield make_acn_if(cond, code, comm)
    else:
      yield make_acn_else_if(cond, @[code], comm)

  yield else_skip



## Generate code for parsing QXmlStreamReader into instance of the
## class
proc acn_class_to_xml_reader*(cls: Acn, spec: TypeTemplates): Acn =
  echo "=== acn_class_to_xml_reader"
  let func_name = "read" & cls.name & "XML"
  let restype = "void"
  let args = @[
    Var(
      name: "target",
      vtyp: make_class_ptr_type(cls)),
    Var(
      name: "xmlStream",
      vtyp: make_ptr_type("QXmlStreamReader")),
    Var(
      name: "_tags",
      vtyp: make_ptr_type("void"))
  ]

  let class_fields: seq[(Var, AcsType)] = cls.get_class_fields()


  echo class_fields.mapIt(it[0].name).join("\n")

  let stream_name = "xmlStream->name()"

  iterator enumerate[T](s: seq[T]): (int, T) =
    var i = 0
    while i < len(s):
      yield (i, s[i])
      i += 1

  let
    else_skip = make_acn_else("xmlStream->skipCurrentElement();")
    read_next_elem = "xmlStream->readNextStartElement()"
    field_readers = toSeq(make_field_reader(
      class_fields.mapIt(it[0]), cls, spec, else_skip))

  let tags_class = cls.name & "::" & cls.name & "XMLTags"
  let field_read_while = make_acn_while(read_next_elem, field_readers)

  let base_selector_while = make_acn_while(
    read_next_elem,
    @[make_acn_if(
      stream_name & " == tags->base.section",
      "\n// read base metadata xml"),
      make_acn_else_if(
        stream_name &
          " ==  tags->" &
          cls.name.toLowerAscii &
          ".section",
        @[field_read_while]),
      else_skip])

  let body = @[
    make_acn_code(tags_class & "* tags;"),
    make_acn_if("_tags == nullptr", "tags = &target->xmlTags;"),
    make_acn_else("tags = static_cast<" & tags_class & "*>(_tags)"),
    make_acn_code(""),
    base_selector_while]

  defer:
    echo "=== ###"

  return Acn(
    kind: acnFunction,
    name: func_name,
    args: args,
    restype: restype,
    body: body)
