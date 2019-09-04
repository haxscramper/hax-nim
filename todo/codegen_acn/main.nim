import strformat
import sequtils
import strutils
import osproc

include acn_to_cpp_cnode




proc print_acn_tree(acn: Acn, level: int = 0) =
  let prefix = repeat(' ', level * 2)
  case acn.kind:
    of acnClass:
      echo prefix, "class ", acn.name
      for sect in acn.sections:
        echo prefix, "  ", case sect.acsType:
          of acsPublic: "public"
          of acsPrivate: "private"
          of acsProtected: "protected"

        for item in sect.body:
          print_acn_tree(item[], level + 2)

    of acnEnum:
      echo prefix, "enum"
      for field in acn.eFields:
        echo prefix, "  ", field.name
    of acnFunction:
      echo prefix, "function ",
       # IDEA create long functions that spans several line, one line
         # for each variable
         # func |
         #      | -> restype
         #      |
        acn.args.mapIt("[ " & type_to_string(it.vtyp) & " ]").join(" X "),
        " |-> ", acn.restype
    of acnPredicate:
      echo prefix, "predicate"
    of acnIfStmt:
      echo prefix, "if ", acn.cond.code
    of acnElseIfStmt:
      echo prefix, "else if"
    of acnElseStmt:
      echo prefix, "else"
    of acnCode:
      echo prefix, acn.code
    of acnSwitch:
      echo prefix, "switch ", acn.swVar.name
      echo join(
        map(
          acn.swCases,
          proc(cs: (string, Acn)): string = prefix & "  " & cs[0]),
        "\n")
    of acnField:
      echo prefix, acn.val.name, ": ", type_to_string(acn.val.vtyp)
    else:
      echo prefix, repr(acn.kind)

  for node in acn.body:
    print_acn_tree(node, level + 1)



var file = open("parse.cpp", fmWrite)
let enum_specs: seq[(string, seq[string])] =
  @[("Status", @["NoStatus", "Completed"])]

proc make_enum_type(acn_enum: Acn): Type =
  Type(kind: enum_t, eName: acn_enum.name)

let enum_fields = ClsSection(
  acsType: acsPrivate,
  body: enum_specs
  .mapIt(Var(
    name: it[0][0].toLowerAscii() & it[0][1..^1],
    vtyp: make_enum_type(it[0])))
  .map(make_acn_field)
  .map(to_ref))

let acn_enums = map(enum_specs, make_enum)

proc make_vec_t(item_name: string): Type =
  Type(kind: vec_t, vItem: Type(kind: other_t, oName: item_name))

proc make_int_t(): Type =
  Type(kind: int_t)

let class_test = Acn(
  kind: acnClass,
  name: "QSTodo"
).add_fields(
  @[
    Var(name: "weight1", vtyp: make_int_t()),
    Var(name: "weight2", vtyp: make_int_t()),
    Var(name: "attachments", vtyp: make_vec_t("Attachment"))
  ]
).add_section(
  section = enum_fields,
  comm = "enum fields"
)

let xml_converter = acn_class_to_xml_reader(class_test)

#print_acn_tree(class_test)


var file = open("parse.cpp", fmWrite)

write(file, (cnode_to_string(acn_to_cnode(class_test))))
write(file, (cnode_to_string(acn_to_cnode(xml_converter))))

close(file)
