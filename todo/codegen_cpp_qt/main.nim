import parsetoml
import strscans
import re
import macros
import ../codegen_acn/code_types
import ../codegen_acn/acn_to_cpp_cnode
import ../codegen_acn/acn_creator
import ../codegen_acn/type_templating
import strutils
import pegs
from sequtils import map, mapIt




proc toStr(t: TypeKind): string =
  result = ($t).replace("_t").capitalizeAscii()

## Peform substitution of fake template parameters and print resulting
## types
proc debugTypeSpec(spec: TypeTemplates) =
  let fil = @[" !!! ", " @@@ ", " ### "]

  proc print_type(name: string, t: TypeTemplate) =
    echo name & " type is: [ " & t.subsTypeTemplate(fil) & " ]"

  for key, val in spec.templates:
    print_type($key, val)


## Parse type template from string. Parsing is done by splitting
## string using regular expression: ``{{\d}}``
proc parseTypeTemplate(str: string): TypeTemplate =
  TypeTemplate(strLiterals: str.split(re"{{\d}}"))



proc parseTypeSpecFile(tomlFile: string): TypeTemplates =
  let typeConfFile = parsetoml.parseFile(tomlFile)

  let typeConf = typeConfFile["type"]

  for e in TypeKind:
    if e != enum_t and e != other_t:
      try:
        result.templates[e] = typeConf[
          e.toStr()]["type"].getStr().parseTypeTemplate()
      except KeyError:
        echo "Could not find type specification for " & e.toStr()



## Recursivelt iterate over `Type` and print all of the nested types
proc printTypeTree(t: Type, level: int = 0): void =
  let prefix = "    ".repeat(level)
  echo prefix, t.kind
  case t.kind:
    of vec_t:
      printTypeTree(t.subtypes[0], level + 1)
    of other_t, int_t, enum_t, string_t:
      discard nil
    else:
      echo " --- "

  if level == 0: echo ""


# =====


proc parseTypeSpec(str:string): Type


## Given sequence of type tokens parse them into `Type` variable.
## Second value in the returning tuple is used for internal purposes
## and might be safely discarded.
proc parseTypeSpecTree(
  tokens: seq[string],
  idx: int = 0,
  level: int = 0): (Type, int) =
  let prefix = "    ".repeat(level)
  var resIdx = idx

  # echo prefix, "Type token: ", tokens[idx]

  var subtypes: seq[Type]

  if tokens[resIdx + 1] == "<":
    let (sub, newIdx) = parseTypeSpecTree(tokens, idx + 2, level + 1)
    resIdx = newIdx + 1
    subtypes.add(sub)

    while (resIdx < tokens.len) and (tokens[resIdx] == ","):
      # echo Prefix, "-- Next"
      let (sub, newIdx) = parseTypeSpecTree(tokens, resIdx + 1, level + 1)
      subtypes.add(sub)
      resIdx = newIdx + 1

  elif tokens[resIdx + 1] == ":":
    let
      specStart = tokens[idx..^1].find("{") + idx + 1
      specEnd   = tokens[idx..^1].find("}") + idx - 1
      specKWDs  = tokens[specStart..specEnd]

    resIdx = specEnd + 1


  let typeKind = case tokens[idx]:
                   of "Vec": vec_t
                   of "String": string_t
                   of "Hash": hash_t
                   else: other_t

  var parent = Type(kind: typeKind)
  case parent.kind:
    of vec_t, hash_t:
      parent.subtypes = subtypes
    else:
      discard



  return (parent, resIdx)

proc parseTypeSpec(str:string): Type =
  parseTypeSpecTree(findAll(str, re"((\w+)|[<>{}:,])"))[0]



proc parseClassSpecFile(tomlFile: string): Acn =
  result = Acn(kind: acnClass)
  let classSpecFile = parsetoml.parseFile(tomlFile)
  let spec = classSpecFile["class"]

  result.name = spec["name"].getStr()

  let classFields = classSpecFile["field"].getElems()

  var section: ClsSection = ClsSection(acsType: acsPrivate)

  for f in classFields:
    section.body.add toRef makeAcnField(Var(
      name: f["name"].getStr(),
      vtyp: parseTypeSpec(f["type"].getStr())))


  result.sections.add(section)



var typeSpec = parseTypeSpecFile("types.toml")
var classSpec = parseClassSpecFile("class.toml")

let xmlReader = acn_class_to_xml_reader(classSpec, typeSpec)
let serialization = ClsSection(acsType: acsPublic, body: @[toRef(xmlReader)])
classSpec.sections.add(serialization)

var file = open("result.cpp.tmp", fmWrite)
file.write(cnode_to_string(acn_to_cnode(classSpec, typeSpec)))

file.close()
