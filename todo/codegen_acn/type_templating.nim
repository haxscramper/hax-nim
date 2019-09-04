import code_types
from sequtils import mapIt
import tables

## Substitute sequence of strings into type template.
proc subsTypeTemplate*(templ: TypeTemplate, args: seq[string]): string =
  for idx, str in templ.strLiterals:
    result &= templ.strLiterals[idx]
    if idx < templ.strLiterals.len - 1:
      result &= args[idx]

## Substitute single string into type template.
proc subsTypeTemplate*(templ: TypeTemplate, args: string): string =
  subsTypeTemplate(templ, @[args])


## Recursively substitute type to the `inType` using type templates
## provided by `typeSpec`
proc realizeType*(inType: Type, typeSpec: TypeTemplates): string =
  case inType.kind:
    of vec_t, hash_t:
      let subtypes = inType.subtypes.mapIt(realizeType(it, typeSpec))
      subsTypeTemplate(
        typeSpec.templates[inType.kind],
        subtypes)

    of string_t:
      subsTypeTemplate(typeSpec.templates[inType.kind], @[])

    of other_t:
      inType.name

    else:
      $inType.kind & " -- NOT IMPLEMENTED "
