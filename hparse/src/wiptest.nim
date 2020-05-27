import macros
import parsecomb
import regex

if false:
  let cat = parseString("cat")
  let dog = parseString("dog")

  echo parseOr(@[cat, dog])("cat", 0)
  echo parseOr(@[cat, dog])("dog", 0)
  echo parseAnd(@[cat, dog])("catdog", 0)

if true:
  static:
    let nl = parseRx(re(r"~(\d?)%"))
    echo nl("~%")
    echo nl("~8%")

static:
  if false:
    var m: RegexMatch
    discard find("**~90%", re r"~(\d+)?%", m, 1)
    echo m

type
  # FormatTok = object

  FormatAst = object
    args: seq[string]
    children: seq[FormatAst]

# echo string is openarray[char]
# proc t(s: openarray[char]) = discard
# t("Hello")
