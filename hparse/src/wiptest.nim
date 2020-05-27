import macros
import parsecomb
import regex

if false:
  let cat = parseString("cat")
  let dog = parseString("dog")

  echo parseOr(@[cat, dog])("cat", 0)
  echo parseOr(@[cat, dog])("dog", 0)
  echo parseAnd(@[cat, dog])("catdog", 0)

static:
  if false:
    let nl = parseRx(re(r"~(\d?)%"))
    echo nl("~%")
    echo nl("~8%")

static:
  if false:
    var m: RegexMatch
    discard find("**~90%", re r"~(\d+)?%", m, 1)
    echo m

if false:
  let nhashes = parseNTimes(parseString("#"), 2, 8)
  echo nhashes("#####")

  let nnewlines = parseZeroOrMore(parseOr(@[
    parseAnd(@[parseString("~"), parseString("%")]),
    parseAnd(@[parseString("$$")])
  ]))

  echo nnewlines("~%~%$$~%")

type
  FormatTok = object
    case command: bool
      of true:
        spec: string
        args: seq[string]
      of false:
        content: string

  FormatAst = object
    args: seq[string]
    children: seq[FormatAst]

let skipUntilFormat = parseSkipUntil[char, string]('~')
let matchFormatCtrl = parseRx(re r"~(C|%|&|\|)")

let matchTokens = parseNTimes(parseOr(@[
  skipUntilFormat, matchFormatCtrl
]), maxtimes = 10)

echo matchTokens("Heloo ~%")
