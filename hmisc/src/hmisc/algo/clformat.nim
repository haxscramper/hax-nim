import strutils

const romanNumerals = [
  (1000, "M"),
  (900, "CM"),
  (500, "D"),
  (400, "CD"),
  (100, "C"),
  (90, "XC"),
  (50, "L"),
  (40, "XL"),
  (10, "X"),
  (9, "IX"),
  (5, "V"),
  (4, "IV"),
  (1, "I")
]

func toRomanNumeral*(x: int): string =
  var x = x
  for (num, numStr) in romanNumerals:
    result.add(numStr.repeat(x div num))
    x = x mod num

func toPluralNoun*(noun: string, count: int, addNum: bool = true): string =
  # NOTE placeholder implementation,
  # TODO implement algorith described here: http://users.monash.edu/~damian/papers/HTML/Plurals.html
  if count == 1:
    return $count & " " & noun
  else:
    return $count & " " & noun & "s"


when isMainModule:
  echo toRomanNumeral(12)
  # echo toPluralNoun("item", 0)
