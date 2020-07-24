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

proc toRomanNumeral*(x: int): string =
  var x = x
  for (num, numStr) in romanNumerals:
    result.add(numStr.repeat(x div num))
    x = x mod num


when isMainModule:
  echo toRomanNumeral(12)
