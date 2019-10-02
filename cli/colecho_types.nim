import terminal, sequtils, strutils

type
  MessageType* = enum
    mLog
    mInfo
    mWarn
    mError

  MessageStyle* = enum
    sDefault
    sVerbose
    sGtest
    sBright
    sLog

  ColoredString* = object
    str*: string
    fg*: ForegroundColor
    bg*: BackgroundColor
    style*: set[Style]


proc toRed*(str: string): ColoredString =
  ColoredString(str: str, fg: fgRed)

proc toGreen*(str: string): ColoredString =
  ColoredString(str: str, fg: fgGreen)


proc `$`*(colored: ColoredString): string =
  let fgCode = if colored.fg.int != 0:
      ansiForegroundColorCode(
        fg = colored.fg,
        bright = styleBright in colored.style)
    else:
      ""

  let bgCode = if colored.bg.int != 0:
      ansiStyleCode(
        colored.bg.int +
        (if styleBright in colored.style: 60 else: 0))
      else:
        ""

  toSeq(colored.style).mapIt(ansiStyleCode(it)).join &
    fgCode &
    bgCode &
    colored.str &
    ansiStyleCode(0)


proc len*(str: ColoredString): int = str.str.len
