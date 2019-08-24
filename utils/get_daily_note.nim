import strformat, strutils, sequtils

import re
import os
import times

const newDayAfter = 5
const logMinDelay = 8

proc getCurrentNote(): string =
  ## Return path to current daily note
  var time = now()
  if time.hour < newDayAfter:
    time.monthday = time.monthday - 1

  result = getHomeDir() &
    ".config/hax-local/dirs/personal/notes/daily/" &
    time.format("yyyy-MM-dd") &
    ".org"

proc noteAppendRequired(note: string): bool =
  var lastHour = 0
  var lastMinute = 0
  for line in note.readFile().split("\n"):
    if line =~ re"^\*\* @time:(\d\d):(\d\d);":
      lastHour = matches[0].parseInt()
      lastMinute = matches[1].parseInt()

  proc getSince0000(hour, minute: int): int =
    if hour < newDayAfter:
      minute + (24 + hour) * 60
    else:
      minute + hour * 60

  let now0000 = getSince0000(now().hour, now().minute)
  let file0000 = getSince0000(lastHour, lastMinute)

  return now0000 - file0000 > logMinDelay

proc getTimeStampNow(): string =
  "@time:" & now().format("hh:mm") & ";"

proc addNewLog(note: string): void =
  let file = note.open(fmAppend)
  file.write("\n** @time:" & now().format("hh:mm") & ";\n\n\n")
  file.close()

proc createNewNote(note: string): void =
  let head = "#+TITLE: @date:" &
    now().format("yyyy-MM-dd") &
    "; " & getTimeStampNow() & "\n\n"

  let tail = """
* Tasks
** TODO [/]
   + [ ]

* Logs

""" & "** " & getTimeStampNow() & "\n\n"

  let file = note.open(fmWrite)
  file.write(head & tail)
  file.close()

proc fileIsEmpty(note: string): bool =
  note.readFile().len == 0

let note = getCurrentNote()
if not fileExists(note) or note.fileIsEmpty():
  createNewNote(note)
elif noteAppendRequired(note):
  addNewLog(note)

echo note
