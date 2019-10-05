import strformat, strutils, sequtils
import ../cli/argparse
import macros

import re
import os
import times

const newDayAfter = 5
const logMinDelay = 8

proc getCurrentNote(fileDirectory: string): string =
  ## Return path to current daily note
  var time = now()
  if time.hour < newDayAfter:
    time.monthday = time.monthday - 1

  fileDirectory & "/" & time.format("yyyy-MM-dd") & ".org"

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

  return now0000 - file0000 > logMinDelay or (
    lastHour == 0 and lastMinute == 0)

proc getTimeStampNow(): string =
  "@time:" & now().format("HH:mm") & ";"

proc addNewLog(note: string): void =
  let file = note.open(fmAppend)
  file.write("\n** " & getTimeStampNow() & "\n\n\n")
  file.close()

proc createNewNote(note: string): void =
  let head = "#+TITLE: @date:" &
    now().format("yyyy-MM-dd") &
    "; " & getTimeStampNow() & "\n\n"

  let org_time = now().format("yyyy-MM-dd ddd hh:mm")
  let tail = &"""
* Tasks
** TODO [/]
   DEADLINE: <{org_time}>
*** TODO <++>

* Logs

""" & "** " & getTimeStampNow() & "\n\n"

  let file = note.open(fmWrite)
  file.write(head & tail)
  file.close()

proc fileIsEmpty(note: string): bool =
  note.readFile().len == 0


parseArgs:
  opt:
    name: "modify-file"
    opt: ["--mod-file"]
    help: "Whether or not to append new log or create missing file"
  opt:
    name: "file-dir"
    opt: ["--file-dir", "+takes_values"]
    help: "Directory for note file"
  opt:
    name: "update-symlink"
    opt: ["--update-symlink"]
    help: "Udate symbolic link for 'today' note"


let fileDirectory =
  if "file-dir".kp:
    let dir = "file-dir".k.toStr()
    if dir.endsWith("/"): dir[0..^2]
    else: dir
  else:
    getHomeDir() & ".config/hax-local/dirs/personal/notes/daily/"


let note = getCurrentNote(fileDirectory)

if "modify-file".kp:
  if not fileExists(note) or note.fileIsEmpty():
    createNewNote(note)

  if noteAppendRequired(note):
    addNewLog(note)

if "update-symlink".kp:
  removeFile(fileDirectory & "/today.org")
  createSymlink(src = note, dest = fileDirectory & "/today.org")

echo note
