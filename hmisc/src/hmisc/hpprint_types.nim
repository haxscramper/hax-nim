import helpers, halgorithm, hmisc_types, hdrawing
import unicode
import sugar
export hdrawing
import sequtils, colors
import tables, strformat, strutils
import math


type
  ObjElem* = object
    text*: string
    color*: Color

func makeObjElem*(text: string): ObjElem =
  ObjElem(text: text)


func toStrings*(str: string): seq[string] =
  str.toSeq().mapIt($it)
