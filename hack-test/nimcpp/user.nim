import os

const cxheader = currentSourcePath().splitPath().head / "lib.hpp"

type 
  PStr {.importcpp: "PStr", header: cxheader.} = ptr object
    a: cint

var h: PStr = nil
