if paramStr(paramCount()) == "test8.nim":
  exec("nim r generate_semhack.nim")
  patchFile("compiler", "sem", "semhack")
