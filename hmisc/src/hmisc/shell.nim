import streams, strutils, osproc

iterator iterstdout*(command: string): string =
  let pid = startProcess(command, options = {poEvalCommand})

  let outStream = pid.outputStream
  var line = ""

  while pid.running:
    try:
      let streamRes = outStream.readLine(line)
      if streamRes:
        yield line
    except IOError, OSError:
      assert outStream.isNil

  let rem = outStream.readAll().split("\n")
  for line in (if rem.len > 0: rem[0..^2] else: rem):
    yield line
