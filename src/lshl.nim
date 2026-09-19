import std/[os, parseopt]
import parser, forms

proc main() =
  var
    filename: string

  for kind, key, val in getopt():
    case kind
    of cmdLongOption, cmdShortOption:
      case key
      # of "locale", "l": locale = val
      else: discard
    of cmdArgument:
      if filename == "": filename = key
    of cmdEnd: discard

  if filename == "":
    echo "Using: ", getAppFilename().extractFilename(), " <file>"
    return

  if not fileExists(filename):
    echo "Error: file not found - ", filename
    return

  let content = readFile(filename)
  let internmentData = newInternmentData()
  let code = content.parse(filename, internmentData)
  #let ctx = icontext()

  #for form in code.values:
  #  discard ctx.eval(form)
  echo code.toStr(internmentData)

when isMainModule:
  main()
