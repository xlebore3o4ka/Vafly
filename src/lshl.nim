import std/[os, parseopt, tables, rdstdin]
import parser, forms, evaluator

proc repl(ctx: Context, internmentData: InternmentData) =
  echo ":quit / :q to exit"
  var counter = 0
  var sources = initTable[string, string]()

  while true:
    let line = readLineFromStdin($counter & "   >>> ")

    if line.len == 0:
      continue
    if line == ":q" or line == ":quit":
      break

    inc counter
    let filename = "<repl:" & $counter & ">"
    sources[filename] = line

    let code = line.parse(filename, internmentData)
    var res: Form

    if code.kind == fkErr:
      stderr.writeLine("Error: " & code.errMsg)
      continue

    for form in code.mapValue.values:
      res = ctx.eval(form)

    if res.isNil():
      continue

    if res.kind == fkErr:
      let loc = res.locationData
      if loc.filename != nil and sources.hasKey(loc.filename[]):
        echo sources[loc.filename[]].errFormToStr(res, internmentData)
      else:
        echo "Error: " & res.errMsg

    echo res.toStr(ctx.internmentData)

proc runFile(filename: string) =
  if not fileExists(filename):
    echo "Error: file not found - ", filename
    return

  let content = readFile(filename)
  let internmentData = newInternmentData()
  let code = content.parse(filename, internmentData)

  if code.kind == fkErr:
    stderr.writeLine("Error: " & code.errMsg)
    return

  let ctx = newContext(internmentData)

  for form in code.mapValue.values:
    discard ctx.eval(form)

proc main() =
  var filename: string

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
    let internmentData = newInternmentData()
    let ctx = newContext(internmentData)
    repl(ctx, internmentData)
  else:
    runFile(filename)

when isMainModule:
  main()