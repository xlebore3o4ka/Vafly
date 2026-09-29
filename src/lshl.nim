import std/[os, parseopt, tables, rdstdin]
import parser, forms, evaluator

proc printEnv(ctx: Context, internmentData: InternmentData) =
  let frame = ctx.env.get(newSymForm(`ENV-CURRENT`))
  for key, val in frame.mapValue.pairs:
    if val.kind == fkMap and val.posValue.len == 1 and
       val.get(0).kind == fkSym and val.get(0).symInterned == `EVAL-BUILTIN`:
      continue
    let name = internmentData.unintern(key.symInterned)
    stdout.writeLine(name & " = " & val.toStr(internmentData))

proc repl(ctx: Context, internmentData: InternmentData) =
  echo ":quit / :q to exit ; :env to show environment"
  var counter = 0
  var sources = initTable[string, string]()

  while true:
    let line = readLineFromStdin($counter & "   >>> ")

    if line.len == 0:
      continue
    if line == ":q" or line == ":quit":
      break
    elif line == ":env":
      printEnv(ctx, internmentData)
      continue

    inc counter
    let filename = "<repl:" & $counter & ">"
    sources[filename] = line

    let code = line.parse(filename, internmentData)
    var res: Form

    if code.kind == fkErr:
      stderr.writeLine("Error: " & code.errMsg)
      continue

    for form in code.posValue:
      res = ctx.eval(form)

    if res.isNil() or res.kind == fkNil:
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
  var sources = initTable[string, string]()
  sources[filename] = content

  let code = content.parse(filename, internmentData)

  if code.kind == fkErr:
    stderr.writeLine("Error: " & code.errMsg)
    return

  let ctx = newContext(internmentData)

  for form in code.posValue:
    let res = ctx.eval(form)
    if res.kind == fkErr:
      let loc = res.locationData
      if loc.filename != nil and sources.hasKey(loc.filename[]):
        echo sources[loc.filename[]].errFormToStr(res, internmentData)
      else:
        echo "Error: " & res.errMsg

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