import std/[os, parseopt, tables, rdstdin]
import parser, forms, evaluator, transpiler

proc printEnv(ctx: Context, internmentData: InternmentData) =
  let frame = ctx.env.get(newSymForm(`ENV-CURRENT`))
  for key, val in frame.mapValue.pairs:
    if val.kind == fkMap and val.posValue.len == 1 and
       val.get(0).kind == fkSym and val.get(0).symInterned == `EVAL-BUILTIN`:
      continue
    let name = internmentData.unintern(key.symInterned)
    stdout.writeLine(name & " = " & val.toStr(internmentData))

proc printErr(err: Form, sources: Table[string, string], data: InternmentData) =
  if err.locationData.filename != nil and sources.hasKey(err.locationData.filename[]):
    stderr.writeLine sources[err.locationData.filename[]].errFormToStr(err, data)
  else:
    stderr.writeLine "Error: " & err.errMsg

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

    for form in code.posValue:
      res = ctx.eval(form)

    if res.isNil() or res.kind == fkNil:
      continue

    if res.kind == fkErr:
      printErr(res, sources, internmentData)
    else:
      echo res.toStr(ctx.internmentData)

proc compileFile(input, output: string) =
  let content = readFile(input)
  let internmentData = newInternmentData()
  let form = content.parse(input, internmentData)

  var sources = initTable[string, string]()
  sources[input] = content

  let code = transpile(form, internmentData, sources)
  writeFile(output, code)

proc runFile(filename: string) =
  if not fileExists(filename):
    echo "Error: file not found - ", filename
    return

  let content = readFile(filename)
  let internmentData = newInternmentData()
  var sources = initTable[string, string]()
  sources[filename] = content

  let code = content.parse(filename, internmentData)

  let ctx = newContext(internmentData)

  for form in code.posValue:
    let res = ctx.eval(form)
    if res.kind == fkErr:
      printErr(res, sources, internmentData)
      quit(1)

proc main() =
  var inputFile: string
  var outputFile: string
  var compileMode: bool

  for kind, key, val in getopt():
    case kind
    of cmdLongOption, cmdShortOption:
      case key
      of "compile", "c": compileMode = true
      else: discard
    of cmdArgument:
      if inputFile == "": inputFile = key
      elif outputFile == "": outputFile = key
    of cmdEnd: discard

  if inputFile == "":
    if compileMode:
      stderr.writeLine("Warning: no input file specified (usage: lshl -c <file.vafl> [-o <output.nim>])\nEntering the REPL")
    let internmentData = newInternmentData()
    let ctx = newContext(internmentData)
    repl(ctx, internmentData)
  elif compileMode:
    if outputFile == "":
      outputFile = changeFileExt(inputFile, "nim")
    compileFile(inputFile, outputFile)  # TODO: lshl -c calculates the relative path from outputFile to src/sysvafly via relativePath.
  else:
    runFile(inputFile)

when isMainModule:
  main()