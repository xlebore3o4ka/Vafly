import std/[os, parseopt, tables, strutils]
import parser, forms, evaluator

proc repl(ctx: Context, internmentData: InternmentData) =
  echo ":quit / :q to exit"
  while true:
    stdout.write(">>> ")
    stdout.flushFile()

    let line = stdin.readLine()

    if line.len == 0:
      continue
    if line == ":q" or line == ":quit":
      break

    let code = line.parse("<repl>", internmentData)
    var res: Form

    if code.kind == fkErr:
      stderr.writeLine("Error: " & code.errMsg)
      continue

    for form in code.mapValue.values:
      res = ctx.eval(form)

    if res.isNil():
      continue

    if res.kind == fkErr:
      stderr.writeLine("Error: " & res.errMsg)
    else:
      echo res.toStr(internmentData)

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