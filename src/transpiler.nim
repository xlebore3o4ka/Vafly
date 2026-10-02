import forms
import std/[tables, strutils]

type
  Transpiler = ref object
    internmentData: InternmentData
    body: seq[string]
    defs: seq[string]
    files: Table[string, string]
    sources: Table[string, string]
    locations: Table[(string, int, int), string]
    locCounter: int
    fileCounter: int

let expandableBuiltins*: Table[int, tuple[name: string, min, max: int]] = {
  `EVAL+`: ("OPT+",    0, -1),
  `EVAL-`: ("OPT-",    1, -1),
  `EVAL*`: ("OPT*",    0, -1),
  `EVAL-DIV`: ("OPT-DIV", 1, -1),
  `EVAL-MOD`: ("OPT-MOD", 1, -1),
}.toTable

let noOptBuiltins*: seq[int] = @[
  `PARSER-QUOTE`,
  `EVAL-LAMBDA`,
  `EVAL-DEFUN`,
  `EVAL-TRY`,
  `EVAL-IF`,
  `EVAL-COND`,
  `EVAL-AND`,
  `EVAL-OR`,
  `EVAL-ALL`,
  `EVAL-ANY`,
  `EVAL-LET`,
  `EVAL-BUILD`,
  `EVAL-DO`,
]

proc fileName(t: Transpiler, name: string): string =
  if t.files.hasKey(name):
    return t.files[name]
  inc t.fileCounter
  result = "f" & $t.fileCounter
  t.files[name] = result
  doAssert t.sources.hasKey(name), "no source for file: " & name
  t.defs.add("let " & result & " = " & name.escape())
  t.defs.add("src[" & result & "] = " & t.sources[name].escape())

proc locationName(t: Transpiler, loc: LocationData): string =
  doAssert loc.filename != nil, "location without filename"

  let key = (loc.filename[], loc.line, loc.col)
  if t.locations.hasKey(key):
    return t.locations[key]

  inc t.locCounter
  result = "l" & $t.locCounter

  t.locations[key] = result
  t.defs.add("let " & result & " = loc(" & t.fileName(loc.filename[]) & ", " &
             $loc.line & ", " & $loc.col & ")")

proc transpileForm(t: Transpiler, form: Form, noOpt: bool = false): string

template loc(t: Transpiler, form: Form): string =
  " ? " & t.locationName(form.locationData)

proc transpileMap(t: Transpiler, form: Form, noOpt: bool = false): string =
  var pos: seq[string] = @[]
  for f in form.posValue:
    pos.add(t.transpileForm(f, noOpt))

  if form.mapValue.len == 0:
    return "M(" & pos.join(", ") & ")" & t.loc(form)

  var pairs: seq[string] = @[]
  for k, v in form.mapValue.pairs:
    pairs.add("(" & t.transpileForm(k, noOpt) & ", " & t.transpileForm(v, noOpt) & ")")

  return "K(@[" & pos.join(", ") & "], @[" & pairs.join(", ") & "])" & t.loc(form)

proc transpileBuiltin(t: Transpiler, form: Form, macroName: string): string =
  let head = form.posValue[0]
  var args: seq[string] = @[]

  args.add(t.locationName(head.locationData))

  for i in 1 ..< form.posValue.len:
    args.add(t.transpileForm(form.posValue[i]))

  return "`" & macroName & "`(" & args.join(", ") & ")"

proc dispatchMapTranspilation(t: Transpiler, form: Form, noOpt: bool = false): string =
  if form.posValue.len == 0 and form.mapValue.len == 0:
    return "M()" & t.loc(form)

  if noOpt or form.posValue.len == 0:
    return t.transpileMap(form, noOpt)
  else:
    let head = form.posValue[0]
    if head.kind != fkSym:
      return t.transpileMap(form)

    let headIntern = head.symInterned

    if headIntern in noOptBuiltins:
      return t.transpileMap(form, true)

    if expandableBuiltins.hasKey(headIntern):
      let argc = form.posValue.len - 1
      let info = expandableBuiltins[headIntern]
      if argc >= info.min and (info.max < 0 or argc <= info.max):
        return t.transpileBuiltin(form, info.name)

    return t.transpileMap(form)

proc transpileForm(t: Transpiler, form: Form, noOpt: bool = false): string =
  result = case form.kind:
  of fkInt: "%" & $form.intValue & t.loc(form)
  of fkStr: "%" & form.strValue.escape() & t.loc(form)
  of fkSym: "^" & t.internmentData.unintern(form.symInterned).escape() & t.loc(form)
  of fkMap: t.dispatchMapTranspilation(form, noOpt)
  of fkNil: "%nil" & t.loc(form)
  of fkErr: raise newException(ValueError, "unexpected fkErr at " & $form.locationData)

proc transpile*(form: Form, internmentData: InternmentData, sources: Table[string, string]): string =
  let t = Transpiler(internmentData: internmentData, sources: sources)

  t.defs.add("var src = initTable[string, string]()")

  for f in form.posValue:
    t.body.add("  checkTop(" & t.transpileForm(f) & ")")

  result = @[
    "import ../src/sysvafly",
    "import std/tables",
    "",
    "template start =",
    t.body.join("\n"),
    "",
    "init()",
    t.defs.join("; "),
    "",
    "start()"
  ].join("\n") & "\n"