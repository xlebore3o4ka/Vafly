import forms
import std/[tables, strutils]

type
  Transpiler = ref object
    internmentData: InternmentData
    body: seq[string]
    defs: seq[string]
    files: Table[string, string]
    locations: Table[(string, int, int), string]
    locCounter: int
    fileCounter: int

proc fileName(t: Transpiler, name: string): string =
  if t.files.hasKey(name):
    return t.files[name]
  inc t.fileCounter
  result = "f" & $t.fileCounter
  t.files[name] = result
  t.defs.add("let " & result & " = " & name.escape())

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

proc transpileForm(t: Transpiler, form: Form): string

proc transpileMap(t: Transpiler, form: Form): string =
  var pos: seq[string] = @[]
  for f in form.posValue:
    pos.add(t.transpileForm(f))

  if form.mapValue.len == 0:
    return "M(" & pos.join(", ") & ")"

  var pairs: seq[string] = @[]
  for k, v in form.mapValue.pairs:
    pairs.add("(" & t.transpileForm(k) & ", " & t.transpileForm(v) & ")")

  return "K(@[" & pos.join(", ") & "], @[" & pairs.join(", ") & "])"

proc transpileForm(t: Transpiler, form: Form): string =
  result = case form.kind:
  of fkInt: "%" & $form.intValue
  of fkStr: "%" & form.strValue.escape()
  of fkSym: "^" & t.internmentData.unintern(form.symInterned).escape()
  of fkMap: t.transpileMap(form)
  of fkNil: "%nil"
  of fkErr: raise newException(ValueError, "unexpected fkErr at " & $form.locationData)
  result &= " ? " & t.locationName(form.locationData)

proc transpile*(form: Form, internmentData: InternmentData): string =
  let t = Transpiler(internmentData: internmentData)

  var bodyLines: seq[string] = @[]
  for f in form.posValue:
    bodyLines.add("  discard eval " & t.transpileForm(f))

  result = @[
    "import ../src/sysvafly",
    "",
    "template start =",
    bodyLines.join("\n"),
    "",
    "init()",
    t.defs.join("; "),
    "",
    "start()"
  ].join("\n") & "\n"