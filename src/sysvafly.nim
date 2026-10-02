import forms
import evaluator
import std/[macros, tables]

var globalCtx: Context
var globalInternmentData: InternmentData
var filenameRefs: Table[string, ref string]

template init*() =
  globalCtx = newContext()
  globalInternmentData = newInternmentData()
  filenameRefs = initTable[string, ref string]()

template eval*(form: Form): Form =
  globalCtx.eval(form)

template `^`*(s: string): Form =
  newSymForm(globalInternmentData.intern(s))

template `%`*(x: int): Form = newIntForm(x)
template `%`*(x: string): Form = newStrForm(x)
template `%`*(x: typeof(nil)): Form = newNilForm()

template M*(forms: varargs[Form]): Form =
  newMapForm(@forms)

template K*(pos: seq[Form], map: seq[(Form, Form)]): Form =
  newMapForm(pos, map)

template loc*(filenameArg: string, lineArg, colArg: Positive): LocationData =
  block:
    if not filenameRefs.hasKey(filenameArg):
      let r = new(string)
      r[] = filenameArg
      filenameRefs[filenameArg] = r
    LocationData(filename: filenameRefs[filenameArg], line: lineArg, col: colArg)

proc `?`*(form: Form, location: LocationData): Form =
  form.locationData = location
  return form

proc bad*(kind: FormKind): Form =
  doAssert false, "Bad form in transpile input: " & $kind

template checkTop*(form: Form) =
  let f = eval(form)
  if f.kind == fkErr:
    let fn = f.locationData.filename[]
    if src.hasKey(fn):
      stderr.writeLine(src[fn].errFormToStr(f, globalInternmentData))
    else:
      stderr.writeLine(f.toStr(globalInternmentData))
    quit(1)