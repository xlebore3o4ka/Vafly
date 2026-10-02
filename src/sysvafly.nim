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

template typeMismatch(loc: LocationData, expected: string, got: FormKind): Form =
  loc.newErrForm(newSymForm(`ERR-TYPE-MISMATCH`), "Expected " & expected & ", got " & $got)

template evalInt(form: Form): int =
  if form.kind == fkInt:
    form.intValue
  else:
    let f = eval(form)
    if f.kind == fkErr: return f
    if f.kind != fkInt: return typeMismatch(form.locationData, "int", f.kind)
    f.intValue

macro `OPT+`*(lHead: LocationData, forms: varargs[untyped]): untyped =
  var expr: NimNode
  if forms.len == 0:
    expr = newCall(bindSym"newIntForm", newLit(0))
  else:
    expr = newCall(bindSym"evalInt", forms[0])
    for i in 1 ..< forms.len:
      expr = nnkInfix.newTree(ident"+", expr, newCall(bindSym"evalInt", forms[i]))
    expr = newCall(bindSym"newIntForm", expr)

  result = quote do:
    (block:
      let r = (proc (): Form {.nimcall.} =
        `expr`
      )()
      if r.kind == fkErr:
        r.errTrace.append(^"+" ? `lHead`)
      r
    )

macro `OPT-`*(lHead: LocationData, forms: varargs[untyped]): untyped =
  var expr: NimNode
  if forms.len == 1:
    expr = newCall(bindSym"newIntForm",
      nnkPrefix.newTree(ident"-", newCall(bindSym"evalInt", forms[0])))
  else:
    expr = newCall(bindSym"evalInt", forms[0])
    for i in 1 ..< forms.len:
      expr = nnkInfix.newTree(ident"-", expr, newCall(bindSym"evalInt", forms[i]))
    expr = newCall(bindSym"newIntForm", expr)

  result = quote do:
    (block:
      let r = (proc (): Form {.nimcall.} =
        `expr`
      )()
      if r.kind == fkErr:
        r.errTrace.append(^"-" ? `lHead`)
      r
    )

macro `OPT*`*(lHead: LocationData, forms: varargs[untyped]): untyped =
  var expr: NimNode
  if forms.len == 0:
    expr = newCall(bindSym"newIntForm", newLit(1))
  else:
    expr = newCall(bindSym"evalInt", forms[0])
    for i in 1 ..< forms.len:
      expr = nnkInfix.newTree(ident"*", expr, newCall(bindSym"evalInt", forms[i]))
    expr = newCall(bindSym"newIntForm", expr)

  result = quote do:
    (block:
      let r = (proc (): Form {.nimcall.} =
        `expr`
      )()
      if r.kind == fkErr:
        r.errTrace.append(^"*" ? `lHead`)
      r
    )

template evalIntNonZero(form: Form): int =
  if form.kind == fkInt:
    if form.intValue == 0:
      return form.locationData.newErrForm(^"ERR-ZERO-DIVISION!", "")
    form.intValue
  else:
    let f = eval(form)
    if f.kind == fkErr: return f
    if f.kind != fkInt: return typeMismatch(form.locationData, "int", f.kind)
    if f.intValue == 0:
      return form.locationData.newErrForm(^"ERR-ZERO-DIVISION!", "")
    f.intValue

macro `OPT-DIV`*(lHead: LocationData, forms: varargs[untyped]): untyped =
  var expr: NimNode
  if forms.len == 1:
    expr = newCall(bindSym"evalInt", forms[0])
  else:
    expr = newCall(bindSym"evalInt", forms[0])
    for i in 1 ..< forms.len:
      expr = nnkInfix.newTree(ident"div", expr,
        newCall(bindSym"evalIntNonZero", forms[i]))
  expr = newCall(bindSym"newIntForm", expr)

  result = quote do:
    (block:
      let r = (proc (): Form {.nimcall.} =
        `expr`
      )()
      if r.kind == fkErr:
        r.errTrace.append(^"DIV" ? `lHead`)
      r
    )

macro `OPT-MOD`*(lHead: LocationData, forms: varargs[untyped]): untyped =
  var expr: NimNode
  if forms.len == 1:
    expr = newCall(bindSym"evalInt", forms[0])
  else:
    expr = newCall(bindSym"evalInt", forms[0])
    for i in 1 ..< forms.len:
      expr = nnkInfix.newTree(ident"mod", expr,
        newCall(bindSym"evalIntNonZero", forms[i]))
  expr = newCall(bindSym"newIntForm", expr)

  result = quote do:
    (block:
      let r = (proc (): Form {.nimcall.} =
        `expr`
      )()
      if r.kind == fkErr:
        r.errTrace.append(^"MOD" ? `lHead`)
      r
    )