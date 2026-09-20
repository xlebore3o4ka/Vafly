import forms
import std/[tables, macros]

type
  Context* = ref object
    internmentData*: InternmentData
    env*:            Form

proc symExists(ctx: Context, intern: int): bool =
  var env = ctx.env

  while env != nil and (not env.mapIsNil) and env.mapValue.len != 0:
    if env.mapValue.at(`ENV-CURRENT`).mapValue.hasKey(newIntForm(intern)):
      return true
    env = env.mapValue.at(`ENV-PARENT`)

  return false

proc getSym(ctx: Context, intern: int, loc: LocationData): Form =
  var env = ctx.env

  while env != nil and (not env.mapIsNil) and env.mapValue.len != 0:
    let frame = env.mapValue.at(`ENV-CURRENT`)
    if frame.mapValue.hasKey(newIntForm(intern)):
      return frame.mapValue[newIntForm(intern)]
    env = env.mapValue.at(`ENV-PARENT`)

  return loc.newErrForm(newSymForm(`ERR-UNBOUND-SYMBOL`),
    "The symbol " & ctx.internmentData.unintern(intern) & " has never been bound to any value")

proc pushEnv(ctx: Context): Form =
  let newFrame = newMapForm(false)
  ctx.env = newMapForm(@{
    newIntForm(`ENV-CURRENT`): newFrame,
    newIntForm(`ENV-PARENT`): ctx.env
  })
  result = ctx.env

proc popEnv(ctx: Context): Form =
  assert not ctx.env.isNil(), "popEnv: env is nil"
  assert ctx.env.mapValue.len != 0, "popEnv: env is empty"

  let parent = ctx.env.mapValue[newIntForm(`ENV-PARENT`)]
  assert not parent.isNil(), "popEnv: parent is nil"

  ctx.env = parent
  result = ctx.env

proc newSym(ctx: Context, intern: int, val: Form) =
  assert not ctx.env.isNil(), "newSym: env is nil"
  assert ctx.env.mapValue.len != 0, "newSym: env is empty"

  let frame = ctx.env.mapValue[newIntForm(`ENV-CURRENT`)]
  assert not frame.isNil(), "newSym: current frame is nil"

  frame.mapValue[newIntForm(intern)] = val

proc setSym(ctx: Context, intern: int, val: Form, loc: LocationData): Form =
  var env = ctx.env

  while (not env.isNil()) and env.mapValue.len != 0:
    let frame = env.mapValue[newIntForm(`ENV-CURRENT`)]

    if frame.mapValue.hasKey(newIntForm(intern)):
      frame.mapValue[newIntForm(intern)] = val
      return val

    env = env.mapValue[newIntForm(`ENV-PARENT`)]

  result = loc.newErrForm(newSymForm(`ERR-UNBOUND-SYMBOL`),
    "The symbol " & ctx.internmentData.unintern(intern) & " has never been bound to any value")

proc eval*(ctx: Context, form: Form): Form 

var builtinDispatcher = initTable[int, proc (ctx: Context, args: OrderedTable[Form, Form]): Form]()
var builtinBindings = newMapForm(false)

macro builtin(name: int, body: untyped): untyped =
  result = newStmtList()

  let evalBuiltin = ident("EVAL-BUILTIN")

  result.add quote do:
    builtinDispatcher[`name`] = proc (ctx {.inject.}: Context, args {.inject.}: OrderedTable[Form, Form]): Form =
      `body`
    builtinBindings.mapValue[newIntForm(`name`)] = newMapForm( @{newIntForm(0): newSymForm(`evalBuiltin`)} )

template arg(idx: int): Form =
  args.atOrErr(idx + 1, loc = args.at(0).locationData)

template argEval(idx: int): Form =
  ctx.eval(args.atOrErr(idx + 1, loc = args.at(0).locationData))

template expect(form: Form, ekind: FormKind): Form =
  if unlikely(form.kind != ekind):
    form.locationData.newErrForm(newSymForm(`ERR-TYPE-MISMATCH`),
      "Expected " & $ekind & ", got " & $form.kind)
  else: form

macro expect(opSym: untyped, rv: static[int]): untyped =
  let errArgsMismatch = ident("ERR-ARGS-MISMATCH")

  let cond = nnkInfix.newTree(opSym, nnkCall.newTree(ident"len", ident"args"), newLit(rv + 1))

  result = quote do:
    if not `cond`:
      return args.at(0).locationData.newErrForm(newSymForm(`errArgsMismatch`),
        "Arguments mismatch: expected len " & astToStr(`opSym`) & " " & $`rv` &
        ", got " & $(args.len - 1))

macro returnIfErr(form: untyped): untyped =
  let tmp = genSym(nskLet, "form")
  result = quote do:
    block:
      let `tmp` = `form`
      if `tmp`.kind == fkErr: return `tmp`
      `tmp`

template argEvalInt(idx: int): int =
  argEval(idx).returnIfErr().expect(fkInt).returnIfErr().intValue

template has(idx: static[int]): bool =
  args.hasKey(newIntForm(idx + 1))

builtin `EVAL+`:
  expect `==`, 2
  return newIntForm(argEvalInt(0) + argEvalInt(1))

builtin `EVAL-`:
  expect `==`, 2
  return newIntForm(argEvalInt(0) - argEvalInt(1))

builtin `EVAL*`:
  expect `==`, 2
  return newIntForm(argEvalInt(0) * argEvalInt(1))

builtin `EVAL-DIV`:
  expect `==`, 2
  return newIntForm(argEvalInt(0) div argEvalInt(1))

builtin `EVAL-MOD`:
  expect `==`, 2
  return newIntForm(argEvalInt(0) mod argEvalInt(1))

builtin `EVAL-EQ`:
  expect `==`, 2
  if argEval(0).returnIfErr() == argEval(1).returnIfErr():
    return newIntForm(1)
  else:
    return newMapForm(true)

builtin `EVAL-NEQ`:
  expect `==`, 2
  if argEval(0).returnIfErr() == argEval(1).returnIfErr():
    return newMapForm(true)
  else:
    return newIntForm(1)

builtin `EVAL>`:
  expect `==`, 2
  if argEvalInt(0) > argEvalInt(1):
    return newIntForm(1)
  else:
    return newMapForm(true)

builtin `EVAL<`:
  expect `==`, 2
  if argEvalInt(0) < argEvalInt(1):
    return newIntForm(1)
  else:
    return newMapForm(true)

builtin `EVAL>=`:
  expect `==`, 2
  if argEvalInt(0) >= argEvalInt(1):
    return newIntForm(1)
  else:
    return newMapForm(true)

builtin `EVAL<=`:
  expect `==`, 2
  if argEvalInt(0) <= argEvalInt(1):
    return newIntForm(1)
  else:
    return newMapForm(true)

builtin `EVAL-ALL`:
  expect `>=`, 1
  var last = newIntForm(1)
  for i in 0 ..< args.len:
    last = argEval(i).returnIfErr()
    if not last.toBool():
      return newMapForm(true)
  return last

builtin `EVAL-ANY`:
  expect `>=`, 1
  for i in 0 ..< args.len:
    let val = argEval(i).returnIfErr()
    if val.toBool():
      return val
  return newMapForm(true)

builtin `EVAL-AND`:
  expect `==`, 2
  let a = argEval(0).returnIfErr()
  if not a.toBool():
    return newMapForm(true)
  return argEval(1).returnIfErr()

builtin `EVAL-OR`:
  expect `==`, 2
  let a = argEval(0).returnIfErr()
  if a.toBool():
    return a
  return argEval(1).returnIfErr()

builtin `PARSER-QUOTE`:
  expect `==`, 1
  return arg(0)

builtin `EVAL-GET`:
  expect `>=`, 2
  expect `<=`, 3
  let map = argEval(0).returnIfErr().expect(fkMap).returnIfErr()
  let key = argEval(1).returnIfErr()
  let default = if has(2): argEval(2).returnIfErr()
    else: args.at(0).locationData.newErrForm(newSymForm(`ERR-KEY-ERROR`),
            "Key " & key.toStr(ctx.internmentData) & " not found")
  return map.mapValue.getOrDefault(key, default)

builtin `EVAL-LOCAL`:
  expect `==`, 2
  let intern = arg(0).expect(fkSym).returnIfErr().symInterned
  let value  = argEval(1).returnIfErr()
  ctx.newSym(intern, value)
  return value

proc newContext*(internmentData: InternmentData = newInternmentData()): Context =
  var env = newMapForm(false)
  for k, v in builtinBindings.mapValue.pairs:
    env.mapValue[k] = v

  Context(
    internmentData: internmentData,
    env: newMapForm(@{
      newIntForm(`ENV-CURRENT`): env,
      newIntForm(`ENV-PARENT`):  newMapForm()
    })
  )

proc eval*(ctx: Context, form: Form): Form =
  if form.kind == fkSym:
    if form.symInterned == `PARSER-KEYWORD`: return form
    return ctx.getSym(form.symInterned, form.locationData)

  elif form.kind in {fkInt, fkStr, fkErr} or form.mapIsNil or form.mapValue.len == 0:
    return form

  let headKey = newIntForm(0)
  if not form.mapValue.hasKey(headKey):
    return form.locationData.newErrForm(newSymForm(`ERR-CANNOT-CALL`),
      "Cannot call " & form.toStr(ctx.internmentData))

  let head = form.mapValue[headKey]
  if head.kind == fkSym and head.symInterned == `PARSER-KEYWORD`:
    return form

  let callForm = ctx.eval(form.mapValue[headKey]).returnIfErr()

  if callForm.kind != fkMap or callForm.mapIsNil or not callForm.mapValue.hasKey(headKey) or
      callForm.mapValue[headKey].kind != fkSym:
    return form.locationData.newErrForm(newSymForm(`ERR-CANNOT-CALL`), "Cannot call " &
      (if callForm.kind != fkMap or callForm.mapIsNil: $form.mapValue[headKey].kind
       else: $callForm.mapValue[headKey].kind))

  let ty = callForm.mapValue[headKey].symInterned

  if ty == `EVAL-BUILTIN`:
    return builtinDispatcher[form.mapValue[headKey].symInterned](ctx, form.mapValue)