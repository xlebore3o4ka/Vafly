import forms
import std/[tables, macros]

type
  Context* = ref object
    internmentData*: InternmentData
    env*:            Form
    stacktrace*:     Form

proc symExists(ctx: Context, intern: int): bool =
  var env = ctx.env

  while env != nil and (not env.mapIsNil) and env.mapValue.len != 0:
    if env.mapValue[newSymForm(`ENV-CURRENT`)].mapValue.hasKey(newSymForm(intern)):
      return true
    env = env.mapValue[newSymForm(`ENV-PARENT`)]

  return false

proc getSym(ctx: Context, intern: int, loc: LocationData): Form =
  var env = ctx.env

  while env != nil and (not env.mapIsNil) and env.mapValue.len != 0:
    let frame = env.mapValue[newSymForm(`ENV-CURRENT`)]
    if frame.mapValue.hasKey(newSymForm(intern)):
      return frame.mapValue[newSymForm(intern)]
    env = env.mapValue[newSymForm(`ENV-PARENT`)]

  return loc.newErrForm(newSymForm(`ERR-UNBOUND-SYMBOL`),
    "The symbol " & ctx.internmentData.unintern(intern) & " has never been bound to any value")

proc pushEnv(ctx: Context) =
  let newFrame = newMapForm(false)
  ctx.env = newMapForm(@{
    newSymForm(`ENV-CURRENT`): newFrame,
    newSymForm(`ENV-PARENT`): ctx.env
  })

proc popEnv(ctx: Context) =
  assert not ctx.env.isNil(), "popEnv: env is nil"
  assert ctx.env.mapValue.len != 0, "popEnv: env is empty"

  let parent = ctx.env.mapValue[newSymForm(`ENV-PARENT`)]
  assert not parent.isNil(), "popEnv: parent is nil"

  ctx.env = parent

proc newSym(ctx: Context, intern: int, val: Form) =
  assert not ctx.env.isNil(), "newSym: env is nil"
  assert ctx.env.mapValue.len != 0, "newSym: env is empty"

  let frame = ctx.env.mapValue[newSymForm(`ENV-CURRENT`)]
  assert not frame.isNil(), "newSym: current frame is nil"

  frame.mapValue[newSymForm(intern)] = val

proc setSym(ctx: Context, intern: int, val: Form, loc: LocationData): Form =
  var env = ctx.env

  while env != nil and (not env.mapIsNil) and env.mapValue.len != 0:
    let frame = env.mapValue[newSymForm(`ENV-CURRENT`)]

    if frame.mapValue.hasKey(newSymForm(intern)):
      frame.mapValue[newSymForm(intern)] = val
      return val

    env = env.mapValue[newSymForm(`ENV-PARENT`)]

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
    builtinBindings.mapValue[newSymForm(`name`)] = newMapForm( @{newIntForm(0): newSymForm(`evalBuiltin`)} )

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
  let b = argEvalInt(1)
  if b == 0: return arg(1).locationData.newErrForm(newSymForm(`ERR-ZERO-DIVISION`))
  return newIntForm(argEvalInt(0) div b)

builtin `EVAL-MOD`:
  expect `==`, 2
  let b = argEvalInt(1)
  if b == 0: return arg(1).locationData.newErrForm(newSymForm(`ERR-ZERO-DIVISION`))
  return newIntForm(argEvalInt(0) mod b)

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
  for i in 0 ..< args.len - 1:
    last = argEval(i).returnIfErr()
    if not last.toBool():
      return newMapForm(true)
  return last

builtin `EVAL-ANY`:
  expect `>=`, 1
  for i in 0 ..< args.len - 1:
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

  if map.mapValue.hasKey(key):
    return map.mapValue[key]

  if has(2):
    return argEval(2).returnIfErr()

  return key.locationData.newErrForm(newSymForm(`ERR-KEY-ERROR`),
    "Key " & key.toStr(ctx.internmentData) & " not found")

builtin `EVAL-LOCAL`:
  expect `==`, 2
  let intern = arg(0).expect(fkSym).returnIfErr().symInterned
  let value  = argEval(1).returnIfErr()
  ctx.newSym(intern, value)
  return value

builtin `EVAL-SET`:
  expect `==`, 2
  let symForm = arg(0).returnIfErr().expect(fkSym).returnIfErr()
  let value   = argEval(1).returnIfErr()
  return ctx.setSym(symForm.symInterned, value, symForm.locationData)

builtin `EVAL-IF`:
  expect `>=`, 2
  expect `<=`, 3

  if argEval(0).returnIfErr().toBool():
    return argEval(1).returnIfErr()

  if has(2):
    return argEval(2).returnIfErr()

  return newMapForm(true)

builtin `EVAL-COND`:
  expect `>=`, 1
  for i in 0 ..< args.len - 1:
    let rawPair = arg(i).returnIfErr()
    let pair = rawPair.expect(fkMap).returnIfErr()
    if pair.mapValue.len != 2:
      return rawPair.locationData.newErrForm(newSymForm(`ERR-ARGS-MISMATCH`),
        "COND: expected pair of 2, got " & $pair.mapValue.len)
    let condForm = pair.mapValue.at(0)
    let bodyForm = pair.mapValue.at(1)
    if ctx.eval(condForm).returnIfErr().toBool():
      return ctx.eval(bodyForm)
  return newMapForm(true)

builtin `EVAL-LET`:
  expect `>=`, 1

  let bindingsForm = arg(0).returnIfErr().expect(fkMap).returnIfErr()

  ctx.pushEnv()

  try:
    for i in 0 ..< bindingsForm.mapValue.len:
      let pair = bindingsForm.mapValue.at(i).expect(fkMap).returnIfErr()
      if pair.mapValue.len != 2:
        return pair.locationData.newErrForm(newSymForm(`ERR-ARGS-MISMATCH`),
          "LET: expected binding pair of 2, got " & $pair.mapValue.len)
      let symForm = pair.mapValue.at(0).expect(fkSym).returnIfErr()
      let value   = ctx.eval(pair.mapValue.at(1)).returnIfErr()
      ctx.newSym(symForm.symInterned, value)

    var res = newMapForm(true)
    for i in 2 ..< args.len:
      res = ctx.eval(args.at(i)).returnIfErr()
    return res
  finally:
    ctx.popEnv()

builtin `EVAL-LAMBDA`:
  expect `>=`, 1

  let paramsForm = arg(0).returnIfErr().expect(fkMap).returnIfErr()
  var params = newMapForm(false)
  for i in 0 ..< paramsForm.mapValue.len:
    let sym = paramsForm.mapValue.at(i).expect(fkSym).returnIfErr()
    params.mapValue[newSymForm(sym.symInterned)] = newSymForm(`EVAL-PARAM`)

  var body = newMapForm(false)
  for i in 2 ..< args.len:
    body.mapValue.append(args.at(i))

  return newMapForm(@{
    newIntForm(0): newSymForm(`EVAL-T-LAMBDA`),
    newIntForm(1): params,
    newIntForm(2): ctx.env,
    newIntForm(3): body
  })

proc newContext*(internmentData: InternmentData = newInternmentData()): Context =
  var env = newMapForm(false)
  for k, v in builtinBindings.mapValue.pairs:
    env.mapValue[k] = v

  Context(
    internmentData: internmentData,
    env: newMapForm(@{
      newSymForm(`ENV-CURRENT`): env,
      newSymForm(`ENV-PARENT`):  newMapForm()
    }),
    stacktrace: newMapForm(false)
  )

proc funcall(ctx: Context, closure: Form, callForm: Form): Form =
  let params = closure.mapValue.at(1)
  let env    = closure.mapValue.at(2)
  let body   = closure.mapValue.at(3)

  let argc = callForm.mapValue.len - 1
  if argc != params.mapValue.len:
    return callForm.locationData.newErrForm(newSymForm(`ERR-ARGS-MISMATCH`),
      "Arguments mismatch: expected " & $params.mapValue.len & ", got " & $argc)

  var argValues = newSeq[Form](argc)
  for i in 0 ..< argc:
    argValues[i] = ctx.eval(callForm.mapValue.at(i + 1)).returnIfErr()

  let oldEnv = ctx.env
  ctx.env = env
  ctx.pushEnv()

  try:
    var pi = 0
    for intern, _ in params.mapValue.pairs:
      ctx.newSym(intern.symInterned, argValues[pi])
      inc pi

    var res = newMapForm(true)
    for i in 0 ..< body.mapValue.len:
      res = ctx.eval(body.mapValue.at(i)).returnIfErr()
    return res
  finally:
    ctx.popEnv()
    ctx.env = oldEnv

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

  let callForm = ctx.eval(head).returnIfErr()

  if callForm.kind != fkMap or callForm.mapIsNil or not callForm.mapValue.hasKey(headKey) or
      callForm.mapValue[headKey].kind != fkSym:
    return head.locationData.newErrForm(newSymForm(`ERR-CANNOT-CALL`), "Cannot call " &
      (if callForm.kind != fkMap or callForm.mapIsNil: $head.kind
       else: $callForm.mapValue[headKey].kind))

  let ty = callForm.mapValue[headKey].symInterned
  let caused = form.mapValue[headKey]

  if ty == `EVAL-BUILTIN`:
    result = builtinDispatcher[caused.symInterned](ctx, form.mapValue)
    if result.kind == fkErr:
      ctx.stacktrace.mapValue.append(caused)

  elif ty == `EVAL-T-LAMBDA`:
    result = funcall(ctx, callForm, form)
    if result.kind == fkErr:
      ctx.stacktrace.mapValue.append(caused.locationData.newSymForm(`EVAL-T-LAMBDA`))