import forms
import std/[tables, macros]

type
  Context* = ref object
    internmentData*: InternmentData
    env*:            Form

proc symExists(ctx: Context, intern: int): bool =
  var env = ctx.env

  while env.kind == fkMap:
    let frame = env.mapValue[newSymForm(`ENV-CURRENT`)]
    if frame.mapValue.hasKey(newSymForm(intern)):
      return true
    env = env.mapValue[newSymForm(`ENV-PARENT`)]

  return false

proc getSym(ctx: Context, intern: int, loc: LocationData): Form =
  var env = ctx.env

  while env.kind == fkMap:
    let frame = env.mapValue[newSymForm(`ENV-CURRENT`)]
    if frame.mapValue.hasKey(newSymForm(intern)):
      return frame.mapValue[newSymForm(intern)]
    env = env.mapValue[newSymForm(`ENV-PARENT`)]

  return loc.newErrForm(newSymForm(`ERR-UNBOUND-SYMBOL`),
    "The symbol " & ctx.internmentData.unintern(intern) & " has never been bound to any value")

proc pushEnv(ctx: Context) =
  let newFrame = newMapForm()
  ctx.env = newMapForm(@{
    newSymForm(`ENV-CURRENT`): newFrame,
    newSymForm(`ENV-PARENT`): ctx.env
  })

proc popEnv(ctx: Context) =
  assert ctx.env.kind == fkMap, "popEnv: env is not a map"
  ctx.env = ctx.env.mapValue[newSymForm(`ENV-PARENT`)]

proc newSym(ctx: Context, intern: int, val: Form) =
  assert ctx.env.kind == fkMap, "newSym: env is not a map"
  let frame = ctx.env.mapValue[newSymForm(`ENV-CURRENT`)]
  frame.mapValue[newSymForm(intern)] = val

proc setSym(ctx: Context, intern: int, val: Form, loc: LocationData): Form =
  var env = ctx.env

  while env.kind == fkMap:
    let frame = env.mapValue[newSymForm(`ENV-CURRENT`)]
    if frame.mapValue.hasKey(newSymForm(intern)):
      frame.mapValue[newSymForm(intern)] = val
      return val
    env = env.mapValue[newSymForm(`ENV-PARENT`)]

  result = loc.newErrForm(newSymForm(`ERR-UNBOUND-SYMBOL`),
    "The symbol " & ctx.internmentData.unintern(intern) & " has never been bound to any value")

proc eval*(ctx: Context, form: Form): Form 

var builtinDispatcher = initTable[int, proc (ctx: Context, args: Form): Form]()
var builtinBindings = newMapForm()

macro builtin(name: int, body: untyped): untyped =
  result = newStmtList()

  let evalBuiltin = ident("EVAL-BUILTIN")

  result.add quote do:
    builtinDispatcher[`name`] = proc (ctx {.inject.}: Context, args {.inject.}: Form): Form =
      `body`
    builtinBindings.mapValue[newSymForm(`name`)] = newMapForm(@[newSymForm(`evalBuiltin`)])

template arg(idx: int): Form =
  if not args.hasKey(idx + 1):
    args.get(0).locationData.newErrForm(newSymForm(`ERR-ARGS-MISMATCH`),
      "Missing argument " & $idx)
  else:
    args.get(idx + 1)

template argEval(idx: int): Form =
  ctx.eval(arg(idx))

template expect(form: Form, ekind: FormKind): Form =
  if unlikely(form.kind != ekind):
    form.locationData.newErrForm(newSymForm(`ERR-TYPE-MISMATCH`),
      "Expected " & $ekind & ", got " & $form.kind)
  else: form

macro expect(opSym: untyped, rv: static[int]): untyped =
  let errArgsMismatch = ident("ERR-ARGS-MISMATCH")

  let cond = nnkInfix.newTree(opSym,
    nnkCall.newTree(ident"len", nnkDotExpr.newTree(ident"args", ident"posValue")),
    newLit(rv + 1))

  result = quote do:
    if not `cond`:
      return args.get(0).locationData.newErrForm(newSymForm(`errArgsMismatch`),
        "Arguments mismatch: expected len " & astToStr(`opSym`) & " " & $`rv` &
        ", got " & $(args.posValue.len - 1))

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
  args.hasKey(idx + 1)

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
    return newNilForm()

builtin `EVAL-NEQ`:
  expect `==`, 2
  if argEval(0).returnIfErr() == argEval(1).returnIfErr():
    return newNilForm()
  else:
    return newIntForm(1)

builtin `EVAL>`:
  expect `==`, 2
  if argEvalInt(0) > argEvalInt(1):
    return newIntForm(1)
  else:
    return newNilForm()

builtin `EVAL<`:
  expect `==`, 2
  if argEvalInt(0) < argEvalInt(1):
    return newIntForm(1)
  else:
    return newNilForm()

builtin `EVAL>=`:
  expect `==`, 2
  if argEvalInt(0) >= argEvalInt(1):
    return newIntForm(1)
  else:
    return newNilForm()

builtin `EVAL<=`:
  expect `==`, 2
  if argEvalInt(0) <= argEvalInt(1):
    return newIntForm(1)
  else:
    return newNilForm()

builtin `EVAL-NEG`:
  expect `==`, 1
  return newIntForm(-argEvalInt(0))

builtin `EVAL-ALL`:
  expect `>=`, 1
  var last = newIntForm(1)
  for i in 0 ..< args.posValue.len - 1:
    last = argEval(i).returnIfErr()
    if not last.toBool():
      return newNilForm()
  return last

builtin `EVAL-ANY`:
  expect `>=`, 1
  for i in 0 ..< args.posValue.len - 1:
    let val = argEval(i).returnIfErr()
    if val.toBool():
      return val
  return newNilForm()

builtin `EVAL-AND`:
  expect `==`, 2
  let a = argEval(0).returnIfErr()
  if not a.toBool():
    return newNilForm()
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

  if map.hasKey(key):
    return map.get(key)

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

  return newNilForm()

builtin `EVAL-COND`:
  expect `>=`, 1
  for i in 0 ..< args.posValue.len - 1:
    let rawPair = arg(i).returnIfErr()
    let pair = rawPair.expect(fkMap).returnIfErr()
    if pair.posValue.len != 2:
      return rawPair.locationData.newErrForm(newSymForm(`ERR-ARGS-MISMATCH`),
        "COND: expected pair of 2, got " & $pair.posValue.len)
    let condForm = pair.get(0)
    let bodyForm = pair.get(1)
    if ctx.eval(condForm).returnIfErr().toBool():
      return ctx.eval(bodyForm)
  return newNilForm()

builtin `EVAL-LET`:
  expect `>=`, 1

  let bindingsForm = arg(0).returnIfErr().expect(fkMap).returnIfErr()

  ctx.pushEnv()

  try:
    for i in 0 ..< bindingsForm.posValue.len:
      let pair = bindingsForm.get(i).expect(fkMap).returnIfErr()
      if pair.posValue.len != 2:
        return pair.locationData.newErrForm(newSymForm(`ERR-ARGS-MISMATCH`),
          "LET: expected binding pair of 2, got " & $pair.posValue.len)
      let symForm = pair.get(0).expect(fkSym).returnIfErr()
      let value   = ctx.eval(pair.get(1)).returnIfErr()
      ctx.newSym(symForm.symInterned, value)

    var res = newNilForm()
    for i in 2 ..< args.posValue.len:
      res = ctx.eval(args.get(i)).returnIfErr()
    return res
  finally:
    ctx.popEnv()

builtin `EVAL-LAMBDA`:
  expect `>=`, 1

  let paramsForm = arg(0).returnIfErr().expect(fkMap).returnIfErr()
  var params = newMapForm()
  for i in 0 ..< paramsForm.posValue.len:
    let sym = paramsForm.get(i).expect(fkSym).returnIfErr()
    let key = newSymForm(sym.symInterned)
    if params.mapValue.hasKey(key):
      return sym.locationData.newErrForm(newSymForm(`ERR-DUPLICATE-PARAM`),
        "Duplicate parameter: " & ctx.internmentData.unintern(sym.symInterned))
    params.mapValue[key] = newSymForm(`EVAL-PARAM`)

  var body = newMapForm()
  for i in 2 ..< args.posValue.len:
    body.append(args.get(i))

  return newMapForm(@[
    newSymForm(`EVAL-T-LAMBDA`),
    params,
    ctx.env,
    body
  ])

builtin `EVAL-TRY`:
  expect `>=`, 2
  expect `<=`, 3

  let bodyForm    = arg(0)
  let handlerForm = arg(1)
  let hasFinally  = has(2)
  let finallyForm = if hasFinally: arg(2) else: newNilForm()

  var res: Form

  try:
    let bodyRes = ctx.eval(bodyForm)
    if bodyRes.kind == fkErr:
      let callForm = newMapForm(@[
        handlerForm,
        newMapForm(@[newSymForm(`PARSER-QUOTE`), bodyRes.errSym]),
        newStrForm(bodyRes.errMsg),
        newMapForm(@[newSymForm(`PARSER-QUOTE`), bodyRes.errTrace])
      ])
      res = ctx.eval(callForm)
    else:
      res = bodyRes
  finally:
    if hasFinally:
      let finRes = ctx.eval(finallyForm)
      if finRes.kind == fkErr:
        res = finRes

  return res

proc newContext*(internmentData: InternmentData = newInternmentData()): Context =
  var env = newMapForm()
  for k, v in builtinBindings.mapValue.pairs:
    env.mapValue[k] = v

  Context(
    internmentData: internmentData,
    env: newMapForm(@{
      newSymForm(`ENV-CURRENT`): env,
      newSymForm(`ENV-PARENT`):  newNilForm()
    })
  )

proc funcall(ctx: Context, closure: Form, callForm: Form): Form =
  let params = closure.get(1)
  let env    = closure.get(2)
  let body   = closure.get(3)

  let argc = callForm.posValue.len - 1
  if argc != params.mapValue.len:
    return callForm.locationData.newErrForm(newSymForm(`ERR-ARGS-MISMATCH`),
      "Arguments mismatch: expected " & $params.mapValue.len & ", got " & $argc)

  var argValues = newSeq[Form](argc)
  for i in 0 ..< argc:
    argValues[i] = ctx.eval(callForm.get(i + 1)).returnIfErr()

  let oldEnv = ctx.env
  ctx.env = env
  ctx.pushEnv()

  try:
    var pi = 0
    for paramSym, _ in params.mapValue.pairs:
      ctx.newSym(paramSym.symInterned, argValues[pi])
      inc pi

    var res = newNilForm()
    for i in 0 ..< body.posValue.len:
      res = ctx.eval(body.get(i)).returnIfErr()
    return res
  finally:
    ctx.popEnv()
    ctx.env = oldEnv

proc eval*(ctx: Context, form: Form): Form =
  if form.kind == fkSym:
    if form.symInterned == `PARSER-KEYWORD`: return form
    return ctx.getSym(form.symInterned, form.locationData)

  if form.kind in {fkNil, fkInt, fkStr, fkErr}:
    return form

  # form.kind == fkMap
  if form.posValue.len == 0:
    return form

  let head = form.get(0)
  if head.kind == fkSym and head.symInterned == `PARSER-KEYWORD`:
    return form

  let callForm = ctx.eval(head).returnIfErr()

  if callForm.kind != fkMap or callForm.posValue.len == 0:
    return head.locationData.newErrForm(newSymForm(`ERR-CANNOT-CALL`),
      "Cannot call " & $head.kind)

  let callHead = callForm.get(0)
  if callHead.kind != fkSym:
    return head.locationData.newErrForm(newSymForm(`ERR-CANNOT-CALL`),
      "Cannot call " & $callHead.kind)

  let ty = callHead.symInterned

  if ty == `EVAL-BUILTIN`:
    result = builtinDispatcher[head.symInterned](ctx, form)
    if result.kind == fkErr:
      result.errTrace.append(head)

  elif ty == `EVAL-T-LAMBDA`:
    result = funcall(ctx, callForm, form)
    if result.kind == fkErr:
      result.errTrace.append(head.locationData.newSymForm(`EVAL-T-LAMBDA`))

  else:
    return head.locationData.newErrForm(newSymForm(`ERR-CANNOT-CALL`),
      "Unknown callable: " & ctx.internmentData.unintern(ty))