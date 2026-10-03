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
      let headLoc {.inject, used.} = args.get(0).locationData
      `body`
    builtinBindings.mapValue[newSymForm(`name`)] = newMapForm(@[newSymForm(`evalBuiltin`)])

template arg(idx: int): Form =
  if not args.hasKey(idx + 1):
    args.get(0).locationData.newErrForm(headLoc.newSymForm(`ERR-ARGS-MISMATCH`),
      "Missing argument " & $idx)
  else:
    args.get(idx + 1)

template argEval(idx: int): Form =
  ctx.eval(arg(idx))

template expect(form: Form, ekind: FormKind): Form =
  if unlikely(form.kind != ekind):
    form.locationData.newErrForm(headLoc.newSymForm(`ERR-TYPE-MISMATCH`),
      "Expected " & $ekind & ", got " & $form.kind)
  else: form

macro expect(opSym: untyped, rv: static[int]): untyped =
  let errArgsMismatch = ident("ERR-ARGS-MISMATCH")

  let cond = nnkInfix.newTree(opSym,
    nnkCall.newTree(ident"len", nnkDotExpr.newTree(ident"args", ident"posValue")),
    newLit(rv + 1))

  result = quote do:
    if not `cond`:
      return args.get(0).locationData.newErrForm(headLoc.newSymForm(`errArgsMismatch`),
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
  expect `>=`, 0
  var res = 0
  for form in args.posValue[1 .. ^1]:
    res += ctx.eval(form).returnIfErr().expect(fkInt).returnIfErr().intValue
  return headLoc.newIntForm(res)

builtin `EVAL-`:
  expect `>=`, 1
  var res = ctx.eval(args.posValue[1]).returnIfErr().expect(fkInt).returnIfErr().intValue
  if args.posValue.len == 2:
    return headLoc.newIntForm(-res)
  for form in args.posValue[2 .. ^1]:
    res -= ctx.eval(form).returnIfErr().expect(fkInt).returnIfErr().intValue
  return headLoc.newIntForm(res)

builtin `EVAL*`:
  expect `>=`, 0
  var res = 1
  for form in args.posValue[1 .. ^1]:
    res *= ctx.eval(form).returnIfErr().expect(fkInt).returnIfErr().intValue
  return headLoc.newIntForm(res)

builtin `EVAL-DIV`:
  expect `>=`, 1
  var res = ctx.eval(args.posValue[1]).returnIfErr().expect(fkInt).returnIfErr().intValue
  for i in 2 ..< args.posValue.len:
    let b = ctx.eval(args.posValue[i]).returnIfErr().expect(fkInt).returnIfErr().intValue
    if b == 0:
      return args.posValue[i].locationData.newErrForm(headLoc.newSymForm(`ERR-ZERO-DIVISION`))
    res = res div b
  return headLoc.newIntForm(res)

builtin `EVAL-MOD`:
  expect `>=`, 1
  var res = ctx.eval(args.posValue[1]).returnIfErr().expect(fkInt).returnIfErr().intValue
  for i in 2 ..< args.posValue.len:
    let b = ctx.eval(args.posValue[i]).returnIfErr().expect(fkInt).returnIfErr().intValue
    if b == 0:
      return args.posValue[i].locationData.newErrForm(headLoc.newSymForm(`ERR-ZERO-DIVISION`))
    res = res mod b
  return headLoc.newIntForm(res)

builtin `EVAL-EQ`:
  expect `==`, 2
  if argEval(0).returnIfErr() == argEval(1).returnIfErr():
    return headLoc.newIntForm(1)
  else:
    return headLoc.newNilForm()

builtin `EVAL-NEQ`:
  expect `==`, 2
  if argEval(0).returnIfErr() == argEval(1).returnIfErr():
    return headLoc.newNilForm()
  else:
    return headLoc.newIntForm(1)

builtin `EVAL>`:
  expect `==`, 2
  if argEvalInt(0) > argEvalInt(1):
    return headLoc.newIntForm(1)
  else:
    return headLoc.newNilForm()

builtin `EVAL<`:
  expect `==`, 2
  if argEvalInt(0) < argEvalInt(1):
    return headLoc.newIntForm(1)
  else:
    return headLoc.newNilForm()

builtin `EVAL>=`:
  expect `==`, 2
  if argEvalInt(0) >= argEvalInt(1):
    return headLoc.newIntForm(1)
  else:
    return headLoc.newNilForm()

builtin `EVAL<=`:
  expect `==`, 2
  if argEvalInt(0) <= argEvalInt(1):
    return headLoc.newIntForm(1)
  else:
    return headLoc.newNilForm()

builtin `EVAL-ALL`:
  expect `>=`, 1
  var last = headLoc.newIntForm(1)
  for form in args.posValue[1..^1]:
    last = ctx.eval(form).returnIfErr()
    if not last.toBool():
      return headLoc.newNilForm()
  return last

builtin `EVAL-ANY`:
  expect `>=`, 1
  for form in args.posValue[1..^1]:
    let val = ctx.eval(form).returnIfErr()
    if val.toBool():
      return val
  return headLoc.newNilForm()

builtin `EVAL-AND`:
  expect `==`, 2
  let a = argEval(0).returnIfErr()
  if not a.toBool():
    return headLoc.newNilForm()
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

  return key.locationData.newErrForm(headLoc.newSymForm(`ERR-KEY-ERROR`),
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

  return headLoc.newNilForm()

builtin `EVAL-COND`:
  expect `>=`, 1
  for i in 0 ..< args.posValue.len - 1:
    let rawPair = arg(i).returnIfErr()
    let pair = rawPair.expect(fkMap).returnIfErr()
    if pair.posValue.len != 2:
      return rawPair.locationData.newErrForm(headLoc.newSymForm(`ERR-ARGS-MISMATCH`),
        "COND: expected pair of 2, got " & $pair.posValue.len)
    let condForm = pair.get(0)
    let bodyForm = pair.get(1)
    if ctx.eval(condForm).returnIfErr().toBool():
      return ctx.eval(bodyForm)
  return headLoc.newNilForm()

builtin `EVAL-LET`:
  expect `>=`, 1

  let bindingsForm = arg(0).returnIfErr().expect(fkMap).returnIfErr()

  ctx.pushEnv()

  try:
    for i in 0 ..< bindingsForm.posValue.len:
      let pair = bindingsForm.get(i).expect(fkMap).returnIfErr()
      if pair.posValue.len != 2:
        return pair.locationData.newErrForm(headLoc.newSymForm(`ERR-ARGS-MISMATCH`),
          "LET: expected binding pair of 2, got " & $pair.posValue.len)
      let symForm = pair.get(0).expect(fkSym).returnIfErr()
      let value   = ctx.eval(pair.get(1)).returnIfErr()
      ctx.newSym(symForm.symInterned, value)

    var res = headLoc.newNilForm()
    for i in 2 ..< args.posValue.len:
      res = ctx.eval(args.get(i)).returnIfErr()
    return res
  finally:
    ctx.popEnv()

template argsBodyImpl(args: Form, paramsForm: Form, bodyStart: int,
                      paramsOut, bodyOut: untyped) =
  paramsOut = headLoc.newMapForm()
  for i in 0 ..< paramsForm.posValue.len:
    let sym = paramsForm.get(i).expect(fkSym).returnIfErr()
    let key = headLoc.newSymForm(sym.symInterned)
    if paramsOut.mapValue.hasKey(key):
      return sym.locationData.newErrForm(headLoc.newSymForm(`ERR-DUPLICATE-PARAM`),
        "Duplicate parameter: " & ctx.internmentData.unintern(sym.symInterned))
    paramsOut.mapValue[key] = headLoc.newSymForm(`EVAL-PARAM`)

  bodyOut = headLoc.newMapForm()
  for i in bodyStart ..< args.posValue.len:
    bodyOut.append(args.get(i))

builtin `EVAL-LAMBDA`:
  expect `>=`, 1

  let paramsForm = arg(0).returnIfErr().expect(fkMap).returnIfErr()

  var params, body: Form
  argsBodyImpl(args, paramsForm, 2, params, body)
  let loc = args.get(0).locationData

  return loc.newMapForm(@[
    headLoc.newSymForm(`EVAL-T-LAMBDA`),
    params,
    ctx.env,
    body
  ])

builtin `EVAL-DEFUN`:
  expect `>=`, 2

  let nameForm   = arg(0).returnIfErr().expect(fkSym).returnIfErr()
  let paramsForm = arg(1).returnIfErr().expect(fkMap).returnIfErr()

  var params, body: Form
  argsBodyImpl(args, paramsForm, 3, params, body)

  let closure = headLoc.newMapForm(@[
    headLoc.newSymForm(`EVAL-T-LAMBDA`),
    params,
    ctx.env,
    body
  ])

  let `func` = args.get(0).locationData.newMapForm(@[
    headLoc.newSymForm(`EVAL-FUNC`),
    nameForm,
    closure
  ])

  ctx.newSym(nameForm.symInterned, `func`)
  return nameForm

builtin `EVAL-TRY`:
  expect `>=`, 2
  expect `<=`, 3

  let bodyForm    = arg(0)
  let handlerForm = arg(1)
  let hasFinally  = has(2)
  let finallyForm = if hasFinally: arg(2) else: headLoc.newNilForm()

  var res: Form

  try:
    let bodyRes = ctx.eval(bodyForm)
    if bodyRes.kind == fkErr:
      let callForm = headLoc.newMapForm(@[
        handlerForm,
        headLoc.newMapForm(@[headLoc.newSymForm(`PARSER-QUOTE`), bodyRes.errSym]),
        headLoc.newStrForm(bodyRes.errMsg),
        headLoc.newMapForm(@[headLoc.newSymForm(`PARSER-QUOTE`), bodyRes.errTrace])
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

proc display(form: Form, data: InternmentData): string =
  if form.kind == fkStr: form.strValue
  else: form.toStr(data)

builtin `EVAL-ECHO`:
  expect `>=`, 1

  var s = ""
  for i in 0 ..< args.posValue.len - 1:
    s &= argEval(i).returnIfErr().display(ctx.internmentData)

  stdout.writeLine(s)
  return headLoc.newNilForm()

proc isCons(form: Form): bool =
  form.kind == fkMap and
    form.hasKey(newSymForm(`CONS-HEAD`)) and
    form.hasKey(newSymForm(`CONS-TAIL`))

builtin `EVAL-LEN`:
  expect `==`, 1
  let form = argEval(0).returnIfErr()
  case form.kind
  of fkMap:
    if isCons(form):
      var n = 0
      var cur = form
      while cur.kind == fkMap and cur.hasKey(headLoc.newSymForm(`CONS-HEAD`)):
        inc n
        cur = cur.get(headLoc.newSymForm(`CONS-TAIL`))
      return headLoc.newIntForm(n)
    return headLoc.newIntForm(form.posValue.len + form.mapValue.len)
  of fkStr:
    return headLoc.newIntForm(form.strValue.len)
  else:
    return form.locationData.newErrForm(headLoc.newSymForm(`ERR-TYPE-MISMATCH`),
      "LEN: expected map, cons or string, got " & $form.kind)

builtin `EVAL-EVAL`:
  expect `==`, 1
  return ctx.eval(argEval(0))

builtin `EVAL-BUILD`:
  expect `>=`, 0

  var m = headLoc.newMapForm()
  for i in 1 ..< args.posValue.len:
    m.append(ctx.eval(args.posValue[i]).returnIfErr())
  for k, v in args.mapValue.pairs:
    m.put(k, ctx.eval(v).returnIfErr())
  return m

builtin `EVAL-APPLY`:
  expect `==`, 2
  let funcForm = arg(0).returnIfErr()
  let argsList = argEval(1).returnIfErr().expect(fkMap).returnIfErr()

  var callForm = funcForm.locationData.newMapForm()
  callForm.append(funcForm)
  for i in 0 ..< argsList.posValue.len:
    callForm.append(argsList.posValue[i])

  return ctx.eval(callForm)

# NOTE: BECOME only works correctly in tail position of a function body.
# Using its result anywhere else (binding, condition, arithmetic, etc.)
# is undefined behavior: the ;BECOME marker leaks as a plain value.
builtin `EVAL-BECOME`:
  expect `>=`, 1

  var closure = argEval(0).returnIfErr()

  if closure.kind == fkMap and closure.posValue.len > 0 and
     closure.get(0).kind == fkSym and closure.get(0).symInterned == `EVAL-T-LAMBDA`:
    discard
  elif closure.kind == fkMap and closure.posValue.len > 0 and
       closure.get(0).kind == fkSym and closure.get(0).symInterned == `EVAL-FUNC`:
    closure = closure.get(2)
  else:
    return args.get(0).locationData.newErrForm(headLoc.newSymForm(`ERR-BECOME-NOT-FUNC`),
      "BECOME: not a function")

  var marker = args.get(0).locationData.newMapForm(@[headLoc.newSymForm(`EVAL-T-BECOME`), closure])
  for i in 1 ..< args.posValue.len - 1:
    marker.append(argEval(i).returnIfErr())
  return marker

builtin `EVAL-DO`:
  expect `>=`, 0
  var res = headLoc.newNilForm()
  for i in 1 ..< args.posValue.len:
    res = ctx.eval(args.get(i)).returnIfErr()
  return res

builtin `EVAL-HAS`:
  expect `==`, 2
  let map = argEval(0).returnIfErr().expect(fkMap).returnIfErr()
  let key = argEval(1).returnIfErr()
  if map.hasKey(key):
    return headLoc.newIntForm(1)
  return headLoc.newNilForm()

builtin `EVAL-PUT`:
  expect `==`, 3
  let map = argEval(0).returnIfErr().expect(fkMap).returnIfErr()
  let key = argEval(1).returnIfErr()
  let val = argEval(2).returnIfErr()
  map.put(key, val)
  return map

builtin `EVAL-APPEND`:
  expect `==`, 2
  let map = argEval(0).returnIfErr().expect(fkMap).returnIfErr()
  let val = argEval(1).returnIfErr()
  map.append(val)
  return map

builtin `EVAL-IN`:
  expect `==`, 2
  let map = argEval(0).returnIfErr().expect(fkMap).returnIfErr()
  let val = argEval(1).returnIfErr()

  for v in map.posValue:
    if v == val: return headLoc.newIntForm(1)
  for _, v in map.mapValue.pairs:
    if v == val: return headLoc.newIntForm(1)
  return headLoc.newNilForm()

builtin `EVAL-WHILE`:
  expect `>=`, 2
  let condForm = arg(0)
  while true:
    let c = ctx.eval(condForm).returnIfErr()
    if not c.toBool(): break
    for i in 2 ..< args.posValue.len:
      let r = ctx.eval(args.get(i))
      if r.kind == fkErr: return r
  return headLoc.newNilForm()

builtin `EVAL-MAP`:
  expect `==`, 2
  let fn = argEval(0).returnIfErr()
  let coll = argEval(1).returnIfErr().expect(fkMap).returnIfErr()
  var res = headLoc.newMapForm()
  for v in coll.posValue:
    res.append(ctx.eval(v.locationData.newMapForm(@[fn, v])).returnIfErr())
  return res

builtin `EVAL-FILTER`:
  expect `==`, 2
  let fn = argEval(0).returnIfErr()
  let coll = argEval(1).returnIfErr().expect(fkMap).returnIfErr()
  var res = headLoc.newMapForm()
  for v in coll.posValue:
    if ctx.eval(v.locationData.newMapForm(@[fn, v])).returnIfErr().toBool():
      res.append(v)
  return res

builtin `EVAL-REDUCE`:
  expect `==`, 3
  let fn = argEval(0).returnIfErr()
  let acc0 = argEval(1).returnIfErr()
  let coll = argEval(2).returnIfErr().expect(fkMap).returnIfErr()
  var acc = acc0
  for v in coll.posValue:
    acc = ctx.eval(v.locationData.newMapForm(@[fn, acc, v])).returnIfErr()
  return acc

builtin `EVAL-SIGNAL`:
  expect `>=`, 1
  expect `<=`, 3

  let kind = argEval(0).returnIfErr().expect(fkSym).returnIfErr()
  let msg  = if has(1): argEval(1).returnIfErr().expect(fkStr).returnIfErr().strValue else: ""

  var loc = args.get(0).locationData
  if has(2):
    let rawTarget = arg(2)
    if rawTarget.kind == fkSym:
      loc = ctx.eval(rawTarget).returnIfErr().locationData

  return loc.newErrForm(headLoc.newSymForm(kind.symInterned), "[signal] " & msg)

builtin `EVAL-INT?`:
  expect `==`, 1
  if argEval(0).returnIfErr().kind == fkInt: return headLoc.newIntForm(1)
  return headLoc.newNilForm()

builtin `EVAL-SYM?`:
  expect `==`, 1
  if argEval(0).returnIfErr().kind == fkSym: return headLoc.newIntForm(1)
  return headLoc.newNilForm()

builtin `EVAL-MAP?`:
  expect `==`, 1
  if argEval(0).returnIfErr().kind == fkMap: return headLoc.newIntForm(1)
  return headLoc.newNilForm()

builtin `EVAL-STR?`:
  expect `==`, 1
  if argEval(0).returnIfErr().kind == fkStr: return headLoc.newIntForm(1)
  return headLoc.newNilForm()

builtin `EVAL-NIL?`:
  expect `==`, 1
  if argEval(0).returnIfErr().kind == fkNil: return headLoc.newIntForm(1)
  return headLoc.newNilForm()

builtin `EVAL-EACH`:
  expect `>=`, 3

  let map     = argEval(0).returnIfErr().expect(fkMap).returnIfErr()
  let binding = arg(1).returnIfErr()

  ctx.pushEnv()

  try:
    var last: Form = headLoc.newNilForm()

    if isCons(map):
      var cur = map
      var idx = 0

      if binding.kind == fkSym:
        while isCons(cur):
          ctx.newSym(binding.symInterned, cur.get(headLoc.newSymForm(`CONS-HEAD`)))
          for i in 3 ..< args.posValue.len:
            let r = ctx.eval(args.get(i))
            if r.kind == fkErr: return r
            last = r
          cur = cur.get(headLoc.newSymForm(`CONS-TAIL`))
          inc idx
      elif binding.kind == fkMap and binding.posValue.len == 2:
        let keySym = binding.get(0).expect(fkSym).returnIfErr()
        let valSym = binding.get(1).expect(fkSym).returnIfErr()
        while isCons(cur):
          ctx.newSym(keySym.symInterned, binding.locationData.newIntForm(idx))
          ctx.newSym(valSym.symInterned, cur.get(headLoc.newSymForm(`CONS-HEAD`)))
          for i in 3 ..< args.posValue.len:
            let r = ctx.eval(args.get(i))
            if r.kind == fkErr: return r
            last = r
          cur = cur.get(headLoc.newSymForm(`CONS-TAIL`))
          inc idx
      else:
        return binding.locationData.newErrForm(headLoc.newSymForm(`ERR-ARGS-MISMATCH`),
          "each: expected symbol or (symbol symbol) after map")

      return last

    elif binding.kind == fkSym:
      for v in map.posValue:
        ctx.newSym(binding.symInterned, v)
        for i in 3 ..< args.posValue.len:
          let r = ctx.eval(args.get(i))
          if r.kind == fkErr: return r
          last = r
      for _, v in map.mapValue.pairs:
        ctx.newSym(binding.symInterned, v)
        for i in 3 ..< args.posValue.len:
          let r = ctx.eval(args.get(i))
          if r.kind == fkErr: return r
          last = r

    elif binding.kind == fkMap and binding.posValue.len == 2:
      let keySym = binding.get(0).expect(fkSym).returnIfErr()
      let valSym = binding.get(1).expect(fkSym).returnIfErr()

      var idx = 0
      for v in map.posValue:
        ctx.newSym(keySym.symInterned, binding.locationData.newIntForm(idx))
        ctx.newSym(valSym.symInterned, v)
        for i in 3 ..< args.posValue.len:
          let r = ctx.eval(args.get(i))
          if r.kind == fkErr: return r
          last = r
        inc idx
      for k, v in map.mapValue.pairs:
        ctx.newSym(keySym.symInterned, k)
        ctx.newSym(valSym.symInterned, v)
        for i in 3 ..< args.posValue.len:
          let r = ctx.eval(args.get(i))
          if r.kind == fkErr: return r
          last = r

    else:
      return binding.locationData.newErrForm(headLoc.newSymForm(`ERR-ARGS-MISMATCH`),
        "each: expected symbol or (symbol symbol) after map")

    return last
  finally:
    ctx.popEnv()

builtin `EVAL-CONS`:
  expect `==`, 2
  let h = argEval(0).returnIfErr()
  let t = argEval(1).returnIfErr()
  return args.get(0).locationData.newMapForm(@{
    headLoc.newSymForm(`CONS-HEAD`): h,
    headLoc.newSymForm(`CONS-TAIL`): t
  })

builtin `EVAL-HEAD`:
  expect `==`, 1
  let xs = argEval(0).returnIfErr().expect(fkMap).returnIfErr()
  if not xs.hasKey(headLoc.newSymForm(`CONS-HEAD`)):
    return args.get(0).locationData.newErrForm(headLoc.newSymForm(`ERR-TYPE-MISMATCH`),
      "HEAD: not a cons")
  return xs.get(headLoc.newSymForm(`CONS-HEAD`))

builtin `EVAL-TAIL`:
  expect `==`, 1
  let xs = argEval(0).returnIfErr().expect(fkMap).returnIfErr()
  if not xs.hasKey(headLoc.newSymForm(`CONS-TAIL`)):
    return args.get(0).locationData.newErrForm(headLoc.newSymForm(`ERR-TYPE-MISMATCH`),
      "TAIL: not a cons")
  return xs.get(headLoc.newSymForm(`CONS-TAIL`))

builtin `EVAL-NTH`:
  expect `==`, 2
  let idx = argEval(0).returnIfErr().expect(fkInt).returnIfErr().intValue
  var xs  = argEval(1).returnIfErr()
  if idx < 0:
    return args.get(2).locationData.newErrForm(headLoc.newSymForm(`ERR-KEY-ERROR`),
      "NTH: negative index")
  var i = 0
  while i < idx:
    xs = xs.expect(fkMap).returnIfErr()
    if not xs.hasKey(headLoc.newSymForm(`CONS-TAIL`)):
      return args.get(2).locationData.newErrForm(headLoc.newSymForm(`ERR-KEY-ERROR`),
        "NTH: index out of range")
    xs = xs.get(headLoc.newSymForm(`CONS-TAIL`))
    inc i
  return xs.expect(fkMap).returnIfErr().get(headLoc.newSymForm(`CONS-HEAD`))

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

proc isBecomeMarker(form: Form): bool =
  form.kind == fkMap and form.posValue.len > 0 and
    form.get(0).kind == fkSym and form.get(0).symInterned == `EVAL-T-BECOME`

proc bindFrame(ctx: Context, closure: Form, argValues: seq[Form]) =
  let params     = closure.get(1)
  let closureEnv = closure.get(2)

  let frame = newMapForm()
  var pi = 0
  for paramSym, _ in params.mapValue.pairs:
    frame.mapValue[newSymForm(paramSym.symInterned)] = argValues[pi]
    inc pi

  ctx.env = newMapForm(@{
    newSymForm(`ENV-CURRENT`): frame,
    newSymForm(`ENV-PARENT`):  closureEnv
  })

proc evalBodies(ctx: Context, body: Form): Form =
  var res = newNilForm()
  for i in 0 ..< body.posValue.len:
    let r = ctx.eval(body.get(i))
    if r.kind == fkErr: return r
    if i < body.posValue.len - 1 and isBecomeMarker(r):
      return body.get(i).locationData.newErrForm(newSymForm(`ERR-BECOME-NON-TAIL`),
        "become in non-tail position")
    res = r
  return res

proc funcall(ctx: Context, closureParam: Form, callForm: Form): Form =
  let outerEnv = ctx.env
  var loc = callForm.locationData
  var closure = closureParam

  let argc = callForm.posValue.len - 1
  var argValues = newSeq[Form](argc)
  for i in 0 ..< argc:
    argValues[i] = ctx.eval(callForm.get(i + 1))
    if argValues[i].kind == fkErr:
      ctx.env = outerEnv
      return argValues[i]

  while true:
    if argValues.len != closure.get(1).mapValue.len:
      ctx.env = outerEnv
      return loc.newErrForm(newSymForm(`ERR-ARGS-MISMATCH`),
        "Arguments mismatch: expected " & $closure.get(1).mapValue.len &
        ", got " & $argValues.len)

    bindFrame(ctx, closure, argValues)

    let res = evalBodies(ctx, closure.get(3))
    if res.kind == fkErr:
      ctx.env = outerEnv
      return res

    if isBecomeMarker(res):
      loc = res.locationData
      closure = res.get(1)
      argValues = res.posValue[2 .. ^1]
      continue

    ctx.env = outerEnv
    return res

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
  if head.kind == fkSym and head.symInterned in [`PARSER-KEYWORD`, `EVAL-T-LAMBDA`, `EVAL-FUNC`]:
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

  elif ty == `EVAL-FUNC`:
    let funcName = callForm.get(1)
    let closure  = callForm.get(2)
    result = funcall(ctx, closure, form)
    if result.kind == fkErr:
      result.errTrace.append(head.locationData.newSymForm(funcName.symInterned))

  else:
    return head.locationData.newErrForm(newSymForm(`ERR-CANNOT-CALL`),
      "Unknown callable: " & ctx.internmentData.unintern(ty))