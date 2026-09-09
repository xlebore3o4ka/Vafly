import std/[strutils, sequtils, tables, macros]

type
  FormKind* = enum
    fkSym = "symbol"
    fkInt = "int"
    fkList = "list"
    fkStr = "str"

  Form* = ref object
    case kind*: FormKind
    of fkSym:  
      original*: string
      symValue*: string
    of fkInt:  intValue*: int
    of fkList: values*:   seq[Form]
    of fkStr:  strValue*: string
    filename*: string
    line*, col*: int

  PContext* = ref object
    text*, filename*: string
    pos*:  int
    col*:  int
    line*: int
    parentheses*: seq[tuple[filename: string, col: int, line: int]]

proc newSymForm*(original: string): Form =
  Form(kind: fkSym, symValue: original.toUpper(), original: original)

proc newIntForm*(intValue: int): Form =
  Form(kind: fkInt, intValue: intValue)

proc newListForm*(values: seq[Form]): Form =
  Form(kind: fkList, values: values)

proc newStrForm*(strValue: string): Form =
  Form(kind: fkStr, strValue: strValue)

proc `$`*(form: Form): string =
  if form == nil: return "NIL-VALUE"
  case form.kind:
  of fkSym: return form.symValue
  of fkInt: return $form.intValue
  of fkList:
    result = "(" & form.values.mapIt($it).join(" ")
    result &= ")"
  of fkStr: return form.strValue.repr

template peek(ctx: PContext): char =
  if ctx.pos in 0..<ctx.text.len: ctx.text[ctx.pos] else: '\0'

template advance(ctx: PContext, n: int = 1) =
  for _ in 0..<n:
    if ctx.pos < ctx.text.len:
      if ctx.text[ctx.pos] == '\n':
        inc ctx.line
        ctx.col = 1
      else:
        inc ctx.col
      inc ctx.pos

template next(ctx: PContext): char =
  let c = ctx.peek()
  if c != '\0':
    ctx.advance()
  c

template isDigit(c: char): bool =
  c >= '0' and c <= '9'

template isSym*(c: char): bool =
  c notin " \0\t\r\n;\"()':"

proc formatError(filename: string; line, col: int; message: string; args: varargs[string, `$`]): string =
  var msg = message
  for i, arg in args:
    msg = msg.replace("$" & $(i + 1), arg)
  return "$1:$2:$3: " & msg % [filename, $line, $col]

proc parseForm*(ctx: PContext): Form

proc parse*(ctx: PContext): Form =
  result = Form(kind: fkList, values: @[], filename: ctx.filename, col: ctx.col, line: ctx.line)

  while ctx.peek() notin ")\0":

    let form = ctx.parseForm()
    if form == nil: break
    result.values.add(form)

proc parseForm*(ctx: PContext): Form =
  while ctx.peek() in " \t\r\n":
    ctx.advance()

  if ctx.peek() == ';':
    while ctx.peek() notin "\n\0":
      ctx.advance()

    if ctx.peek() == '\n':
      ctx.advance()

    return ctx.parseForm()

  if ctx.peek() in "\0)":
    return nil

  let formLine = ctx.line
  let formCol = ctx.col
  var c = ctx.next()

  if c.isDigit() or (c == '-' and ctx.peek().isDigit()):
    var buffer: string
    buffer.add(c)

    while true:
      let nextC = ctx.peek()

      if nextC.isDigit():
        buffer.add(nextC)
        ctx.advance()
      else: break

    result = newIntForm(buffer.parseInt())

  elif c == '"':
    var buffer = "\""
    c = ctx.next()

    while c != '"' and c != '\0':
      if c == '\\' and ctx.peek() in "\"\\":
        c = ctx.next()

      buffer.add(c)

      c = ctx.next()

    buffer.add('"')

    if c == '"':
      result = newStrForm(buffer.unescape)
    else:
      raise newException(ValueError, formatError(
        ctx.filename, ctx.line, ctx.col,
        "unterminated string literal"
      ))

  elif c == '(':
    ctx.parentheses.add((filename: ctx.filename, col: formCol, line: formLine))
    
    result = ctx.parse()

    if ctx.parentheses.len == 0: 
      raise newException(ValueError, formatError(
        ctx.filename, ctx.line, ctx.col,
        "unmatched ')'"
      ))
    discard ctx.parentheses.pop()
    ctx.advance()

  elif c == '\'':
    let form = ctx.parseForm()

    if form == nil:
      raise newException(ValueError, formatError(
        ctx.filename, ctx.line, ctx.col,
        "expected form after '\\''"
      ))

    result = newListForm(@[
      newSymForm("quote"),
      form
    ])

  elif c == ':':
    let sym = ctx.parseForm()

    if sym == nil:
      raise newException(ValueError, formatError(
        ctx.filename, ctx.line, ctx.col,
        "expected symbol after ':'"
      ))
    if sym.kind != fkSym:
      raise newException(ValueError, formatError(
        ctx.filename, ctx.line, ctx.col,
        "expected symbol, got $1",
        $sym.kind
      ))

    result = newListForm(@[
      newSymForm("keyword"),
      sym
    ])

  elif c.isSym():
    var buffer: string
    buffer.add(c)

    while true:
      let nextC = ctx.peek()

      if nextC.isSym():
        buffer.add(nextC)
        ctx.advance()

      else:
        break

    result = newSymForm(buffer)

  result.filename = ctx.filename
  result.line = formLine
  result.col = formCol

proc parse*(text, filename: string): Form =
  var ctx = PContext(text: text, filename: filename, pos: 0, col: 0, line: 0, parentheses: @[])

  result = parse(ctx)

  if ctx.parentheses.len != 0:
    let (f, c, l) = ctx.parentheses.pop()
    raise newException(ValueError, formatError(
      f, l, c,
      "unclosed '('"
    ))

type
  Env* = ref object
    parent*: Env
    bindings*: Table[string, Form]

  IContext* = ref object
    environment*: Env

proc symExists(ctx: IContext, name: string): bool =
  var env = ctx.environment
  while env != nil:
    if name in env.bindings:
      return true
    env = env.parent
  return false

proc getSym(ctx: IContext, name: string): Form =
  var env = ctx.environment
  while env != nil:
    if name in env.bindings:
      return env.bindings[name]
    env = env.parent
  raise newException(ValueError, "undefined variable: " & name)

proc pushEnv(ctx: IContext) =
  ctx.environment = Env(parent: ctx.environment, bindings: initTable[string, Form]())

proc popEnv(ctx: IContext) =
  if ctx.environment != nil:
    ctx.environment = ctx.environment.parent
  else:
    raise newException(ValueError, "EnvStack underflow")

proc newSym(ctx: IContext, name: string, form: Form) =
  if ctx.environment == nil:
    raise newException(ValueError, "Env is nil")
  ctx.environment.bindings[name] = form

proc setSym(ctx: IContext, name: string, form: Form) =
  var env = ctx.environment
  while env != nil:
    if name in env.bindings:
      env.bindings[name] = form
      return
    env = env.parent
  if ctx.environment != nil:
    ctx.environment.bindings[name] = form
  else:
    raise newException(ValueError, "Env is nil")

proc eval*(ctx: IContext, form: Form): Form 

var builtinDispatcher = initTable[string, proc (ctx: IContext, args: seq[Form]): Form]()
var builtinBindings = initTable[string, Form]()

macro builtin(name: string, body: untyped): untyped =
  result = newStmtList()
  result.add quote do:
    builtinDispatcher[`name`] = proc (ctx {.inject.}: IContext, args {.inject.}: seq[Form]): Form =
      `body`
    builtinBindings[`name`] = newListForm(@[newSymForm("<BUILTIN>")])

template argN(a: int): Form =
  ctx.eval(args[a])

template expectArgsN(a: int) =
  if args.len != a:
    raise newException(ValueError, "arguments mismatch (expected " & $a & ")")

template `expectArgs<=N`(a: int) =
  if args.len > a:
    raise newException(ValueError, "arguments mismatch (expected <=" & $a & ")")

template `expectArgs>=N`(a: int) =
  if args.len < a:
    raise newException(ValueError, "arguments mismatch (expected >=" & $a & ")")

template expect(form: Form, ekind: FormKind): Form =
  if form.kind != ekind:
    raise newException(ValueError, "Expected " & $ekind & ", got " & $form.kind)
  form

builtin "+":
  var res = argN(0).expect(fkInt).intValue
  for arg in args[1..^1]:
    res += ctx.eval(arg).expect(fkInt).intValue
  return newIntForm(res)

builtin "-":
  var res = argN(0).expect(fkInt).intValue
  for arg in args[1..^1]:
    res -= ctx.eval(arg).expect(fkInt).intValue
  return newIntForm(res)

builtin "*":
  var res = argN(0).expect(fkInt).intValue
  for arg in args[1..^1]:
    res *= ctx.eval(arg).expect(fkInt).intValue
  return newIntForm(res)

builtin "DIV":
  var res = argN(0).expect(fkInt).intValue
  for arg in args[1..^1]:
    res = res div ctx.eval(arg).expect(fkInt).intValue
  return newIntForm(res)

proc eqForm(a, b: Form): bool =
  if a == nil and b == nil: return true
  if a == nil or b == nil: return false
  if a.kind != b.kind: return false
  case a.kind:
  of fkInt: return a.intValue == b.intValue
  of fkStr: return a.strValue == b.strValue
  of fkSym: return a.symValue == b.symValue
  of fkList:
    if a.values.len != b.values.len: return false
    for i in 0..<a.values.len:
      if not eqForm(a.values[i], b.values[i]): return false
    return true

builtin "EQ":
  `expectArgs>=N`(2)
  let first = ctx.eval(argN(0))
  for i in 1 ..< args.len:
    let next = ctx.eval(args[i])
    if not eqForm(first, next):
      return newListForm(@[])
  return newIntForm(1)

builtin "NEQ":
  `expectArgs>=N`(2)
  let first = ctx.eval(argN(0))
  for i in 1 ..< args.len:
    let next = ctx.eval(args[i])
    if eqForm(first, next):
      return newListForm(@[])
  return newIntForm(1)

builtin ">":
  expectArgsN(2)
  let a = argN(0).expect(fkInt).intValue
  let b = argN(1).expect(fkInt).intValue
  if a > b: return newIntForm(1) else: return newListForm(@[])

builtin "<":
  expectArgsN(2)
  let a = argN(0).expect(fkInt).intValue
  let b = argN(1).expect(fkInt).intValue
  if a < b: return newIntForm(1) else: return newListForm(@[])

builtin ">=":
  expectArgsN(2)
  let a = argN(0).expect(fkInt).intValue
  let b = argN(1).expect(fkInt).intValue
  if a >= b: return newIntForm(1) else: return newListForm(@[])

builtin "<=":
  expectArgsN(2)
  let a = argN(0).expect(fkInt).intValue
  let b = argN(1).expect(fkInt).intValue
  if a <= b: return newIntForm(1) else: return newListForm(@[])

proc formToBool(form: Form): bool =
  if form == nil: return false
  case form.kind:
  of fkStr: return form.strValue.len != 0
  of fkSym: return false
  of fkInt: return form.intValue != 0
  of fkList: return form.values.len != 0

builtin "AND":
  `expectArgs>=N`(1)
  var last = newListForm(@[])
  for i in 0 ..< args.len:
    let val = ctx.eval(args[i])
    if not formToBool(val):
      return newListForm(@[])
    last = val
  return last

builtin "OR":
  `expectArgs>=N`(1)
  for i in 0 ..< args.len:
    let val = ctx.eval(args[i])
    if formToBool(val):
      return val
  return newListForm(@[])

builtin "QUOTE":
  expectArgsN(1)
  return args[0]

builtin "NTH":
  expectArgsN(2)
  let list = argN(0).expect(fkList)
  let idx = argN(1).expect(fkInt).intValue
  return list.values[idx]

builtin "LETH":
  expectArgsN(2)

  let name = args[0].expect(fkSym).symValue
  let val = argN(1)
  ctx.newSym(name, val)

  return val

builtin "LET":
  `expectArgs>=N`(1)
  ctx.pushEnv()

  let pairs = args[0].expect(fkList).values
  for rawpair in pairs:

    let pair = rawpair.expect(fkList).values

    if pair.len != 2:
      raise newException(ValueError, "Expected form: '(sym form)'")

    let name = pair[0].expect(fkSym).symValue
    let val = ctx.eval(pair[1])

    ctx.newSym(name, val)

  for i in 1..<args.len:
    result = ctx.eval(args[i])

  ctx.popEnv()

builtin "SET":
  `expectArgs>=N`(2)

  let sym = args[0].expect(fkSym)
  let name = sym.symValue

  if not ctx.symExists(name):
    raise newException(ValueError, "Undefined symbol '$1'" % [sym.original])

  let val = argN(1)
  ctx.setSym(name, val)
  
  return val

proc formToStr(form: Form): string =
    if form == nil: return "NIL"
    case form.kind:
    of fkStr: return form.strValue
    of fkSym: return form.symValue
    of fkInt: return $form.intValue
    of fkList: return $form

template `builtin-PRINF-impl`(line: static[bool]) =
  `expectArgs>=N`(1)
  `expectArgs<=N`(100)

  var str = argN(0).expect(fkStr).strValue.replace("~~", "\x01")

  for n, arg in args[1..^1]:
    str = str.replace("~" & $n, formToStr ctx.eval(arg))

  when line:
    stdout.writeLine str.replace("\x01", "~")
  else:
    stdout.write str.replace("\x01", "~")

  return newStrForm(str)

builtin "PRINT":
  `builtin-PRINF-impl`(false)

builtin "PRINTLN":
  `builtin-PRINF-impl`(true)

builtin "IF":
  expectArgsN(3)

  let cond = argN(0).formToBool

  if cond:
    return argN(1)
  return argN(2)

builtin "COND":
  `expectArgs>=N`(1)

  for rawpair in args:

    let pair = rawpair.expect(fkList).values

    if pair.len != 2:
      raise newException(ValueError, "Expected form: '(form form)'")

    let cond = ctx.eval(pair[0]).formToBool
    
    if cond:
      return ctx.eval(pair[1])

  return newListForm(@[])

builtin "EVAL":
  expectArgsN(1)
  return ctx.eval(argN(0))

builtin "DEFUN":
  discard 

proc eval*(ctx: IContext, form: Form): Form =
  if form.kind == fkSym: 
    if not ctx.symExists(form.symValue):
      raise newException(ValueError, formatError(
        form.filename, form.line, form.col,
        "Undefined symbol '$1'", form.original
      ))

    return ctx.getSym(form.symValue)

  elif form.kind in {fkInt, fkStr} or form.values.len == 0: 
    return form
  elif form.values[0].kind != fkSym:
    var evaluatedForm = newListForm(@[])
    for val in form.values:
      evaluatedForm.values.add ctx.eval(val)
    return evaluatedForm

  let sym = form.values[0].symValue 

  if not ctx.symExists(sym):
    raise newException(ValueError, formatError(
      form.filename, form.line, form.col,
      "Undefined symbol '$1'", form.values[0].original
    ))

  let symForm = ctx.getSym(sym)
  
  if symForm.values.len == 0 or symForm.values[0].kind != fkSym: return symForm

  let ty = symForm.values[0].symValue

  if ty == "<BUILTIN>":
    return builtinDispatcher[sym](ctx, form.values[1..^1])

proc icontext*(): IContext =
  return IContext(environment: Env(
    bindings: builtinBindings
  ))