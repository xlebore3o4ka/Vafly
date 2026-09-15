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
  return "$1:$2:$3: " % [filename, $line, $col] & msg

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
  IContext* = ref object
    environment*: Form

proc symExists(ctx: IContext, name: string): bool =
  var env = ctx.environment

  while env != nil and env.values.len != 0:
    if env.values[0].values.anyIt(it.values[0].symValue == name):
      return true
    env = env.values[1]

  return false

proc getSym(ctx: IContext, name: string): Form =
  var env = ctx.environment

  while env != nil and env.values.len != 0:
    for pair in env.values[0].values:
      if pair.values[0].symValue == name:
        return pair.values[1]
    env = env.values[1]

  raise newException(ValueError, "undefined symbol: " & name)

proc pushEnv(ctx: IContext) =
  ctx.environment = newListForm(@[
    newListForm(@[]),
    ctx.environment
  ])

proc popEnv(ctx: IContext) =
  if ctx.environment == nil or ctx.environment.values.len == 0:
    raise newException(ValueError, "EnvStack underflow")

  let parent = ctx.environment.values[1]
  if parent == nil and ctx.environment.values[0].values.len > 0:
    raise newException(ValueError, "EnvStack underflow")

  ctx.environment = parent

proc newSym(ctx: IContext, name: string, form: Form) =
  var env = ctx.environment
  if env == nil or env.values.len == 0:
    raise newException(ValueError, "Env is nil")

  for pair in env.values[0].values:
    if pair.values[0].symValue == name:
      pair.values[1] = form
      return

  env.values[0].values.add(newListForm(@[
    newSymForm(name), form
  ]))

proc setSym(ctx: IContext, name: string, form: Form) =
  var env = ctx.environment
  while env != nil and env.values.len != 0:
    for pair in env.values[0].values:
      if pair.values[0].symValue == name:
        pair.values[1] = form
        return
    env = env.values[1]

  if ctx.environment == nil:
    raise newException(ValueError, "Env is nil")

  ctx.environment.values[0].values.add(newListForm(@[
    newSymForm(name), form
  ]))

proc eval*(ctx: IContext, form: Form): Form 

var builtinDispatcher = initTable[string, proc (ctx: IContext, args: seq[Form]): Form]()
var builtinBindings = newListForm(@[newListForm(@[]), newListForm(@[])])

macro builtin(name: string, body: untyped): untyped =
  result = newStmtList()
  result.add quote do:
    builtinDispatcher[`name`] = proc (ctx {.inject.}: IContext, args {.inject.}: seq[Form]): Form =
      `body`
    builtinBindings.values[0].values.add(newListForm(@[
      newSymForm(`name`), newListForm(@[newSymForm(";BUILTIN")])
    ]))

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

proc icontext*(): IContext =
  return IContext(environment: builtinBindings)

var testsExamples {.compiletime.}: seq[(NimNode, NimNode)]

macro example(comparison: untyped) =
  expectKind(comparison, nnkInfix)

  let op = comparison[0]
  if not op.eqIdent("=="):
    error "example: expected '==' comparison, got '" & op.repr & "'", comparison

  testsExamples.add (comparison[1], comparison[2])

macro testExamples(): untyped =
  result = newStmtList()

  for i, (lvalue, rvalue) in testsExamples:
    let ctxSym   = genSym(nskLet, "ctx"   & $i)
    let lformSym = genSym(nskLet, "lform" & $i)
    let rformSym = genSym(nskLet, "rform" & $i)

    let lvalueStr = lvalue.strVal
    let rvalueStr = rvalue.strVal
    let lvalueNode = lvalue
    let rvalueNode = rvalue
    let lineInfo = lvalueNode.lineInfoObj

    result.add quote do:
      block:
        let `ctxSym`   = icontext()
        let `lformSym` = `ctxSym`.eval(parse(`lvalueNode`, "<exampleStringL>").values[0])
        let `rformSym` = `ctxSym`.eval(parse(`rvalueNode`, "<exampleStringR>").values[0])
        if not eqForm(`lformSym`, `rformSym`):
          echo "example failed at ", `lineInfo`, ": ",
            `lvalueStr`, " == ", `rvalueStr`,
            "  (got ", `lformSym`.formToStr, " vs ", `rformSym`.formToStr, ")"

builtin "+":
  example "(+ 1 2)"   == "3"
  example "(+ 10 13)" == "23"

  var res = argN(0).expect(fkInt).intValue
  for arg in args[1..^1]:
    res += ctx.eval(arg).expect(fkInt).intValue
  return newIntForm(res)

builtin "-":
  example "(- 5 3)"   == "2"
  example "(- 10 4)"  == "6"

  var res = argN(0).expect(fkInt).intValue
  for arg in args[1..^1]:
    res -= ctx.eval(arg).expect(fkInt).intValue
  return newIntForm(res)

builtin "*":
  example "(* 2 3)"   == "6"
  example "(* 4 5)"   == "20"

  var res = argN(0).expect(fkInt).intValue
  for arg in args[1..^1]:
    res *= ctx.eval(arg).expect(fkInt).intValue
  return newIntForm(res)

builtin "DIV":
  example "(DIV 6 2)"  == "3"
  example "(DIV 20 4)" == "5"

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
  example "(EQ 1 1)" == "1"
  example "(EQ 1 2)" == "()"

  `expectArgs>=N`(2)
  let first = ctx.eval(argN(0))
  for i in 1 ..< args.len:
    let next = ctx.eval(args[i])
    if not eqForm(first, next):
      return newListForm(@[])
  return newIntForm(1)

builtin "NEQ":
  example "(NEQ 1 2)" == "1"
  example "(NEQ 1 1)" == "()"

  `expectArgs>=N`(2)
  let first = ctx.eval(argN(0))
  for i in 1 ..< args.len:
    let next = ctx.eval(args[i])
    if eqForm(first, next):
      return newListForm(@[])
  return newIntForm(1)

builtin ">":
  example "(> 2 1)"   == "1"
  example "(> 1 2)"   == "()"

  expectArgsN(2)
  let a = argN(0).expect(fkInt).intValue
  let b = argN(1).expect(fkInt).intValue
  if a > b: return newIntForm(1) else: return newListForm(@[])

builtin "<":
  example "(< 1 2)"   == "1"
  example "(< 2 1)"   == "()"

  expectArgsN(2)
  let a = argN(0).expect(fkInt).intValue
  let b = argN(1).expect(fkInt).intValue
  if a < b: return newIntForm(1) else: return newListForm(@[])

builtin ">=":
  example "(>= 2 2)"  == "1"
  example "(>= 1 2)"  == "()"

  expectArgsN(2)
  let a = argN(0).expect(fkInt).intValue
  let b = argN(1).expect(fkInt).intValue
  if a >= b: return newIntForm(1) else: return newListForm(@[])

builtin "<=":
  example "(<= 1 1)"  == "1"
  example "(<= 2 1)"  == "()"

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
  example "(AND 1 1)"   == "1"
  example "(AND 1 ())"  == "()"

  `expectArgs>=N`(1)
  var last = newListForm(@[])
  for i in 0 ..< args.len:
    let val = ctx.eval(args[i])
    if not formToBool(val):
      return newListForm(@[])
    last = val
  return last

builtin "OR":
  example "(OR () 1)"   == "1"
  example "(OR () ())"  == "()"

  `expectArgs>=N`(1)
  for i in 0 ..< args.len:
    let val = ctx.eval(args[i])
    if formToBool(val):
      return val
  return newListForm(@[])

builtin "QUOTE":
  example "(QUOTE (+ 1 2))" == "'(+ 1 2)"
  example "(QUOTE 5)"       == "5"

  expectArgsN(1)
  return args[0]

builtin "NTH":
  example "(NTH '(10 20 30) 0)" == "10"
  example "(NTH '(10 20 30) 2)" == "30"

  expectArgsN(2)
  let list = argN(0).expect(fkList)
  let idx = argN(1).expect(fkInt).intValue
  return list.values[idx]

builtin "LETH":
  example "(LETH X 5)"  == "5"
  example "(LETH Y 42)" == "42"

  expectArgsN(2)

  let name = args[0].expect(fkSym).symValue
  let val = argN(1)
  ctx.newSym(name, val)

  return val

builtin "LET":
  example "(LET ((X 5)) X)"                     == "5"
  example "(LET ((X 1) (Y 2)) (+ X Y))"         == "3"
  example "(LET ((X 5)) X (+ X 1))"             == "6"
  example "(LET ((X 5)) (+ X X))"               == "10"
  example "(LET ((X 5)) (LET ((Y 6)) (+ X Y)))" == "11"

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
  example "(LET ((X 0)) (SET X 5) X)"                == "5"
  example "(LET ((X 1) (Y 2)) (SET X 10) (+ X Y))"   == "12"
  example "(LET ((X 1)) (LET ((Y 2)) (SET X 99)) X)" == "99"

  `expectArgs>=N`(2)

  let sym = args[0].expect(fkSym)
  let name = sym.symValue

  if not ctx.symExists(name):
    raise newException(ValueError, "Undefined symbol '$1'" % [sym.original])

  let val = argN(1)
  ctx.setSym(name, val)
  
  return val

proc formToStr(form: Form, nestingLevel: Natural = 0): string =
  if form == nil: return "NIL"
  case form.kind:
  of fkStr: return form.strValue
  of fkSym: return form.symValue
  of fkInt: return $form.intValue
  of fkList: 
    if nestingLevel > 4:
      return "(...)"
    return "(" & form.values.mapIt(it.formToStr(nestingLevel + 1)).join(" ") & ")"

template `builtin-PRINF-impl`(line: static[bool]) =
  `expectArgs>=N`(1)
  `expectArgs<=N`(100)

  var str = argN(0).expect(fkStr).strValue.replace("~~", "\\\x01")

  for n, arg in args[1..^1]:
    str = str.replace("~" & $n, formToStr ctx.eval(arg))

  when line:
    stdout.writeLine str.replace("\\\x01", "~")
  else:
    stdout.write str.replace("\\\x01", "~")

  return newStrForm(str)

builtin "PRINT":
  `builtin-PRINF-impl`(false)

builtin "PRINTLN":
  `builtin-PRINF-impl`(true)

builtin "IF":
  example "(IF 1 10 20)"  == "10"
  example "(IF 0 10 20)"  == "20"
  example "(IF () 10 20)" == "20"

  expectArgsN(3)

  let cond = argN(0).formToBool

  if cond:
    return argN(1)
  return argN(2)

builtin "COND":
  example "(COND (1 10) (1 20))"    == "10"
  example "(COND (0 10) (1 20))"    == "20"
  example "(COND (() 10) (() 20))"  == "()"

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
  example "(EVAL '(+ 1 2))" == "3"
  example "(EVAL ''(+ 1 2))" == "'(+ 1 2)"

  expectArgsN(1)
  return ctx.eval(argN(0))

builtin "LAMBDA":
  example "(LET ((FN (LAMBDA (X) X))) (FN 5))"                     == "5"
  example "(LET ((FN (LAMBDA (X Y) (+ X Y)))) (FN 2 3))"           == "5"
  example "(LET ((X 5)) (LET ((FN (LAMBDA (Y) (+ X Y)))) (FN 3)))" == "8"
  example "(LET ((FN (LAMBDA () 42))) (FN))"                       == "42"
  example "(LET ((FN (LAMBDA (X) (+ X 1)))) (FN (FN (FN 0))))"     == "3"
  example "(LET ((ADD (LAMBDA (X Y) (+ X Y))) (MUL (LAMBDA (X Y) (* X Y)))) (ADD (MUL 2 3) 4))" == "10"

  `expectArgs>=N`(1)

  let lambdaArgs = args[0]
  for arg in lambdaArgs.expect(fkList).values:
    discard arg.expect(fkSym)

  let body = newListForm(args[1..^1])

  return newListForm(@[
    newSymForm(";LAMBDA"), lambdaArgs, ctx.environment, body
  ])

proc funcall(ctx: IContext, funcForm: Form, form: Form): Form =
  let lambdaArgs = funcForm.values[1].values

  if form.values.len - 1 != lambdaArgs.len:
    raise newException(ValueError, formatError(
      form.filename, form.line, form.col,
      "arguments mismatch (expected $1)", lambdaArgs.len
    ))

  var argValues = newSeq[Form](lambdaArgs.len)

  for i in 0 ..< lambdaArgs.len:
    argValues[i] = ctx.eval(form.values[1 + i])

  let env = ctx.environment
  ctx.environment = funcForm.values[2]
  ctx.pushEnv()

  try:
    for i, argName in lambdaArgs:
      ctx.newSym(argName.symValue, argValues[i])
    for value in funcForm.values[3].values:
      result = ctx.eval(value)
  finally:
    ctx.environment = env

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

  let symForm = ctx.eval(form.values[0])

  if symForm.kind != fkList or symForm.values[0].kind != fkSym:
    raise newException(ValueError, formatError(
      form.filename, form.line, form.col,
      "Cannot call $1", if symForm.kind != fkList: form.values[0].kind else: symForm.values[0].kind
    ))

  let ty = symForm.values[0].symValue

  if ty == ";BUILTIN":
    return builtinDispatcher[form.values[0].symValue](ctx, form.values[1..^1])

  elif ty == ";LAMBDA":
    return ctx.funcall(symForm, form)

  return newListForm(@[])

when isMainModule:
  testExamples()