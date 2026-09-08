import std/[strutils, sequtils]

type
  FormKind* = enum
    nkSym
    nkInt
    nkList
    nkStr

  Form* = ref object
    case kind*: FormKind
    of nkSym:  
      original*: string
      symValue*: string
    of nkInt:  intValue*: int
    of nkList: values*:   seq[Form]
    of nkStr:  strValue*: string
    filename*: string
    line*, col*: int

  Context* = ref object
    text*, filename*: string
    pos*:  int
    col*:  int
    line*: int
    parentheses*: seq[tuple[filename: string, col: int, line: int]]

proc newSymForm*(original: string): Form =
  Form(kind: nkSym, symValue: original.toUpper(), original: original)

proc newIntForm*(intValue: int): Form =
  Form(kind: nkInt, intValue: intValue)

proc newListForm*(values: seq[Form]): Form =
  Form(kind: nkList, values: values)

proc newStrForm*(strValue: string): Form =
  Form(kind: nkStr, strValue: strValue)

proc `$`*(form: Form): string =
  case form.kind:
  of nkSym: return form.symValue
  of nkInt: return $form.intValue
  of nkList:
    result = "(" & form.values.mapIt($it).join(" ")
    result &= ")"
  of nkStr: return form.strValue.repr

template peek(ctx: Context): char =
  if ctx.pos in 0..<ctx.text.len: ctx.text[ctx.pos] else: '\0'

template advance(ctx: Context, n: int = 1) =
  for _ in 0..<n:
    if ctx.pos < ctx.text.len:
      if ctx.text[ctx.pos] == '\n':
        inc ctx.line
        ctx.col = 1
      else:
        inc ctx.col
      inc ctx.pos

template next(ctx: Context): char =
  let c = ctx.peek()
  if c != '\0':
    ctx.advance()
  c

template isDigit(c: char): bool =
  c >= '0' and c <= '9'

template isSym*(c: char): bool =
  c notin " \0\t\r\n;\"()':"

proc parseForm*(ctx: Context): Form

proc parse*(ctx: Context): Form =
  result = Form(kind: nkList, values: @[], filename: ctx.filename, col: ctx.col, line: ctx.line)

  while ctx.peek() notin ")\0":

    let form = ctx.parseForm()
    if form == nil: break
    result.values.add(form)

proc parseForm*(ctx: Context): Form =
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
    var buffer: string
    c = ctx.next()

    while c != '"' and c != '\0':
      buffer.add(c)

      c = ctx.next()

    if c == '"':
      result = newStrForm(buffer.unescape)
    else:
      raise newException(ValueError, "$1:$2:$3: unterminated string literal" % [
        ctx.filename, $ctx.line, $ctx.col
      ])

  elif c == '(':
    ctx.parentheses.add((filename: ctx.filename, col: ctx.col, line: ctx.line))
    
    result = ctx.parse()
    
    if ctx.parentheses.len == 0: 
      raise newException(ValueError, "$1:$2:$3: unmatched ')'" % [
        ctx.filename, $ctx.line, $ctx.col
      ])
    discard ctx.parentheses.pop()
    ctx.advance()

  elif c == '\'':
    let form = ctx.parseForm()

    if form == nil:
      raise newException(ValueError, "$1:$2:$3: expected form after '\\''" % [
        ctx.filename, $ctx.line, $ctx.col
      ])

    result = newListForm(@[
      newSymForm("quote"),
      form
    ])

  elif c == ':':
    let sym = ctx.parseForm()

    if sym == nil:
      raise newException(ValueError, "$1:$2:$3: expected symbol after ':'" % [
        ctx.filename, $ctx.line, $ctx.col
      ])
    if sym.kind != nkSym:
      raise newException(ValueError, "$1:$2:$3: expected symbol, got $1" % [
        ctx.filename, $ctx.line, $ctx.col, $sym.kind
      ])

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
  var ctx = Context(text: text, filename: filename, pos: 0, col: 0, line: 0, parentheses: @[])

  result = parse(ctx)

  if ctx.parentheses.len != 0:
    let (f, c, l) = ctx.parentheses.pop()
    raise newException(ValueError, "$1:$2:$3: unclosed '('" % [
      f, $l, $c
    ])