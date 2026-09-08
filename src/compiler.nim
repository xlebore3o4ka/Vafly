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

template peek(text: string; pos: int): char =
  if pos in 0..<text.len: text[pos] else: '\0'

template advance(text: string; pos: var int; col: var int; line: var int; n: int = 1) =
  for _ in 0..<n:
    if pos < text.len:
      if text[pos] == '\n':
        inc line
        col = 1
      else:
        inc col
      inc pos

template next(text: string; pos: var int; col: var int; line: var int): char =
  let c = text.peek(pos)
  if c != '\0':
    text.advance(pos, col, line)
  c

template isDigit(c: char): bool =
  c >= '0' and c <= '9'

template isSym*(c: char): bool =
  c notin " \0\t\r\n;\"()':"

proc parseForm*(text, filename: string; pos: var int; col: var int; line: var int): Form

proc parse*(text, filename: string; pos: var int; col: var int; line: var int): Form =
  result = Form(kind: nkList, values: @[], filename: filename, col: col, line: line)

  var parenthes: tuple[exists: bool, filename: string, col: int, line: int]

  while (let c = text.peek(pos); c) notin "\0":

    if c == '(': parenthes = (exists: true, filename: filename, col: col, line: line)
    elif c == ')':
      if not parenthes.exists: 
        raise newException(ValueError, "$1:$2:$3: unmatched ')'" % [
          filename, $line, $col
        ])
      parenthes.exists = false

    let form = text.parseForm(filename, pos, col, line)
    if form == nil: break
    result.values.add(form)

  if parenthes.exists:
    let (_, f, c, l) = parenthes
    raise newException(ValueError, "$1:$2:$3: unclosed '('" % [
      f, $c, $l
    ])

proc parseForm*(text, filename: string; pos: var int; col: var int; line: var int): Form =
  while text.peek(pos) in " \t\r\n":
    text.advance(pos, col, line)

  if text.peek(pos) == ';':
    while text.peek(pos) notin "\n\0":
      text.advance(pos, col, line)

    if text.peek(pos) == '\n':
      text.advance(pos, col, line)

    return text.parseForm(filename, pos, col, line)

  if text.peek(pos) in "\0)":
    return nil

  let formLine = line
  let formCol = col
  var c = text.next(pos, col, line)

  if c.isDigit() or (c == '-' and text.peek(pos).isDigit()):
    var buffer: string
    buffer.add(c)

    while true:
      let nextC = text.peek(pos)

      if nextC.isDigit():
        buffer.add(nextC)
        text.advance(pos, col, line)
      else: break

    result = newIntForm(buffer.parseInt())

  elif c == '"':
    var buffer: string
    c = text.next(pos, col, line)

    while c != '"' and c != '\0':
      buffer.add(c)

      c = text.next(pos, col, line)

    if c == '"':
      result = newStrForm(buffer.unescape)
    else:
      raise newException(ValueError, "$1:$2:$3: unterminated string literal" % [
        filename, $line, $col
      ])

  elif c == '(':
    result = text.parse(filename, pos, col, line)
    text.advance(pos, col, line)

  elif c == '\'':
    let form = text.parseForm(filename, pos, col, line)

    if form == nil:
      raise newException(ValueError, "$1:$2:$3: expected form after '\\''" % [
        filename, $line, $col
      ])

    result = newListForm(@[
      newSymForm("quote"),
      form
    ])

  elif c == ':':
    let sym = text.parseForm(filename, pos, col, line)

    if sym == nil:
      raise newException(ValueError, "$1:$2:$3: expected symbol after ':'" % [
        filename, $line, $col
      ])
    if sym.kind != nkSym:
      raise newException(ValueError, "$1:$2:$3: expected symbol, got $1" % [
        filename, $line, $col, $sym.kind
      ])

    result = newListForm(@[
      newSymForm("keyword"),
      sym
    ])

  elif c.isSym():
    var buffer: string
    buffer.add(c)

    while true:
      let nextC = text.peek(pos)

      if nextC.isSym():
        buffer.add(nextC)
        text.advance(pos, col, line)

      else:
        break

    result = newSymForm(buffer)

  result.filename = filename
  result.line = formLine
  result.col = formCol

proc parse*(text, filename: string): Form =
  var
    pos = 0
    col = 1
    line = 1

  text.parse(filename, pos, col, line)
