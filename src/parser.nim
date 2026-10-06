import std/[tables, macros, strutils, strformat]
import forms

type
  ParserError* = ref object of CatchableError
    message*:      string
    locationData*: LocationData

  Parser* = ref object
    text*:           string
    textlen*:        Natural
    offset*:         Natural
    internmentData*: InternmentData
    locationData*:   LocationData
    parentheses*:    seq[LocationData]

template newParserError(locationDataArg: LocationData, messageArg: string): ParserError =
  ParserError(locationData: locationDataArg, message: messageArg)

proc toStr(self: Parser, err: ParserError): string =
  let filename = err.locationData.filename[]
  let col      = err.locationData.col
  let line     = err.locationData.line
  let message  = err.message
  let lexeme   = self.text.split("\n")[line - 1]

  result = fmt"{filename}({line}:{col}) ParserError: {message}" & "\n"
  result &= "  |\n"
  result &= "  |  " & lexeme & "\n"
  result &= "  | " & " ".repeat(col) & "^"

template peek(self: Parser): char =
  if self.offset >= self.textlen: '\0'
  else: self.text[self.offset]

template advance(self: Parser) =
  self.offset.inc
  self.locationData.col.inc

template next(self: Parser): char =
  let temp = self.peek()
  advance(self)
  temp

template isDigit(c: char): bool =
  c >= '0' and c <= '9'

template isSym*(c: char): bool =
  c notin " \0\t\r\n;\"()':"

proc parse*(self: Parser, loc: LocationData = self.locationData): Form

proc parseForm(self: Parser): Form

template parseComment(self: Parser): Form =
  while self.peek() notin "\n\0":
    self.advance()

  self.parseForm()

template parseDigit(self: Parser, locationData: LocationData): Form =
  var buffer: string
  buffer.add(c)

  while true:
    let nextC = self.peek()

    if nextC.isDigit():
      buffer.add(nextC)
      self.advance()
    else: break

  locationData.newIntForm(buffer.parseInt())

template parseString(self: Parser, locationData: LocationData): Form =
  var buffer = "\""
  c = self.next()

  while c != '"' and c != '\0':
    if c == '\\' and self.peek() in "\"\\":
      c = self.next()

    buffer.add(c)

    c = self.next()

  buffer.add('"')

  if c == '"':
    locationData.newStrForm(buffer.unescape)
  else:
    raise locationData.newParserError("Unterminated string literal")

template parseSymbol(self: Parser, locationData: LocationData, c: char): Form =
  var buffer: string
  buffer.add(c)

  while true:
    let nextC = self.peek()
    
    if nextC.isSym():
      buffer.add(nextC)
      self.advance()
    else:
      break

  if buffer.toUpper() == "NIL":
    return locationData.newNilForm()

  locationData.newSymForm(self.internmentData.intern(buffer))

proc skipWhitespaces(self: Parser) =
  while (let c = self.peek(); c) in " \t\r\n":
    if c == '\n':
      self.offset.inc
      self.locationData.col = 1
      self.locationData.line.inc
    else: 
      self.advance()

proc isKeywordPair(form: Form): bool =
  if form.kind != fkMap: return false
  if not form.hasKey(0): return false
  if not form.hasKey(1): return false
  if form.posValue.len != 2: return false

  let head = form.get(0)
  if head.kind != fkMap: return false
  if not head.hasKey(0): return false
  if not head.hasKey(1): return false
  if head.posValue.len != 2: return false

  let headSym = head.get(0)
  headSym.kind == fkSym and headSym.symInterned == `PARSER-KEYWORD`

template parseMap(self: Parser, locationDataArg: LocationData): Form =
  let temp = locationDataArg.newMapForm()

  while (let c = self.peek(); c) notin ")\0":
    let form = self.parseForm()

    if form.isNil():
      break

    if form.isKeywordPair():
      temp.put(form.get(0).get(1), form.get(1))
    else:
      temp.append(form)

  self.skipWhitespaces()

  if self.peek() == ')':
    if self.parentheses.len == 0:
      raise self.locationData.newParserError("Unmatched ')'")

    self.advance()
    discard self.parentheses.pop()

  temp

template parseQuote(self: Parser, locationData: LocationData): Form =
  let temp = locationData.newMapForm()
  temp.append(locationData.newSymForm(`PARSER-QUOTE`))

  let form = self.parseForm()
  if form.isNil():
    raise locationData.newParserError("Expected form after '\\''")

  temp.append(form)
  temp

template parseKeyword(self: Parser, locationData: LocationData): Form =
  let temp = locationData.newMapForm()
  temp.append(locationData.newSymForm(`PARSER-KEYWORD`))

  let form = self.parseForm()
  if form.isNil():
    raise locationData.newParserError("Expected form after ':'")

  temp.append(form)
  temp

proc parseForm(self: Parser): Form =
  self.skipWhitespaces()

  if self.peek() == ';':
    return self.parseComment()

  if self.peek() in ")\0":
    return nil

  let loc = self.locationData

  var c = self.next()

  if c.isDigit() or (c in "-+" and self.peek().isDigit()):
    return self.parseDigit(loc)

  elif c == '"':
    return self.parseString(loc)

  elif c == '(':
    self.parentheses.add(loc)
    return self.parseMap(loc)

  elif c == '\'':
    return self.parseQuote(loc)

  elif c == ':':
    # TODO: if ')' after ':' then close all opened maps
    return self.parseKeyword(loc)

  elif c.isSym():
    return self.parseSymbol(loc, c)

proc parse(self: Parser, loc: LocationData = self.locationData): Form =
  result = loc.newMapForm()

  while self.peek() notin "\0":
    let form = self.parseForm()
    if form.isNil(): break
    result.append(form)

  if self.peek() == ')':
    raise self.locationData.newParserError("Unmatched ')'")

proc parse*(text, filename: string, internmentData: InternmentData = newInternmentData()): Form =
  let parser = Parser(
    text: text, textlen: text.len, 
    locationData: LocationData( filename: (let f = new(string); f[] = filename; f) ),
    internmentData: internmentData
  )
  try:
    result = parser.parse()

    if parser.parentheses.len > 0:
      raise parser.parentheses.pop().newParserError("unclosed '('")

  except ParserError as e:
    stderr.writeLine(parser.toStr(e))
    return e.locationData.newErrForm(newSymForm(`ERR-PARSER-ERROR`), e.message)