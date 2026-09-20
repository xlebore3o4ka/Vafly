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
  result &= "  | " & " ".repeat(col) & "_" & "\n"
  result &= "  |  " & lexeme

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

  locationData.newSymForm(self.internmentData.intern(buffer))

proc skipWhitespaces(self: Parser) =
  while (let c = self.peek(); c) in " \t\r\n":
    if c == '\n':
      self.offset.inc
      self.locationData.col = 1
      self.locationData.line.inc
    else: 
      self.advance()

template parseMap(self: Parser, locationDataArg: LocationData): Form =
  let temp = locationDataArg.newMapForm(false)

  while (let c = self.peek(); c) notin ")\0":
    let form = self.parseForm()

    if form.isNil():
      break

    let kwform = if form.kind == fkMap and form.mapValue.len == 2: form.mapValue.at(0) else: nil
    let kwsym  = if kwform != nil and kwform.kind == fkMap and kwform.mapValue.len == 2: kwform.mapValue.at(0) else: nil
    let isKeywordPair = kwsym != nil and kwsym.kind == fkSym and kwsym.symInterned == `PARSER-KEYWORD`

    if isKeywordPair:
      temp.mapValue[kwform.mapValue.at(1)] = form.mapValue.at(1)
    else:
      temp.mapValue.append(form)

  self.skipWhitespaces()

  if self.peek() == ')':
    if self.parentheses.len == 0: 
      raise self.locationData.newParserError("Unmatched ')'")

    self.advance()
    discard self.parentheses.pop()

  temp

template parseQuote(self: Parser, locationData: LocationData): Form =
  let temp = locationData.newMapForm(false)
  temp.mapValue.append(newSymForm(`PARSER-QUOTE`))

  let form = self.parseForm()
  if form.isNil():
    raise locationData.newParserError("Expected form after '\\''")

  temp.mapValue.append(form)
  temp

template parseKeyword(self: Parser, locationData: LocationData): Form =
  let temp = locationData.newMapForm(false)
  temp.mapValue.append(newSymForm(`PARSER-KEYWORD`))

  let form = self.parseForm()
  if form.isNil():
    raise locationData.newParserError("Expected form after ':'")

  temp.mapValue.append(form)
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
    return self.parseKeyword(loc)

  elif c.isSym():
    return self.parseSymbol(loc, c)

proc parse(self: Parser, loc: LocationData = self.locationData): Form =
  result = loc.newMapForm(false)

  while self.peek() notin "\0":
    let form = self.parseForm()
    if form == nil: break
    result.mapValue.append(form)

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
    return newErrForm(newSymForm(`ERR-PARSER-ERROR`), e.message)