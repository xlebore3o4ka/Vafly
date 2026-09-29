import std/[tables, macros, hashes, strutils, strformat]

type
  FormKind* = enum
    fkMap = "map"
    fkInt = "int"
    fkSym = "symbol"
    fkStr = "string"
    fkErr = "error"
    fkNil = "nil"

  LocationData* = object
    filename*: ref string
    line*: Positive = 1
    col*: Positive = 1

  InternmentData* = ref object
    bindings*:  Table[string, int]
    interning*: seq[string]

  Form* = ref object
    locationData*: LocationData
    case      kind*:        FormKind
    of fkSym: symInterned*: int
    of fkInt: intValue*:    int
    of fkMap:  
              posValue*:    seq[Form]
              mapValue*:    OrderedTable[Form, Form]
    of fkStr: strValue*:    string
    of fkErr:   
              errSym*:      Form
              errMsg*:      string
              errTrace*:    Form
    of fkNil: discard

proc newInternmentData*(): InternmentData

proc hash*(f: Form): Hash =
  result = result !& hash(ord(f.kind))
  case f.kind
  of fkSym: result = result !& hash(f.symInterned)
  of fkInt: result = result !& hash(f.intValue)
  of fkMap:
    for v in f.posValue: 
      result = result !& hash(v)
    for k, v in f.mapValue:
      result = result !& hash(k)
      result = result !& hash(v)
  of fkStr: result = result !& hash(f.strValue)
  of fkErr: 
    result = result !& hash(f.errSym)
    result = result !& hash(f.errMsg)
  of fkNil: discard
  result = !$result

proc `==`*(a, b: Form): bool =
  if a.isNil() or b.isNil():
    return a.isNil() and b.isNil()
  if a.kind != b.kind:
    return false
  case a.kind
  of fkNil: return true
  of fkSym: return a.symInterned == b.symInterned
  of fkInt: return a.intValue    == b.intValue
  of fkStr: return a.strValue    == b.strValue
  of fkErr:
    return a.errSym == b.errSym and a.errMsg == b.errMsg
  of fkMap:
    if a.posValue.len != b.posValue.len: return false
    if a.mapValue.len != b.mapValue.len: return false
    for n, f in a.posValue:
      if b.posValue[n] != f: return false
    for k, v in a.mapValue:
      if not b.mapValue.hasKey(k): return false
      if b.mapValue[k] != v:       return false
    return true

proc toBool*(form: Form): bool =
  case form.kind
  of fkNil: false
  of fkMap: form.posValue.len != 0 or form.mapValue.len != 0
  of fkInt: form.intValue != 0
  of fkStr: form.strValue.len != 0
  of fkSym: false
  of fkErr: false

proc intern*(data: InternmentData, original: string): int =
  let name = original.toUpper()

  if data.bindings.hasKey(name):
    result = data.bindings[name]
  else:
    result = data.interning.len
    data.interning.add(name)
    data.bindings[name] = result

template unintern*(data: InternmentData, intern: int): string =
  data.interning[intern]

proc errFormToStr*(text: string, err: Form, data: InternmentData): string =
  let filename = if err.locationData.filename != nil: err.locationData.filename[] else: "?"
  let col      = err.locationData.col
  let line     = err.locationData.line
  let message  = err.errMsg
  let lexeme   = text.split("\n")[line - 1]
  let errkind  = data.unintern(err.errSym.symInterned)

  for name in err.errTrace.posValue:
    let loc = name.locationData
    let locFile = loc.filename[]
    let locLine = loc.line
    let locCol  = loc.col
    let locStr  = fmt"{locFile}({locLine}:{locCol})"

    result = locStr & " ".repeat(max(30 - locStr.len, 1)) & "in " & data.unintern(name.symInterned) & "\n" & result

  result &= fmt"{filename}({line}:{col}) {errkind} {message}" & "\n"
  result &= "  |\n"
  result &= "  |  " & lexeme & "\n"
  result &= "  ? " & " ".repeat(col) & "^"

var requiredInterns*  = InternmentData()
let `PARSER-KEYWORD`* = requiredInterns.intern(";KEYWORD")
let `PARSER-QUOTE`*   = requiredInterns.intern("QUOTE")

let `ENV-CURRENT`*    = requiredInterns.intern(";ENV-CURRENT")
let `ENV-PARENT`*     = requiredInterns.intern(";ENV-PARENT")

let `EVAL-BUILTIN`*   = requiredInterns.intern(";BUILTIN")
let `EVAL-T-LAMBDA`*  = requiredInterns.intern(";LAMBDA")
let `EVAL-PARAM`*     = requiredInterns.intern(";PARAM")
let `EVAL-FUNC`*      = requiredInterns.intern(";FUNC")

let `EVAL+`*          = requiredInterns.intern("+")
let `EVAL-`*          = requiredInterns.intern("-")
let `EVAL*`*          = requiredInterns.intern("*")
let `EVAL-DIV`*       = requiredInterns.intern("DIV")
let `EVAL-MOD`*       = requiredInterns.intern("MOD")
let `EVAL-EQ`*        = requiredInterns.intern("EQ")
let `EVAL-NEQ`*       = requiredInterns.intern("NEQ")
let `EVAL>`*          = requiredInterns.intern(">")
let `EVAL<`*          = requiredInterns.intern("<")
let `EVAL>=`*         = requiredInterns.intern(">=")
let `EVAL<=`*         = requiredInterns.intern("<=")
let `EVAL-AND`*       = requiredInterns.intern("AND")
let `EVAL-OR`*        = requiredInterns.intern("OR")
let `EVAL-ALL`*       = requiredInterns.intern("ALL")
let `EVAL-ANY`*       = requiredInterns.intern("ANY")
let `EVAL-GET`*       = requiredInterns.intern("GET")
let `EVAL-LOCAL`*     = requiredInterns.intern("LOCAL")
let `EVAL-SET`*       = requiredInterns.intern("SET")
let `EVAL-IF`*        = requiredInterns.intern("IF")
let `EVAL-COND`*      = requiredInterns.intern("COND")
let `EVAL-LET`*       = requiredInterns.intern("LET")
let `EVAL-LAMBDA`*    = requiredInterns.intern("LAMBDA")
let `EVAL-TRY`*       = requiredInterns.intern("TRY")
let `EVAL-DEFUN`*     = requiredInterns.intern("DEFUN")
let `EVAL-ECHO`*      = requiredInterns.intern("ECHO")
let `EVAL-LEN`*       = requiredInterns.intern("LEN")
let `EVAL-EVAL`*      = requiredInterns.intern("EVAL")
let `EVAL-BUILD`*     = requiredInterns.intern("BUILD")
let `EVAL-APPLY`*     = requiredInterns.intern("APPLY")

let `ERR-PARSER-ERROR`*    = requiredInterns.intern("ERR-PARSER-ERROR!")
let `ERR-UNBOUND-SYMBOL`*  = requiredInterns.intern("ERR-UNBOUND-SYMBOL!")
let `ERR-TYPE-MISMATCH`*   = requiredInterns.intern("ERR-TYPE-MISMATCH!")
let `ERR-ARGS-MISMATCH`*   = requiredInterns.intern("ERR-ARGS-MISMATCH!")
let `ERR-CANNOT-CALL`*     = requiredInterns.intern("ERR-CANNOT-CALL!")
let `ERR-KEY-ERROR`*       = requiredInterns.intern("ERR-KEY-ERROR!")
let `ERR-ZERO-DIVISION`*   = requiredInterns.intern("ERR-ZERO-DIVISION!")
let `ERR-DUPLICATE-PARAM`* = requiredInterns.intern("ERR-DUPLICATE-PARAM!")

proc newInternmentData*(): InternmentData =
  result = InternmentData(
    bindings: initTable[string, int](),
    interning: @[]
  )
  result.bindings = requiredInterns.bindings
  result.interning = requiredInterns.interning

macro variantWithLocation(name, signature, body: untyped): untyped =
  expectKind(signature, nnkProcTy)
  let pubName = nnkPostfix.newTree(ident"*", name)

  let fp1 = signature[0].copyNimTree
  var fp2 = fp1.copyNimTree
  fp2.insert(1, nnkIdentDefs.newTree(
    ident"locationDataSym", ident"LocationData", newEmptyNode()))

  let tmp = ident("frmTmp")
  let withLoc = nnkStmtListExpr.newTree(
    nnkBlockStmt.newTree(
      newEmptyNode(),
      nnkStmtList.newTree(
        nnkVarSection.newTree(nnkIdentDefs.newTree(
          tmp, newEmptyNode(), body.copyNimTree)),
        newAssignment(nnkDotExpr.newTree(tmp, ident"locationData"),
                      ident"locationDataSym"),
        tmp)))

  template mk(fp, stmts: NimNode): untyped =
    nnkTemplateDef.newTree(pubName, newEmptyNode(), newEmptyNode(), fp,
      newEmptyNode(), newEmptyNode(), stmts)

  result = newStmtList(
    mk(fp1, body),
    mk(fp2, withLoc))

variantWithLocation newMapForm, proc (): Form:
  Form(kind: fkMap, mapValue: initOrderedTable[Form, Form]())

variantWithLocation newMapForm, proc(mapValueArg: OrderedTable[Form, Form]): Form:
  Form(kind: fkMap, mapValue: mapValueArg)

variantWithLocation newMapForm, proc(mapValueArg: seq[(Form, Form)]): Form:
  Form(kind: fkMap, mapValue: mapValueArg.toOrderedTable)

variantWithLocation newMapForm, proc(posValueArg: seq[Form]): Form:
  Form(kind: fkMap, posValue: posValueArg, mapValue: initOrderedTable[Form, Form]())

variantWithLocation newSymForm, proc(symInternedArg: int): Form:
  Form(kind: fkSym, symInterned: symInternedArg)

variantWithLocation newStrForm, proc(strValueArg: string): Form:
  Form(kind: fkStr, strValue: strValueArg)

variantWithLocation newIntForm, proc(intValueArg: int): Form:
  Form(kind: fkInt, intValue: intValueArg)

variantWithLocation newErrForm, proc(errSymArg: Form, errMsgArg: string = "",
                                     errTraceArg: Form = newMapForm()): Form:
  Form(kind: fkErr, errSym: errSymArg, errMsg: errMsgArg, errTrace: errTraceArg)

variantWithLocation newNilForm, proc(): Form:
  Form(kind: fkNil)

proc toStr*(self: Form, data: InternmentData = newInternmentData()): string =
  case self.kind
  of fkNil: "nil"
  of fkSym:
    if self.symInterned >= 0 and self.symInterned < data.interning.len:
      data.interning[self.symInterned]
    else:
      "?" & $self.symInterned
  of fkInt:
    $self.intValue
  of fkMap:
    if newSymForm(`ENV-CURRENT`) in self.mapValue or newSymForm(`ENV-PARENT`) in self.mapValue:
      ";ENVIROMENT"
    else:
      var parts: seq[string] = @[]
      for form in self.posValue:
        parts.add(form.toStr(data))
      for key, val in self.mapValue:
        parts.add((key.toStr(data) & ": ") & val.toStr(data))
      if parts.len == 0: "()" else: "(" & parts.join(" ") & ")"
  of fkStr:
    "\"" & self.strValue & "\""
  of fkErr:
    let loc = self.locationData
    let filename = if loc.filename != nil: loc.filename[] else: "?"
    "err(" & self.errSym.toStr(data) & ", " & self.errMsg &
      " @" & filename & ":" & $loc.line & ":" & $loc.col & ")"

template get*(map: Form, index: int): Form =
  assert map.kind == fkMap, "get: not a map"
  assert index >= 0 and index < map.posValue.len, "get: index out of range"
  map.posValue[index]

proc get*(map: Form, key: Form): Form =
  assert map.kind == fkMap, "get: not a map"
  if key.kind == fkInt and key.intValue >= 0 and key.intValue < map.posValue.len:
    map.posValue[key.intValue]
  else:
    assert map.mapValue.hasKey(key), "get: key not found"
    map.mapValue[key]

proc append*(map: Form, value: Form) =
  assert map.kind == fkMap, "append: not a map"
  let key = newIntForm(map.posValue.len)
  if map.mapValue.hasKey(key):
    map.mapValue.del(key)
  map.posValue.add(value)

proc hasKey*(map: Form, index: int): bool =
  assert map.kind == fkMap, "hasKey: not a map"
  index >= 0 and index < map.posValue.len

proc hasKey*(map: Form, key: Form): bool =
  assert map.kind == fkMap, "hasKey: not a map"
  if key.kind == fkInt and key.intValue >= 0 and key.intValue < map.posValue.len:
    true
  else:
    map.mapValue.hasKey(key)

proc put*(map: Form, key: Form, value: Form) =
  assert map.kind == fkMap, "put: not a map"
  if key.kind == fkInt and key.intValue >= 0 and key.intValue < map.posValue.len:
    map.posValue[key.intValue] = value
  else:
    map.mapValue[key] = value

proc len*(map: Form): int =
  assert map.kind == fkMap, "len: not a map"
  map.posValue.len + map.mapValue.len