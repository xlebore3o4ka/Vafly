import std/[tables, macros, hashes, strutils]

type
  FormKind* = enum
    fkMap = "map"
    fkInt = "int"
    fkSym = "symbol"
    fkStr = "string"
    fkErr = "error"

  LocationData* = object
    filename*: ref string
    line*: Positive = 1
    col*: Positive = 1

  InternmentData* = ref object
    bindings*:  Table[string, int]
    interning*: seq[string]

  Form* = ref object
    locationData*: LocationData
    case        kind*:       FormKind
    of fkSym:   symInterned*: int
    of fkInt:   intValue*:   int
    of fkMap:  
      case      mapIsNil*:   bool = true
      of false: mapValue*:   OrderedTable[Form, Form]
      else:     discard
    of fkStr:   strValue*:   string
    of fkErr:   
                errSym*:     Form
                errMsg*:     string

proc newInternmentData*(): InternmentData

proc toStr*(self: Form, data: InternmentData = newInternmentData()): string =
  case self.kind
  of fkSym:
    if self.symInterned >= 0 and self.symInterned < data.interning.len:
      data.interning[self.symInterned]
    else:
      "?" & $self.symInterned
  of fkInt:
    $self.intValue
  of fkMap:
    if self.mapIsNil:
      "nil"
    else:
      var parts: seq[string] = @[]
      for key, val in self.mapValue:
        parts.add((if key.kind != fkInt: key.toStr(data) & ": " else: "") & val.toStr(data))
      if parts.len == 0: "()" else: "(" & parts.join(" ") & ")"
  of fkStr:
    "\"" & self.strValue & "\""
  of fkErr:
    "err(" & self.errSym.toStr(data) & ", " & self.errMsg & ")"

proc hash*(f: Form): Hash =
  result = result !& hash(ord(f.kind))
  case f.kind
  of fkSym: result = result !& hash(f.symInterned)
  of fkInt: result = result !& hash(f.intValue)
  of fkMap:
    result = result !& hash(f.mapIsNil)
    if not f.mapIsNil:
      for k, v in f.mapValue:
        result = result !& hash(k)
        result = result !& hash(v)
  of fkStr: result = result !& hash(f.strValue)
  of fkErr: 
    result = result !& hash(f.errSym)
    result = result !& hash(f.errMsg)
  result = !$result

proc `==`*(a, b: Form): bool =
  if a.isNil() or b.isNil():
    return a.isNil() and b.isNil()
  if a.kind != b.kind:
    return false
  case a.kind
  of fkSym: return a.symInterned == b.symInterned
  of fkInt: return a.intValue    == b.intValue
  of fkStr: return a.strValue    == b.strValue
  of fkErr:
    return a.errSym == b.errSym and a.errMsg == b.errMsg
  of fkMap:
    if a.mapIsNil != b.mapIsNil: return false
    if a.mapIsNil: return true
    if a.mapValue.len != b.mapValue.len: return false
    for k, v in a.mapValue:
      if not b.mapValue.hasKey(k): return false
      if b.mapValue[k] != v:       return false
    return true

proc toBool*(form: Form): bool =
  case form.kind
  of fkMap:  not form.mapIsNil and form.mapValue.len != 0
  of fkInt:  form.intValue != 0
  of fkStr:  form.strValue.len != 0
  of fkSym:  false
  of fkErr:  false

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

var requiredInterns*  = InternmentData()
let `PARSER-KEYWORD`* = requiredInterns.intern(";KEYWORD")
let `PARSER-QUOTE`*   = requiredInterns.intern("QUOTE")

let `ENV-CURRENT`*    = requiredInterns.intern(";ENV-CURRENT")
let `ENV-PARENT`*     = requiredInterns.intern(";ENV-PARENT")

let `EVAL-BUILTIN`*   = requiredInterns.intern(";BUILTIN")
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
let `EVAL-QUOTE`*     = requiredInterns.intern("QUOTE")
let `EVAL-GET`*       = requiredInterns.intern("GET")

let `ERR-PARSER-ERROR`*   = requiredInterns.intern("ERR-PARSER-ERROR!")
let `ERR-UNBOUND-SYMBOL`* = requiredInterns.intern("ERR-UNBOUND-SYMBOL!")
let `ERR-TYPE-MISMATCH`*  = requiredInterns.intern("ERR-TYPE-MISMATCH!")
let `ERR-ARGS-MISMATCH`*  = requiredInterns.intern("ERR-ARGS-MISMATCH!")
let `ERR-CANNOT-CALL`*    = requiredInterns.intern("ERR-CANNOT-CALL!")
let `ERR-KEY-ERROR`*      = requiredInterns.intern("ERR-KEY-ERROR!")

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
    nnkVarSection.newTree(nnkIdentDefs.newTree(
      tmp, newEmptyNode(), body.copyNimTree)),
    newAssignment(nnkDotExpr.newTree(tmp, ident"locationData"),
                  ident"locationDataSym"),
    tmp)

  template mk(fp, stmts: NimNode): untyped =
    nnkTemplateDef.newTree(pubName, newEmptyNode(), newEmptyNode(), fp,
      newEmptyNode(), newEmptyNode(), stmts)

  result = newStmtList(
    mk(fp1, body),
    mk(fp2, withLoc))


variantWithLocation newMapForm, proc (isNilArg: bool = true): Form:
  Form(kind: fkMap, mapIsNil: isNilArg)

variantWithLocation newMapForm, proc(mapValueArg: OrderedTable[Form, Form]): Form:
  Form(kind: fkMap, mapIsNil: false, mapValue: mapValueArg)

variantWithLocation newMapForm, proc(mapValueArg: seq[(Form, Form)]): Form:
  Form(kind: fkMap, mapIsNil: false, mapValue: mapValueArg.toOrderedTable)

template append*(mapValue: OrderedTable[Form, Form], value: Form) =
  var maxIdx = -1
  for k in mapValue.keys:
    if k.kind == fkInt:
      maxIdx = max(maxIdx, k.intValue)
  mapValue[newIntForm(maxIdx + 1)] = value

template at*(mapValue: OrderedTable[Form, Form], index: int): Form =
  mapValue[newIntForm(index)]

template atOrErr*(mapValue: OrderedTable[Form, Form], index: int, name: string = "?" & $index): Form =
  mapValue.getOrDefault(newIntForm(index), newErrForm(newSymForm(`ERR-UNBOUND-SYMBOL`), 
    "The symbol " & name & " has never been bound to any value"))

variantWithLocation newSymForm, proc(symInternedArg: int): Form:
  Form(kind: fkSym, symInterned: symInternedArg)

variantWithLocation newStrForm, proc(strValueArg: string): Form:
  Form(kind: fkStr, strValue: strValueArg)

variantWithLocation newIntForm, proc(intValueArg: int): Form:
  Form(kind: fkInt, intValue: intValueArg)


variantWithLocation newErrForm, proc(errSymArg: Form, errMsgArg: string = ""): Form:
  Form(kind: fkErr, errSym: errSymArg, errMsg: errMsgArg)