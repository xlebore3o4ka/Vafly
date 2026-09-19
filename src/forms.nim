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
      case      isNil*:      bool = true
      of false: mapValue*:   OrderedTable[Form, Form]
      else:     discard
    of fkStr:   strValue*:   string
    of fkErr:   
                errSym*:     Form
                errMsg*:     string

proc toStr*(self: Form, data: InternmentData = InternmentData()): string =
  case self.kind
  of fkSym:
    if self.symInterned >= 0 and self.symInterned < data.interning.len:
      data.interning[self.symInterned]
    else:
      "?" & $self.symInterned
  of fkInt:
    $self.intValue
  of fkMap:
    if self.isNil:
      "nil"
    else:
      var parts: seq[string] = @[]
      for key, val in self.mapValue:
        parts.add((if key.kind != fkInt: key.toStr(data) & ": " else: "") & val.toStr(data))
      if parts.len == 0: "nil" else: "(" & parts.join(" ") & ")"
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
    result = result !& hash(f.isNil)
    if not f.isNil:
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
    if a.isNil != b.isNil: return false
    if a.isNil: return true
    if a.mapValue.len != b.mapValue.len: return false
    for k, v in a.mapValue:
      if not b.mapValue.hasKey(k): return false
      if b.mapValue[k] != v:       return false
    return true

proc intern*(data: InternmentData, original: string): int =
  let name = original.toUpper()

  if data.bindings.hasKey(name):
    result = data.bindings[name]
  else:
    result = data.interning.len
    data.interning.add(name)
    data.bindings[name] = result

var requiredInterns*: InternmentData
let `ENV-CURRENT`*    = requiredInterns.intern(";ENV-CURRENT")
let `ENV-PARENT`*     = requiredInterns.intern(";ENV-PARENT")
let `PARSER-KEYWORD`* = requiredInterns.intern(";KEYWORD")
let `PARSER-QUOTE`*   = requiredInterns.intern("QUOTE")
let `PARSER-ERROR`*   = requiredInterns.intern("ParserError!")

proc newInternmentData(): InternmentData =
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
  Form(kind: fkMap, isNil: isNilArg)

variantWithLocation newMapForm, proc(mapValueArg: OrderedTable[Form, Form]): Form:
  Form(kind: fkMap, isNil: false, mapValue: mapValueArg)

variantWithLocation newMapForm, proc(mapValueArg: seq[(Form, Form)]): Form:
  Form(kind: fkMap, isNil: false, mapValue: mapValueArg.toOrderedTable)

template append*(mapValue: OrderedTable[Form, Form], value: Form) =
  var maxIdx = -1
  for k in mapValue.keys:
    if k.kind == fkInt:
      maxIdx = max(maxIdx, k.intValue)
  mapValue[newIntForm(maxIdx + 1)] = value

template at*(mapValue: OrderedTable[Form, Form], index: int): Form =
  mapValue[newIntForm(index)]


variantWithLocation newSymForm, proc(symInternedArg: int): Form:
  Form(kind: fkSym, symInterned: symInternedArg)

variantWithLocation newStrForm, proc(strValueArg: string): Form:
  Form(kind: fkStr, strValue: strValueArg)

variantWithLocation newIntForm, proc(intValueArg: int): Form:
  Form(kind: fkInt, intValue: intValueArg)


variantWithLocation newErrForm, proc(errSymArg: Form, errMsgArg: string = ""): Form:
  Form(kind: fkErr, errSym: errSymArg, errMsg: errMsgArg)