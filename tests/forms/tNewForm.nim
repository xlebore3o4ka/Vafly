import std/[unittest, tables]
import forms

suite "LocationData":
  test "locationData variant sets location":
    let loc = LocationData(line: 5, col: 10)
    let m = newMapForm(loc)
    let s = newSymForm(loc, 1)
    let i = newIntForm(loc, 2)
    let t = newStrForm(loc, "x")
    let n = newNilForm(loc)
    let e = newErrForm(loc, newSymForm(1), "boom")
    check:
      m.locationData.line == 5
      s.locationData.line == 5
      i.locationData.line == 5
      t.locationData.line == 5
      n.locationData.line == 5
      e.locationData.line == 5
      m.locationData.col == 10

  test "locationData default is (nil, 1, 1)":
    let m = newMapForm()
    check:
      m.locationData.filename == nil
      m.locationData.line == 1
      m.locationData.col == 1

suite "newForm constructors": 
  test "newMapForm(): empty map":
    let m = newMapForm()
    check:
      m.kind == fkMap
      m.posValue.len == 0
      m.mapValue.len == 0

  test "newMapForm(OrderedTable)":
    let m = newMapForm(@{newSymForm(1): newIntForm(10)})
    check:
      m.kind == fkMap
      m.posValue.len == 0
      m.mapValue.len == 1
      m.mapValue[newSymForm(1)] == newIntForm(10)

  test "newMapForm(seq[(Form, Form)])":
    let m = newMapForm(@[(newSymForm(1), newIntForm(10)),
                         (newSymForm(2), newIntForm(20))])
    check:
      m.kind == fkMap
      m.posValue.len == 0
      m.mapValue.len == 2
      m.mapValue[newSymForm(1)] == newIntForm(10)
      m.mapValue[newSymForm(2)] == newIntForm(20)

  test "newMapForm(seq[Form])":
    let m = newMapForm(@[newIntForm(1), newIntForm(2)])
    check:
      m.kind == fkMap
      m.posValue.len == 2
      m.mapValue.len == 0
      m.posValue[0] == newIntForm(1)
      m.posValue[1] == newIntForm(2)

  test "newMapForm(seq[Form], seq[(Form, Form)])":
    let m = newMapForm(@[newIntForm(1)],
                       @[(newSymForm(2), newIntForm(20))])
    check:
      m.kind == fkMap
      m.posValue.len == 1
      m.mapValue.len == 1
      m.posValue[0] == newIntForm(1)
      m.mapValue[newSymForm(2)] == newIntForm(20)

  test "newSymForm":
    let s = newSymForm(42)
    check:
      s.kind == fkSym
      s.symInterned == 42

  test "newStrForm":
    let s = newStrForm("hello")
    check:
      s.kind == fkStr
      s.strValue == "hello"

  test "newStrForm empty":
    let s = newStrForm("")
    check:
      s.kind == fkStr
      s.strValue == ""

  test "newIntForm":
    let i = newIntForm(7)
    check:
      i.kind == fkInt
      i.intValue == 7

  test "newIntForm negative":
    let i = newIntForm(-7)
    check:
      i.kind == fkInt
      i.intValue == -7

  test "newErrForm defaults":
    let e = newErrForm(newSymForm(1))
    check:
      e.kind == fkErr
      e.errSym == newSymForm(1)
      e.errMsg == ""
      e.errTrace.kind == fkMap
      e.errTrace.posValue.len == 0

  test "newErrForm with msg":
    let e = newErrForm(newSymForm(1), "boom")
    check:
      e.errMsg == "boom"
      e.errTrace.posValue.len == 0

  test "newErrForm with trace":
    let tr = newMapForm(@[newSymForm(2)])
    let e = newErrForm(newSymForm(1), "boom", tr)
    check:
      e.errTrace == tr

  test "newNilForm":
    let n = newNilForm()
    check:
      n.kind == fkNil