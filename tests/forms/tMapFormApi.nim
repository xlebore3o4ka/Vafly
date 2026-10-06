import std/[unittest, tables]
import forms

suite "MapForm API - get(int)":
  test "returns posValue by index":
    let m = newMapForm(@[newIntForm(10), newIntForm(20)])
    check:
      m.get(0) == newIntForm(10)
      m.get(1) == newIntForm(20)

  test "returns nil-form at hole":
    let m = newMapForm(@[newNilForm(), newIntForm(20)])
    check:
      m.get(0).kind == fkNil

  test "asserts on non-map":
    let i = newIntForm(1)
    expect AssertionDefect:
      discard i.get(0)

  test "asserts on out of range":
    let m = newMapForm(@[newIntForm(10)])
    expect AssertionDefect:
      discard m.get(1)
    expect AssertionDefect:
      discard m.get(-1)


suite "MapForm API - get(Form)":
  test "int key in posValue range":
    let m = newMapForm(@[newIntForm(10), newIntForm(20)])
    check:
      m.get(newIntForm(0)) == newIntForm(10)
      m.get(newIntForm(1)) == newIntForm(20)

  test "int key outside posValue falls to mapValue":
    let m = newMapForm(@[newIntForm(10)],
                       @[(newIntForm(5), newIntForm(50))])
    check:
      m.get(newIntForm(5)) == newIntForm(50)

  test "negative int key goes to mapValue":
    let m = newMapForm(@[], @[(newIntForm(-1), newIntForm(99))])
    check:
      m.get(newIntForm(-1)) == newIntForm(99)

  test "sym key in mapValue":
    let m = newMapForm(@[], @[(newSymForm(1), newIntForm(99))])
    check:
      m.get(newSymForm(1)) == newIntForm(99)

  test "str key in mapValue":
    let m = newMapForm(@[], @[(newStrForm("k"), newIntForm(99))])
    check:
      m.get(newStrForm("k")) == newIntForm(99)

  test "asserts on missing key":
    let m = newMapForm()
    expect AssertionDefect:
      discard m.get(newSymForm(1))

  test "asserts on non-map":
    let i = newIntForm(1)
    expect AssertionDefect:
      discard i.get(newSymForm(1))


suite "MapForm API - append":
  test "appends to empty map":
    let m = newMapForm()
    m.append(newIntForm(10))
    check:
      m.posValue.len == 1
      m.posValue[0] == newIntForm(10)

  test "appends to non-empty map":
    let m = newMapForm(@[newIntForm(1)])
    m.append(newIntForm(2))
    check:
      m.posValue.len == 2
      m.posValue[1] == newIntForm(2)

  test "shadow removal: appending over existing int key in mapValue":
    let m = newMapForm(@[], @[(newIntForm(0), newIntForm(99))])
    m.append(newIntForm(10))
    check:
      m.posValue.len == 1
      m.posValue[0] == newIntForm(10)
      not m.mapValue.hasKey(newIntForm(0))

  test "asserts on non-map":
    let i = newIntForm(1)
    expect AssertionDefect:
      i.append(newIntForm(2))


suite "MapForm API - hasKey(int)":
  test "true for valid posValue index":
    let m = newMapForm(@[newIntForm(10), newIntForm(20)])
    check:
      m.hasKey(0)
      m.hasKey(1)

  test "false for out of range":
    let m = newMapForm(@[newIntForm(10)])
    check:
      not m.hasKey(1)
      not m.hasKey(-1)


suite "MapForm API - hasKey(Form)":
  test "int key in posValue range":
    let m = newMapForm(@[newIntForm(10)])
    check:
      m.hasKey(newIntForm(0))
      not m.hasKey(newIntForm(1))

  test "int key in mapValue":
    let m = newMapForm(@[], @[(newIntForm(5), newIntForm(50))])
    check:
      m.hasKey(newIntForm(5))
      not m.hasKey(newIntForm(6))

  test "negative int key":
    let m = newMapForm(@[], @[(newIntForm(-1), newIntForm(99))])
    check:
      m.hasKey(newIntForm(-1))

  test "sym key":
    let m = newMapForm(@[], @[(newSymForm(1), newIntForm(99))])
    check:
      m.hasKey(newSymForm(1))
      not m.hasKey(newSymForm(2))


suite "MapForm API - put":
  test "put overwrites posValue in range":
    let m = newMapForm(@[newIntForm(1), newIntForm(2)])
    m.put(newIntForm(1), newIntForm(99))
    check:
      m.posValue[1] == newIntForm(99)
      m.posValue.len == 2

  test "put in [len, 2*len) extends posValue":
    let m = newMapForm(@[newIntForm(1), newIntForm(2)])
    m.put(newIntForm(3), newIntForm(99))
    check:
      m.posValue.len == 4
      m.posValue[3] == newIntForm(99)

  test "put in [len, 2*len) fills holes with nil":
    let m = newMapForm(@[newIntForm(1)])
    m.put(newIntForm(1), newIntForm(99))
    check:
      m.posValue.len == 2
      m.posValue[1] == newIntForm(99)

  test "put beyond 2*len goes to mapValue":
    let m = newMapForm(@[newIntForm(1)])
    m.put(newIntForm(10), newIntForm(99))
    check:
      m.posValue.len == 1
      m.mapValue[newIntForm(10)] == newIntForm(99)

  test "put beyond 2*len on empty map goes to mapValue":
    let m = newMapForm()
    m.put(newIntForm(1), newIntForm(99))
    check:
      m.posValue.len == 0
      m.mapValue[newIntForm(1)] == newIntForm(99)

  test "put negative int key goes to mapValue":
    let m = newMapForm()
    m.put(newIntForm(-1), newIntForm(99))
    check:
      m.posValue.len == 0
      m.mapValue[newIntForm(-1)] == newIntForm(99)

  test "put sym key goes to mapValue":
    let m = newMapForm()
    m.put(newSymForm(1), newIntForm(99))
    check:
      m.mapValue[newSymForm(1)] == newIntForm(99)

  test "put in extended range removes shadow in mapValue":
    # mapValue has int key 2, we extend posValue to cover it
    let m = newMapForm(@[newIntForm(1), newIntForm(2)],
                       @[(newIntForm(2), newIntForm(99))])
    m.put(newIntForm(2), newIntForm(50))
    check:
      m.posValue.len == 3
      m.posValue[2] == newIntForm(50)
      not m.mapValue.hasKey(newIntForm(2))

  test "put clears shadows only in newly created range":
    # mapValue has int keys 5 and 10, extend only up to 5
    let m = newMapForm(@[], @[(newIntForm(5), newIntForm(50)),
                              (newIntForm(10), newIntForm(100))])
    m.put(newIntForm(5), newIntForm(55))
    check:
      m.mapValue.hasKey(newIntForm(10))
      m.mapValue[newIntForm(10)] == newIntForm(100)


suite "MapForm API - len":
  test "empty map":
    let m = newMapForm()
    check:
      m.len == 0

  test "only posValue":
    let m = newMapForm(@[newIntForm(1), newIntForm(2)])
    check:
      m.len == 2

  test "only mapValue":
    let m = newMapForm(@[], @[(newSymForm(1), newIntForm(10))])
    check:
      m.len == 1

  test "both":
    let m = newMapForm(@[newIntForm(1)],
                       @[(newSymForm(2), newIntForm(20))])
    check:
      m.len == 2


suite "MapForm API - invariant A (no int shadows)":
  test "after put within posValue range":
    let m = newMapForm(@[newIntForm(1), newIntForm(2)],
                       @[(newIntForm(1), newIntForm(99))])
    m.put(newIntForm(1), newIntForm(50))
    check:
      not m.mapValue.hasKey(newIntForm(1))

  test "after append at boundary":
    let m = newMapForm(@[newIntForm(1)],
                       @[(newIntForm(1), newIntForm(99))])
    m.append(newIntForm(2))
    check:
      m.posValue.len == 2
      not m.mapValue.hasKey(newIntForm(1))