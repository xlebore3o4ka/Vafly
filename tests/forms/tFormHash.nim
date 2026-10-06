import std/[unittest, tables]
import forms

suite "Form hash":
  test "nil forms hash equal":
    check hash(newNilForm()) == hash(newNilForm())

  test "int forms: equal values hash equal, different differ":
    check hash(newIntForm(0)) == hash(newIntForm(0))
    check hash(newIntForm(42)) == hash(newIntForm(42))
    check hash(newIntForm(-1)) == hash(newIntForm(-1))
    check hash(newIntForm(1)) != hash(newIntForm(2))

  test "sym forms":
    check hash(newSymForm(1)) == hash(newSymForm(1))
    check hash(newSymForm(1)) != hash(newSymForm(2))

  test "str forms":
    check hash(newStrForm("")) == hash(newStrForm(""))
    check hash(newStrForm("a")) == hash(newStrForm("a"))
    check hash(newStrForm("a")) != hash(newStrForm("b"))

  test "different kinds do not collide (kind is mixed in)":
    check hash(newIntForm(0)) != hash(newNilForm())
    check hash(newIntForm(1)) != hash(newSymForm(1))
    check hash(newSymForm(1)) != hash(newStrForm("1"))

  test "empty maps":
    check hash(newMapForm()) == hash(newMapForm())

  test "posValue order matters":
    let a = newMapForm(@[newIntForm(1), newIntForm(2)])
    let b = newMapForm(@[newIntForm(2), newIntForm(1)])
    check hash(a) != hash(b)

  test "posValue equality implies hash equality":
    let a = newMapForm(@[newIntForm(1), newIntForm(2)])
    let b = newMapForm(@[newIntForm(1), newIntForm(2)])
    check hash(a) == hash(b)

  test "mapValue equality":
    let a = newMapForm(@[(newSymForm(1), newIntForm(10))])
    let b = newMapForm(@[(newSymForm(1), newIntForm(10))])
    check hash(a) == hash(b)

  test "mapValue different values":
    let a = newMapForm(@[(newSymForm(1), newIntForm(10))])
    let b = newMapForm(@[(newSymForm(1), newIntForm(20))])
    check hash(a) != hash(b)

  test "mapValue different keys":
    let a = newMapForm(@[(newSymForm(1), newIntForm(10))])
    let b = newMapForm(@[(newSymForm(2), newIntForm(10))])
    check hash(a) != hash(b)

  test "err forms":
    let a = newErrForm(newSymForm(1), "boom")
    let b = newErrForm(newSymForm(1), "boom")
    check hash(a) == hash(b)

  test "err forms differ by sym":
    let a = newErrForm(newSymForm(1), "boom")
    let b = newErrForm(newSymForm(2), "boom")
    check hash(a) != hash(b)

  test "err forms differ by msg":
    let a = newErrForm(newSymForm(1), "boom")
    let b = newErrForm(newSymForm(1), "bang")
    check hash(a) != hash(b)

  test "consistency: equal forms hash equal":
    let a = newMapForm(@[newSymForm(1), newIntForm(5)])
    let b = newMapForm(@[newSymForm(1), newIntForm(5)])
    check a == b
    check hash(a) == hash(b)