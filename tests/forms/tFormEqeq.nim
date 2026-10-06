import std/[unittest, tables]
import forms

suite "Form ==":
  test "nil forms are equal":
    check newNilForm() == newNilForm()

  test "nil forms are not equal to non-nil":
    check newNilForm() != newIntForm(0)
    check newIntForm(0) != newNilForm()
    check newNilForm() != newMapForm()

  test "int forms":
    check newIntForm(0) == newIntForm(0)
    check newIntForm(42) == newIntForm(42)
    check newIntForm(-1) == newIntForm(-1)
    check newIntForm(1) != newIntForm(2)

  test "sym forms":
    check newSymForm(1) == newSymForm(1)
    check newSymForm(1) != newSymForm(2)

  test "str forms":
    check newStrForm("") == newStrForm("")
    check newStrForm("abc") == newStrForm("abc")
    check newStrForm("abc") != newStrForm("abd")

  test "different kinds are not equal":
    check newIntForm(1) != newSymForm(1)
    check newSymForm(1) != newStrForm("1")
    check newStrForm("nil") != newNilForm()
    check newIntForm(0) != newNilForm()

  test "err forms: sym and msg compared":
    let a = newErrForm(newSymForm(1), "boom")
    let b = newErrForm(newSymForm(1), "boom")
    let c = newErrForm(newSymForm(2), "boom")
    let d = newErrForm(newSymForm(1), "bang")
    check a == b
    check a != c
    check a != d

  test "err forms: trace is ignored":
    let a = newErrForm(newSymForm(1), "boom")
    let b = newErrForm(newSymForm(1), "boom")
    b.errTrace.append(newSymForm(99))
    check a == b

  test "empty maps":
    check newMapForm() == newMapForm()

  test "maps: posValue compared positionally":
    let a = newMapForm(@[newIntForm(1), newIntForm(2)])
    let b = newMapForm(@[newIntForm(1), newIntForm(2)])
    let c = newMapForm(@[newIntForm(2), newIntForm(1)])
    let d = newMapForm(@[newIntForm(1)])
    check a == b
    check a != c
    check a != d

  test "maps: mapValue compared by keys":
    let a = newMapForm(@[(newSymForm(1), newIntForm(10))])
    let b = newMapForm(@[(newSymForm(1), newIntForm(10))])
    let c = newMapForm(@[(newSymForm(2), newIntForm(10))])
    let d = newMapForm(@[(newSymForm(1), newIntForm(20))])
    let e = newMapForm(@[(newSymForm(1), newIntForm(10)),
                         (newSymForm(2), newIntForm(20))])
    check a == b
    check a != c
    check a != d
    check a != e

  test "maps: insertion order of mapValue is ignored":
    let a = newMapForm(@[(newSymForm(1), newIntForm(10)),
                         (newSymForm(2), newIntForm(20))])
    let b = newMapForm(@[(newSymForm(2), newIntForm(20)),
                         (newSymForm(1), newIntForm(10))])
    check a == b

  test "maps: posValue and mapValue must both match":
    let a = newMapForm(@[newIntForm(1)], @[(newSymForm(2), newIntForm(20))])
    let b = newMapForm(@[newIntForm(1)], @[(newSymForm(2), newIntForm(20))])
    let c = newMapForm(@[newIntForm(1)])
    let d = newMapForm(@[], @[(newSymForm(2), newIntForm(20))])
    check a == b
    check a != c
    check a != d

  test "nested maps":
    let inner = newMapForm(@[newIntForm(1)])
    let a = newMapForm(@[inner])
    let b = newMapForm(@[inner])
    let c = newMapForm(@[newMapForm(@[newIntForm(1)])])
    check a == b
    check a == c

  test "symmetry":
    let a = newMapForm(@[newIntForm(1)])
    let b = newMapForm(@[newIntForm(2)])
    check (a == b) == (b == a)

  test "reflexivity on non-nil forms":
    let a = newMapForm(@[newIntForm(1), newSymForm(2)])
    check a == a

  test "nil Form refs are equal":
    let a: Form = nil
    let b: Form = nil
    check a == b