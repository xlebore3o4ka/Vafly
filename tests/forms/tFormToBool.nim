import std/[unittest, tables]
import forms

suite "Form toBool":
  test "nil is false":
    check:
      not newNilForm().toBool()

  test "int: zero is false, non-zero is true":
    check:
      not newIntForm(0).toBool()
      newIntForm(1).toBool()
      newIntForm(-1).toBool()
      newIntForm(42).toBool()

  test "str: empty is false, non-empty is true":
    check:
      not newStrForm("").toBool()
      newStrForm("a").toBool()
      newStrForm(" ").toBool()
      newStrForm("0").toBool()

  test "sym is always false":
    check:
      not newSymForm(0).toBool()
      not newSymForm(1).toBool()
      not newSymForm(42).toBool()

  test "err is always false":
    check:
      not newErrForm(newSymForm(1), "boom").toBool()
      not newErrForm(newSymForm(1), "").toBool()

  test "map: empty is false":
    check:
      not newMapForm().toBool()

  test "map: with posValue is true":
    check:
      newMapForm(@[newNilForm()]).toBool()
      newMapForm(@[newIntForm(0)]).toBool()

  test "map: with mapValue is true":
    check:
      newMapForm(@{newSymForm(1): newNilForm()}).toBool()

  test "map: with both is true":
    check:
      newMapForm(@[newIntForm(1)], @{newSymForm(2): newIntForm(0)}).toBool()