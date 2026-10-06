import std/[unittest, tables]
import forms, evaluator, sysvafly

suite "sysvafly - init":
  setup:
    init()

  test "globalCtx is set":
    check not globalCtx.isNil()

  test "globalInternmentData is set":
    check not globalInternmentData.isNil()

  test "filenameRefs is empty":
    check filenameRefs.len == 0

  test "requiredInterns are available":
    check:
      globalInternmentData.unintern(`ENV-CURRENT`) == ";ENV-CURRENT"
      globalInternmentData.unintern(`EVAL-IF`) == "IF"

  test "reinit resets filenameRefs":
    discard loc("a.vafl", 1, 1)
    check filenameRefs.len == 1
    init()
    check filenameRefs.len == 0


suite "sysvafly - ^ (symbol)":
  setup:
    init()

  test "interns string":
    let s = ^"foo"
    check:
      s.kind == fkSym
      globalInternmentData.unintern(s.symInterned) == "FOO"

  test "case-insensitive":
    check:
      ^"foo" == ^"FOO"
      ^"foo" == ^"Foo"

  test "different symbols differ":
    check ^"foo" != ^"bar"

  test "existing intern reused":
    let a = ^"x"
    let b = ^"x"
    check a.symInterned == b.symInterned


suite "sysvafly - % (int/str/nil)":
  setup:
    init()

  test "%int":
    let i = %42
    check:
      i.kind == fkInt
      i.intValue == 42

  test "%negative int":
    check (%(-7)).intValue == -7

  test "%str":
    let s = %"hello"
    check:
      s.kind == fkStr
      s.strValue == "hello"

  test "%empty str":
    check (%"").strValue == ""

  test "%nil":
    check (%nil).kind == fkNil

  test "% overloads pick correct kind":
    check:
      (%1).kind == fkInt
      (%"1").kind == fkStr
      (%nil).kind == fkNil


suite "sysvafly - M (map varargs)":
  setup:
    init()

  test "empty M":
    let m = M()
    check:
      m.kind == fkMap
      m.posValue.len == 0
      m.mapValue.len == 0

  test "M with one element":
    let m = M(%1)
    check:
      m.posValue.len == 1
      m.posValue[0] == %1

  test "M with multiple elements":
    let m = M(%1, %2, %3)
    check:
      m.posValue.len == 3
      m.posValue[0] == %1
      m.posValue[1] == %2
      m.posValue[2] == %3

  test "M preserves order":
    let m = M(%"a", %"b", %"c")
    check:
      m.posValue[0] == %"a"
      m.posValue[1] == %"b"
      m.posValue[2] == %"c"


suite "sysvafly - K (pos + map)":
  setup:
    init()

  test "empty K":
    let m = K(@[], newSeq[(Form, Form)]())
    check:
      m.kind == fkMap
      m.posValue.len == 0
      m.mapValue.len == 0

  test "K with pos only":
    let m = K(@[%1, %2], newSeq[(Form, Form)]())
    check:
      m.posValue.len == 2
      m.mapValue.len == 0

  test "K with map only":
    let m = K(@[], @{^"a": %1})
    check:
      m.posValue.len == 0
      m.mapValue.len == 1
      m.mapValue[^"a"] == %1

  test "K with both":
    let m = K(@[%1], @{^"a": %2})
    check:
      m.posValue[0] == %1
      m.mapValue[^"a"] == %2


suite "sysvafly - loc":
  setup:
    init()

  test "creates LocationData with fields":
    let l = loc("a.vafl", 3, 5)
    check:
      l.filename[] == "a.vafl"
      l.line == 3
      l.col == 5

  test "same filename shares ref":
    let l1 = loc("a.vafl", 1, 1)
    let l2 = loc("a.vafl", 2, 2)
    check l1.filename == l2.filename

  test "different filenames have different refs":
    let l1 = loc("a.vafl", 1, 1)
    let l2 = loc("b.vafl", 1, 1)
    check l1.filename != l2.filename

  test "filenameRefs cache grows":
    check filenameRefs.len == 0
    discard loc("a.vafl", 1, 1)
    check filenameRefs.len == 1
    discard loc("a.vafl", 9, 9)
    check filenameRefs.len == 1
    discard loc("b.vafl", 1, 1)
    check filenameRefs.len == 2


suite "sysvafly - ? (set location)":
  setup:
    init()

  test "sets location and returns form":
    let l = loc("a.vafl", 3, 5)
    let f = %1 ? l
    check:
      f.kind == fkInt
      f.locationData.line == 3
      f.locationData.col == 5
      f.locationData.filename[] == "a.vafl"

  test "overwrites previous location":
    let l1 = loc("a.vafl", 1, 1)
    let l2 = loc("b.vafl", 2, 2)
    let f = (%1 ? l1) ? l2
    check:
      f.locationData.filename[] == "b.vafl"
      f.locationData.line == 2


suite "sysvafly - bad":
  test "bad raises AssertionDefect":
    expect AssertionDefect:
      discard bad(fkInt)


suite "sysvafly - eval":
  setup:
    init()

  test "eval int returns int":
    check eval(%1) == %1

  test "eval str returns str":
    check eval(%"x") == %"x"

  test "eval nil returns nil":
    check eval(%nil).kind == fkNil

  test "eval empty map returns map":
    check eval(M()).kind == fkMap

  test "eval symbol is unbound error":
    let r = eval(^"undefined-sym")
    check r.kind == fkErr


suite "sysvafly - checkTop":
  var src {.used.}: Table[string, string]

  setup:
    init()
    src = initTable[string, string]()

  test "checkTop on ok value does not raise":
    checkTop(%1)

  test "checkTop raises VaflyError on error":
    expect VaflyError:
      checkTop(^"undefined-sym")