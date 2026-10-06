import std/[unittest, tables, strutils]
import forms

suite "InternmentData":
  test "intern returns stable id for same name":
    let d = newInternmentData()
    let a = d.intern("foo")
    let b = d.intern("foo")
    check:
      a == b

  test "intern is case-insensitive":
    let d = newInternmentData()
    check:
      d.intern("foo") == d.intern("FOO")
      d.intern("foo") == d.intern("Foo")
      d.intern("foo") == d.intern("fOo")

  test "intern stores name uppercased":
    let d = newInternmentData()
    check:
      d.unintern(d.intern("foo")) == "FOO"

  test "different names get different ids":
    let d = newInternmentData()
    check:
      d.intern("foo") != d.intern("bar")

  test "unintern roundtrips":
    let d = newInternmentData()
    for name in ["foo", "BAR", "baz123", "x"]:
      check:
        d.unintern(d.intern(name)) == name.toUpper()

  test "ids are sequential within fresh data":
    let d = newInternmentData()
    let a = d.intern("first-new-sym")
    let b = d.intern("second-new-sym")
    check:
      b == a + 1

  test "bindings and interning stay in sync":
    let d = newInternmentData()
    let id = d.intern("sync-check")
    check:
      d.bindings["SYNC-CHECK"] == id
      d.interning[id] == "SYNC-CHECK"

  test "newInternmentData copies requiredInterns":
    let d = newInternmentData()
    check:
      d.unintern(`ENV-CURRENT`) == ";ENV-CURRENT"
      d.unintern(`EVAL-IF`) == "IF"
      d.unintern(`CONS-HEAD`) == ";CONS-HEAD"

  test "two fresh InternmentData are independent":
    let d1 = newInternmentData()
    let d2 = newInternmentData()
    check:
      d1.intern("shared-check") == d2.intern("shared-check")

    discard d1.intern("only-in-d1")
    check:
      not d2.bindings.hasKey("ONLY-IN-D1")