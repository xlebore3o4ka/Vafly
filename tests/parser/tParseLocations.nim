import std/[unittest, tables]
import forms, parser, sysvafly

template parseLine(s: string): Form =
  parse(s, "<test>", globalInternmentData).get(0)

template locOf(f: Form): tuple[line, col: int] =
  (f.locationData.line, f.locationData.col)

suite "Parser - locations: single line":
  setup:
    init()

  test "int at col 1":
    check parseLine("1").locOf == (1, 1)

  test "int at col 3":
    check parseLine("  1").locOf == (1, 3)

  test "second int":
    let m = parse("1 2", "<test>", globalInternmentData)
    check:
      m.get(0).locOf == (1, 1)
      m.get(1).locOf == (1, 3)

  test "map starts at ( position":
    check parseLine("  (1)").locOf == (1, 3)

  test "element inside map":
    let m = parseLine("(1 2)")
    check:
      m.get(0).locOf == (1, 2)
      m.get(1).locOf == (1, 4)

  test "nested map position":
    let m = parseLine("(a (b))")
    check:
      m.get(0).locOf == (1, 2)
      m.get(1).locOf == (1, 4)
      m.get(1).get(0).locOf == (1, 5)

  test "symbol position":
    check parseLine("  foo").locOf == (1, 3)

  test "string position":
    check parseLine("  \"x\"").locOf == (1, 3)

  test "quote position":
    check parseLine("'x").locOf == (1, 1)


suite "Parser - locations: multiline":
  setup:
    init()

  test "second element on line 2":
    let m = parse("1\n2", "<test>", globalInternmentData)
    check:
      m.get(0).locOf == (1, 1)
      m.get(1).locOf == (2, 1)

  test "map opens on line 2":
    let m = parseLine("\n(1)")
    check:
      m.locOf == (2, 1)

  test "element inside multiline map":
    let m = parseLine("(\n1\n2)")
    check:
      m.get(0).locOf == (2, 1)
      m.get(1).locOf == (3, 1)

  test "CRLF treated as newline":
    let m = parse("1\r\n2", "<test>", globalInternmentData)
    check:
      m.get(0).locOf == (1, 1)
      m.get(1).locOf == (2, 1)


suite "Parser - locations: filename":
  setup:
    init()

  test "filename propagated to top form":
    let m = parse("1", "hello.vafl", globalInternmentData)
    check:
      m.locationData.filename[] == "hello.vafl"

  test "filename propagated to nested forms":
    let m = parse("(a (b))", "x.vafl", globalInternmentData).get(0)
    check:
      m.get(0).locationData.filename[] == "x.vafl"
      m.get(1).locationData.filename[] == "x.vafl"
      m.get(1).get(0).locationData.filename[] == "x.vafl"