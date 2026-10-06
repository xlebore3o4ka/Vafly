import std/[unittest, tables]
import forms, parser, sysvafly

template parseLine(s: string): Form =
  parse(s, "<test>", globalInternmentData).get(0)

suite "Parser - map: empty":
  setup:
    init()

  test "()":
    check parseLine("()") == newMapForm()


suite "Parser - map: positional":
  setup:
    init()

  test "(1)":
    check parseLine("(1)") == newMapForm(@[%1])

  test "(1 2 3)":
    check parseLine("(1 2 3)") == newMapForm(@[%1, %2, %3])

  test "(a b c)":
    check parseLine("(a b c)") == newMapForm(@[^"A", ^"B", ^"C"])

  test "(1 a \"x\" nil)":
    check parseLine("(1 a \"x\" nil)") ==
      newMapForm(@[%1, ^"A", %"x", newNilForm()])

  test "leading and trailing spaces":
    check parseLine("  ( 1 2 )  ") == newMapForm(@[%1, %2])

  test "newlines inside":
    check parseLine("(1\n2\n3)") == newMapForm(@[%1, %2, %3])


suite "Parser - map: nested":
  setup:
    init()

  test "(())":
    check parseLine("(())") == newMapForm(@[newMapForm()])

  test "((1 2) (3 4))":
    check parseLine("((1 2) (3 4))") ==
      newMapForm(@[newMapForm(@[%1, %2]), newMapForm(@[%3, %4])])

  test "(((1)))":
    check parseLine("(((1)))") ==
      newMapForm(@[newMapForm(@[newMapForm(@[%1])])])

  test "(1 (2 (3 4)))":
    check parseLine("(1 (2 (3 4)))") ==
      newMapForm(@[%1, newMapForm(@[%2, newMapForm(@[%3, %4])])])


suite "Parser - map: comments":
  setup:
    init()

  test "comment before":
    check parseLine("; comment\n1") == %1

  test "comment in map":
    check parseLine("(1 ; c\n2)") == newMapForm(@[%1, %2])

  test "comment after map":
    check parseLine("(1 2) ; c") == newMapForm(@[%1, %2])

  test "comment only":
    check parse("; only comment", "<test>", globalInternmentData) == newMapForm()


suite "Parser - map: errors":
  setup:
    init()

  test "unclosed (":
    let res = parse("(1 2", "<test>", globalInternmentData)
    check res.kind == fkErr

  test "unmatched )":
    let res = parse("1)", "<test>", globalInternmentData)
    check res.kind == fkErr

  test "unmatched ) alone":
    let res = parse(")", "<test>", globalInternmentData)
    check res.kind == fkErr