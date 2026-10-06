import std/[unittest, tables]
import forms, parser, sysvafly

template parseLine(s: string): Form =
  parse(s, "<test>", globalInternmentData).get(0)

suite "Parser - quote":
  setup:
    init()

  test "'(1 2 3)":
    let expected = newMapForm(@[
      newSymForm(`PARSER-QUOTE`),
      newMapForm(@[%1, %2, %3])
    ])
    check parseLine("'(1 2 3)") == expected

  test "'x":
    let expected = newMapForm(@[
      newSymForm(`PARSER-QUOTE`),
      ^"X"
    ])
    check parseLine("'x") == expected

  test "''x":
    let expected = newMapForm(@[
      newSymForm(`PARSER-QUOTE`),
      newMapForm(@[newSymForm(`PARSER-QUOTE`), ^"X"])
    ])
    check parseLine("''x") == expected


suite "Parser - keyword":
  setup:
    init()

  test "((:a 1))":
    let expected = newMapForm(@{^"A": %1})
    check parseLine("((:a 1))") == expected

  test "((:1 b))":
    let expected = newMapForm(@{%1: ^"B"})
    check parseLine("((:1 b))") == expected

  test "((:\"c\" 3))":
    let expected = newMapForm(@{%"c": %3})
    check parseLine("((:\"c\" 3))") == expected

  test "((:a 1) (:1 b) (:\"c\" 3))":
    check parseLine("((:a 1) (:1 b) (:\"c\" 3))") ==
      newMapForm(@{^"A": %1, %1: ^"B", %"c": %3})