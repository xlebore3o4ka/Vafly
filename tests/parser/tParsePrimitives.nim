import std/[unittest, tables]
import forms, parser, sysvafly

template parseLine(s: string): Form =
  parse(s, "<test>", globalInternmentData).get(0)

suite "Parser primitives - int":
  setup:
    init()

  test "1":
    check parseLine("1") == %1

  test "42":
    check parseLine("42") == %42

  test "0":
    check parseLine("0") == %0

  test "-1":
    check parseLine("-1") == %(-1)

  test "+1":
    check parseLine("+1") == %1

  test "1234567890":
    check parseLine("1234567890") == %1234567890


suite "Parser primitives - string":
  setup:
    init()

  test "empty":
    check parseLine("\"\"") == %""

  test "hello":
    check parseLine("\"hello\"") == %"hello"

  test "with spaces":
    check parseLine("\"a b c\"") == %"a b c"

  test "escaped quote":
    check parseLine("\"a\\\"b\"") == %"a\"b"

  test "escaped backslash":
    check parseLine("\"a\\\\b\"") == %"a\\b"

  test "digits as string":
    check parseLine("\"123\"") == %"123"


suite "Parser primitives - symbol":
  setup:
    init()

  test "foo":
    check parseLine("foo") == ^"FOO"

  test "FOO":
    check parseLine("FOO") == ^"FOO"

  test "mixed case":
    check parseLine("FoO") == ^"FOO"

  test "plus":
    check parseLine("+") == ^"+"

  test "minus":
    check parseLine("-") == ^"-"

  test "equals":
    check parseLine("=") == ^"="

  test "question mark":
    check parseLine("int?") == ^"INT?"

  test "ampersand":
    check parseLine("&raw") == ^"&RAW"


suite "Parser primitives - nil":
  setup:
    init()

  test "nil":
    check parseLine("nil") == newNilForm()

  test "NIL":
    check parseLine("NIL") == newNilForm()

  test "Nil":
    check parseLine("Nil") == newNilForm()