import ../src/sysvafly
import std/tables

template start =
  checkTop(M(^"ECHO" ? l1, M(^"LOCAL" ? l2, ^"X" ? l3, %10 ? l4) ? l5, M(^"+" ? l6, ^"X" ? l7, %1 ? l8) ? l9) ? l10)

init()
var src = initTable[string, string](); let f1 = "examples/test.vafl"; src[f1] = "(echo (local x 10) (+ x 1))"; let l1 = loc(f1, 1, 2); let l2 = loc(f1, 1, 8); let l3 = loc(f1, 1, 14); let l4 = loc(f1, 1, 16); let l5 = loc(f1, 1, 7); let l6 = loc(f1, 1, 21); let l7 = loc(f1, 1, 23); let l8 = loc(f1, 1, 25); let l9 = loc(f1, 1, 20); let l10 = loc(f1, 1, 1)

start()
