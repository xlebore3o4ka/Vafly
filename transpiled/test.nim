import ../src/sysvafly
import std/tables

template start =
  checkTop(M(^"ECHO" ? l1, `OPT+`(l2, `OPT+`(l3, %1 ? l4, %2 ? l5), `OPT*`(l6, `OPT+`(l7, %4 ? l8, %5 ? l9), %3 ? l10), `OPT-DIV`(l11, %24 ? l12, %5 ? l13))) ? l14)

init()
var src = initTable[string, string](); let f1 = "examples/test.vafl"; src[f1] = "(echo (+ (+ 1 2) (* (+ 4 5) 3) (div 24 5)))"; let l1 = loc(f1, 1, 2); let l2 = loc(f1, 1, 8); let l3 = loc(f1, 1, 11); let l4 = loc(f1, 1, 13); let l5 = loc(f1, 1, 15); let l6 = loc(f1, 1, 19); let l7 = loc(f1, 1, 22); let l8 = loc(f1, 1, 24); let l9 = loc(f1, 1, 26); let l10 = loc(f1, 1, 29); let l11 = loc(f1, 1, 33); let l12 = loc(f1, 1, 37); let l13 = loc(f1, 1, 40); let l14 = loc(f1, 1, 1)

start()
