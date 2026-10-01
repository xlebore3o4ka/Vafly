import ../src/sysvafly
import macros

template start =
  expandMacros:
    discard eval M(^"DEFUN" ? l1, ^"FACT" ? l2, M(^"N" ? l3) ? l4, M(^"IF" ? l5, M(^"EQ" ? l6, ^"N" ? l7, %0 ? l8) ? l9, %1 ? l10, M(^"*" ? l11, ^"N" ? l12, M(^"FACT" ? l13, M(^"-" ? l14, ^"N" ? l15, %1 ? l16) ? l17) ? l18) ? l19) ? l20) ? l21
    discard eval M(^"ECHO" ? l22, %"fact 5 = " ? l23, M(^"FACT" ? l24, %5 ? l25) ? l26) ? l27

init()
let f1 = "examples/test.vafl"; let l1 = loc(f1, 1, 2); let l2 = loc(f1, 1, 8); let l3 = loc(f1, 1, 14); let l4 = loc(f1, 1, 13); let l5 = loc(f1, 2, 4); let l6 = loc(f1, 2, 8); let l7 = loc(f1, 2, 11); let l8 = loc(f1, 2, 13); let l9 = loc(f1, 2, 7); let l10 = loc(f1, 2, 16); let l11 = loc(f1, 3, 5); let l12 = loc(f1, 3, 7); let l13 = loc(f1, 3, 10); let l14 = loc(f1, 3, 16); let l15 = loc(f1, 3, 18); let l16 = loc(f1, 3, 20); let l17 = loc(f1, 3, 15); let l18 = loc(f1, 3, 9); let l19 = loc(f1, 3, 4); let l20 = loc(f1, 2, 3); let l21 = loc(f1, 1, 1); let l22 = loc(f1, 5, 2); let l23 = loc(f1, 5, 7); let l24 = loc(f1, 5, 20); let l25 = loc(f1, 5, 25); let l26 = loc(f1, 5, 19); let l27 = loc(f1, 5, 1); 
start()
