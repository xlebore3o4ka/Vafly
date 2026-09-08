import compiler

echo """
(defun test (a b) (
  + a b ; return
)) 
(print (test (10 -20)))
""".parse("<string>")