val assert = fun id act exp ->
   if act == exp
   then "  PASS: " ++ id
   else "* FAIL: " ++ id;