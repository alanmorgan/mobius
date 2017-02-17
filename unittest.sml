structure UnitTest =
struct
  fun eqs (s1, s2) = if s1 = s2 then
                         true
                     else
                         false before (app print [ "Strings [", s1, "] and [", s2, "] differ\n"])
                         
  fun eqr (r1, r2) = if Real.== (r1, r2) then
                         true
                     else
                         false before (app print [ "Real numbers ", Real.toString r1, ", ", Real.toString r2, " differ\n"])

  fun eqi (i, j) = if i = j then
                       true
                   else
                       false before (app print ["Integers ", Int.toString i, ", ", Int.toString j, " differ\n"])

  fun istrue (b, desc) = if b then
                     true
                 else
                     false before (app print ["Failure for ", desc, "\n"])

end
