structure ReaderTest =
struct
  structure U = UnitTest
  fun mkdesc (i, j) = concat [Int.toString i, ", ", Int.toString j]
  fun iscorner (i, j) = i mod 2 = 0 andalso j mod 2 = 0

  (* Steps through a 2D array of data and validates according to supplied function (and known invariants) *)
  fun checkdata f data = Array2.appi Array2.RowMajor (fn (i, j, e) => let
                                                             val desc = mkdesc (i, j)
                                                         in
                                                             ignore (if (f (i, j) orelse iscorner (i, j)) then
                                                                         U.istrue(e, desc)
                                                                     else
                                                                         U.istrue(not e, desc))
                                                         end)
                                     (Misc.toRegion data)
                         
  fun test() = let
      val test1 = Reader.read "test1.desc"
      val _ = U.eqi(3, Array2.nRows test1)
      val _ = U.eqi(3, Array2.nCols test1)
      val _ = checkdata (fn (i, j) => i = 0 andalso j = 1) test1

      val test2 = Reader.read "test2.desc"
      val _ = U.eqi(3, Array2.nRows test1)
      val _ = U.eqi(3, Array2.nCols test1)
      val _ = checkdata (fn (i, j) => i = 0 orelse i = 2 orelse j = 0 orelse j = 2) test2
                  
      val test3 = Reader.read "test3.desc"
      val _ = U.eqi(5, Array2.nRows test3)
      val _ = U.eqi(5, Array2.nCols test3)

      val _ = checkdata (fn (i, j) => i = 0 orelse j = 0 orelse i = 4 orelse j = 4 orelse (i = 2 andalso j = 1)) test3
  in
      ()
  end
end
