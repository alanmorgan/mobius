structure Misc =
struct
(* Convert an Array2 to a region.  Stupid sml *)
fun toRegion (arr:'a Array2.array) = { base = arr, row = 0, col = 0, ncols = NONE, nrows = NONE }

(* Change [[a, b, c], [d, e, f]] to [[a, d], [b, e], [c, f]] *)
fun transposeList ls = let
    val (fst, rst) = foldr (fn (l, (ls, acc)) => ((hd l)::ls, (tl l)::acc)) ([],[]) ls 
in
    if (List.null (hd rst)) then
        [fst]
    else
        fst::(transposeList rst)
end

(* Remove the last character from a string *)
fun chop s = String.extract (s, 0, SOME (size s -1))

end
