structure Reader : sig
val read : string -> (bool Array2.array * real * real * real)
end = 
struct

fun gettriple s = let
    fun realFromString s =
        case Real.fromString s of 
            NONE => 0.0
          | SOME x => x

    val ts = String.tokens (fn x => x = #" ") s
in
    (realFromString (List.nth (ts, 0)),
     realFromString (List.nth (ts, 1)),
     realFromString (List.nth (ts, 2)))
end
    
fun getdata f = let
    val so = TextIO.inputLine f
in
    case so of 
        NONE => raise Fail "End of file reached"
      | SOME s => if (String.isPrefix ";" s) then
                      getdata f
                  else
                      gettriple s
end

(* Skipping ;; *)
fun getline f = let 
    val so = TextIO.inputLine f
in
    case so of
        NONE => NONE
      | SOME s => if (String.isPrefix ";;" s) then
                      getline f
                  else
                      so
end

(* Spaces are walls, everything else isn't.  We are actually recording more information than we need, but
 * it's more trouble than it's worth to come up with a more compact representation.  Also, strip off the
 * trailing newline
 *)
fun toWalls s = map (fn c => c <> #" ") ((rev o tl o rev o explode) s)

fun read fname = let
    val f = TextIO.openIn fname
            
    fun getWalls f =
        let
            fun getlines f = let
                fun getlines' acc =
                    case getline f of
                        NONE => acc
                      | SOME s => getlines' (s::acc)
            in
                rev (getlines' [])
            end

            val (wheight, wthick, thresh) = getdata f

            (* Tweak the input data.  We want walls on the top and bottom.  The easiest way to do this is to
             * insert blank data at the beginning and end.  This actually requires two lines at both ends
             *)
            val lines = let
                val lines' = getlines f
                val spaces = implode (List.tabulate (size (hd lines'), fn _ => #" "))
            in
                (spaces::spaces::lines')@[spaces, spaces]
            end
        in
            (Array2.fromList (Misc.transposeList (map (fn x => toWalls x) lines)),
             wheight,
             wthick,
             thresh)
        end
in
    getWalls f
end

end
