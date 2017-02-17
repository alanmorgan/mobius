structure Builder : sig
    val buildit : string -> Facet.f list
end =
struct
structure D = Deform
structure F = Facet
structure V = Vector
structure Q = Quad


(* Is there a border to the top, bottom, left, right? *)
type square = { top : bool, bottom : bool, left : bool, right : bool}

(*
 * Squares have walls on some number of sides.  Walls can't intersect each other, so we have
 * to build our faces carefully.
 *
 *               /   /                                        / / /
 *              /C  /                                        / / /
 *     ________/___/__________                       _______/_/_/__________
 *      A     / J /   B                             _______/_/_/_________  
 *    _______/___/_________                        _______/_/_/_________
 *           |   |                                        | | |
 *      A'   |J' |   B'                                   | | | 
 *           |   |                                        | | |
 *
 * Consider the above case of walls A, B, and C meeting at a joint J.  A and B (and C as well), have
 * vertical faces A' and B' (and ones on the backside blah blah blah).  J doesn't really belong to any
 * wall, but the vertical face that it has (J') depends on what walls surround it.  If there is just one
 * wall going in to it then it will have three vertical faces; two, two; three, one; four, none (if there
 * are no walls going there, which does happen for the P polyomino) then the joint doesn't exist.
 *
 * Each of A, B, and C need to stop at half the wall thickness from the joint to leave space for it.  To
 * ensure that we don't have overlapping pieces we can either be clever or we can build each wall in two
 * bits (or four bits, in the case of the joints), as the second picture illustrates.  Not being clever
 * turns out to be a lot easier
 *
 * We are going to build this along the x/y axis, with the walls having height along the z axis.  Luckily,
 * all walls and joints have the same height and start at the same point on the z axis
 *
 *)

(* Wall positions *)
datatype dirs = LEFT | RIGHT | TOP | BOTTOM
(* Joint positions *)
datatype corners = TOPLEFT | TOPRIGHT | BOTTOMLEFT | BOTTOMRIGHT
                                                         

fun realpair (x, y) = (Real.fromInt x, Real.fromInt y)

fun mksquares walls =
    let
        fun mksquare (x, y) =
            { bottom = Array2.sub(walls, 2*x+1, 2* y),
              top = Array2.sub(walls, 2*x+1, 2 * (y+1)),
              left = Array2.sub(walls, 2*x, 2*y+1),
              right = Array2.sub(walls, 2*(x+1), 2*y+1) }
    in
        Array2.tabulate Array2.RowMajor ((Array2.nRows walls - 1) div 2, (Array2.nCols walls - 1) div 2, mksquare)
    end

val unit_size = 0.5

fun buildit s = 
let
    val (data, wheight, wthick, thresh) = Reader.read s

    val wall_thickness = wthick * unit_size
    val wall_height = wheight * unit_size 
                      
    val half_wall = wall_thickness / 2.0
    val zhigh = wall_height / 2.0
    val zlow = ~wall_height / 2.0
               
    fun buildsinglewall ((x, y), dir) = let
        (* x and y vectors for a half-wall thickness *)
        val xhwvec = V.mkvector (half_wall, 0.0, 0.0)
        val yhwvec = V.mkvector (0.0, half_wall, 0.0)
        (* And other other directions *)
        val xhwvec' = V.mkvector (~half_wall, 0.0, 0.0)
        val yhwvec' = V.mkvector (0.0, ~half_wall, 0.0)
        (* x and y vectors for a wall length *)
        val xwvec = V.mkvector (unit_size - wall_thickness, 0.0, 0.0)
        val ywvec = V.mkvector (0.0, unit_size - wall_thickness, 0.0)
        (* The standard vector in the z direction *)
        val zvec = V.mkvector (0.0, 0.0, wall_height)
    in
        case dir of
            LEFT => let
                val pt = V.mkpoint (x * unit_size + half_wall, y * unit_size + half_wall, zlow)
                val pt' = V.mkpoint (x * unit_size + half_wall, y * unit_size + half_wall, zhigh)
            in
                [Q.mkquad (pt, ywvec, zvec), Q.mkquad (pt, xhwvec', ywvec), Q.mkquad (pt', ywvec, xhwvec')]
            end
          | RIGHT => let
                val pt = V.mkpoint ((x+1.0) * unit_size - half_wall, y * unit_size + half_wall, zlow)
                val pt' = V.mkpoint ((x+1.0) * unit_size - half_wall, y * unit_size + half_wall, zhigh)
            in
                [Q.mkquad (pt, zvec, ywvec), Q.mkquad (pt, ywvec, xhwvec), Q.mkquad (pt', xhwvec, ywvec)]
            end
          | TOP => let
                val pt = V.mkpoint (x * unit_size + half_wall, (y+1.0) * unit_size - half_wall, zlow)
                val pt' = V.mkpoint (x * unit_size + half_wall, (y+1.0) * unit_size - half_wall, zhigh)
            in
                [Q.mkquad (pt, xwvec, zvec), Q.mkquad (pt, yhwvec, xwvec), Q.mkquad (pt', xwvec, yhwvec)]
            end
          | BOTTOM => let
                val pt = V.mkpoint (x * unit_size + half_wall, y * unit_size + half_wall, zlow)
                val pt' = V.mkpoint (x * unit_size + half_wall, y * unit_size + half_wall, zhigh)
            in
                [Q.mkquad (pt, zvec, xwvec), Q.mkquad (pt, xwvec, yhwvec'), Q.mkquad (pt', yhwvec', xwvec)]
            end
    end
                                        
    (* Build the necessary walls for a single square *)
    fun buildsquareswalls (pos, (sqinfo : square)) = let
        fun haswall (dir) = case dir of
                                LEFT => #left sqinfo
                              | RIGHT => #right sqinfo
                              | TOP => #top sqinfo
                              | BOTTOM => #bottom sqinfo
    in
        foldl op@ [] (List.map (fn d => buildsinglewall (pos, d)) (List.filter haswall [LEFT, RIGHT, TOP, BOTTOM]))
    end
                                                     
                                                         
    (* A joint exists in a square, in one of four corners.  Joints can have walls (vertical bits) of
     * their own.  A joint on the top right might have a wall on the left or on the bottom or both
     * (or neither) depending on whether or not there are main walls to the left/bottom of it.
     *)
    fun buildsinglejoint ((x, y), corner, (haswall1, haswall2)) = let
        val xvec = V.mkvector (half_wall, 0.0, 0.0)
        val xvec' = V.mkvector (~half_wall, 0.0, 0.0)
        val yvec = V.mkvector (0.0, half_wall, 0.0)
        val yvec' = V.mkvector (0.0, ~half_wall, 0.0)
        val zvec = V.mkvector (0.0, 0.0, wall_height)
        val zvec' = V.mkvector (0.0, 0.0, ~wall_height)
                    
        fun updown (pt, pt') = [Q.mkquad (pt, xvec, yvec), Q.mkquad (pt', yvec, xvec)]
        fun buildwall (b, pt, v1, v2) = if b then [Q.mkquad(pt, v1, v2)] else []
    in
        case corner of
            TOPRIGHT => let
                val pt = V.mkpoint ((x+1.0) * unit_size - half_wall, (y+1.0) * unit_size - half_wall, zhigh)
                val pt' = V.mkpoint ((x+1.0) * unit_size - half_wall, (y+1.0) * unit_size - half_wall, zlow)
                (* The wall corner *)
                val ptw = pt
            in
                updown(pt, pt') @ 
                (* LEFT *)
                buildwall(haswall1, ptw, yvec, zvec') @
                (* BOTTOM *)
                buildwall(haswall2, ptw, zvec', xvec)
            end
          | TOPLEFT => let 
                val pt = V.mkpoint (x * unit_size, (y+1.0) * unit_size - half_wall, zhigh)
                val pt' = V.mkpoint (x * unit_size, (y+1.0) * unit_size - half_wall, zlow)
                val ptw = V.mkpoint (x * unit_size + half_wall, (y+1.0) * unit_size - half_wall, zhigh)
            in
                updown (pt, pt') @
                (* RIGHT *)
                buildwall(haswall1, ptw, zvec', yvec) @
                (* BOTTOM *)
                buildwall(haswall2, ptw, xvec', zvec')
                
            end
          | BOTTOMRIGHT => let
                val pt = V.mkpoint ((x+1.0) * unit_size - half_wall, y * unit_size, zhigh)
                val pt' = V.mkpoint ((x+1.0) * unit_size - half_wall, y * unit_size, zlow)
                val ptw = V.mkpoint ((x+1.0) * unit_size - half_wall, y * unit_size + half_wall, zhigh)
            in
                updown (pt, pt') @
                (* LEFT *)
                buildwall(haswall1, ptw, zvec', yvec') @
                (* TOP *)
                buildwall(haswall2, ptw, xvec, zvec')
            end
          | BOTTOMLEFT => let
                val pt = V.mkpoint (x * unit_size, y * unit_size, zhigh)
                val pt' = V.mkpoint (x * unit_size, y * unit_size, zlow)
                val ptw = V.mkpoint (x * unit_size + half_wall, y * unit_size + half_wall, zhigh)
            in
                updown (pt, pt') @
                (* RIGHT *)
                buildwall(haswall1, ptw, yvec', zvec') @
                (* TOP *)
                buildwall(haswall2, ptw, zvec', xvec')
            end
    end             
                                                                  
                                                                  
    (* Consider a joint in the top left.  If there is no wall below it then it needs a vertical section on the bottom, if
     * there is then it doesn't (and so on for other directions).  But it's worse than that.  If the joint doesn't have
     * walls in any direction (consider the P polyomino or some edge cases) then it doesn't exist.
     *
     * More confusingly, the wall to the left of a top left joint is the top wall of the square to the left.  And so on.
     *)
                                                                  
    fun jointdirections ((x, y), corner, squares) = let
        val (sqinfo:square) = Array2.sub(squares, x, y)
                              
        (* Compute wall existence in every direction, including in the adjacent squares.  Technically each
         * joint only needs half of these, but to heck with it
         *)
        val hastop = #top sqinfo
        val hasbottom = #bottom sqinfo
        val hasleft = #left sqinfo
        val hasright = #right sqinfo
                       
        (* Top and bottom walls in square to left *)
        val (hastop_l, hasbottom_l) = if x = 0 then
                                          (false, false)
                                      else
                                          (#top (Array2.sub(squares, x-1, y)),
                                           #bottom (Array2.sub (squares, x-1, y)))
                                          
        (* Top and bottom walls in square to right *)
        val (hastop_r, hasbottom_r) = if x = (Array2.nRows(squares)-1) then 
                                          (false, false)
                                      else 
                                          (#top (Array2.sub(squares, x+1, y)),
                                           #bottom (Array2.sub(squares, x+1, y)))
                                          
        (* Left and right walls in square to the top *)
        val (hasleft_u, hasright_u) = if y = (Array2.nCols(squares)-1) then
                                          (false, false)
                                      else
                                          (#left (Array2.sub(squares, x, y+1)),
                                           #right (Array2.sub(squares, x, y+1)))
                                          
        (* Left and right walls in square to the bottom *)
        val (hasleft_b, hasright_b) = if y = 0 then
                                          (false, false)
                                      else
                                          (#left (Array2.sub(squares, x, y-1)),
                                           #right (Array2.sub(squares, x, y-1)))
                                          
    in
        case corner of
            TOPRIGHT => if (hasright orelse hastop orelse hastop_r orelse hasright_u) then
                            SOME (not hastop, not hasright)
                        else
                            NONE
          | TOPLEFT => if (hasleft orelse hastop orelse hastop_l orelse hasleft_u) then
                           SOME (not hastop, not hasleft)
                       else
                           NONE
          | BOTTOMLEFT => if (hasbottom orelse hasleft orelse hasbottom_l orelse hasleft_b) then
                              SOME(not hasbottom, not hasleft)
                          else
                              NONE
          | BOTTOMRIGHT => if (hasbottom orelse hasright orelse hasbottom_r orelse hasright_b) then
                               SOME (not hasbottom, not hasright)
                           else
                               NONE
    end
                                                    
                                                    
    fun buildsquaresjoints (pos, sqinfo) = let
        fun bsj c = case jointdirections(pos, c, sqinfo) of
                        NONE => []
                      | SOME bs => buildsinglejoint(realpair pos, c, bs)
    in 
        foldl op@ [] (List.map bsj [TOPRIGHT, TOPLEFT, BOTTOMRIGHT, BOTTOMLEFT])
    end
                                           
    fun buildallwalls sqinfo = let
        fun buildsquaresborders (x, y, elem, acc) = (buildsquareswalls (realpair (x, y), elem)) :: (buildsquaresjoints ((x, y), sqinfo) :: acc)
    in
        foldl op@ [] (Array2.foldi Array2.RowMajor buildsquaresborders [] (Misc.toRegion sqinfo))
    end
in
    foldl (fn (q, res) => (Q.mkfacets(q, thresh)) @ res) [] (buildallwalls (mksquares data))
end

end
