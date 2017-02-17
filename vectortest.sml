structure VectorTest =
struct
structure V = Vector
structure U = UnitTest

fun test () = let
    val pt1 = V.mkpoint (1.0, 2.0, 3.0);
    val v1 = V.mkvector (1.0, 2.0, 3.0);
    val ptx = V.mkpoint (1.0, 0.0, 0.0);
    val vx = V.mkvector (1.0, 0.0, 0.0);
    val pty = V.mkpoint (0.0, 1.0, 0.0);
    val ptz = V.mkpoint (0.0, 0.0, 1.0);

    val v2s = V.toString o V.v2p

    val _ = U.eqs (V.toString pt1, "1.0 2.0 3.0")
    val _ = U.eqr (V.len v1, Math.sqrt(14.0))
    val _ = U.eqs (v2s (V.scale 2.0 v1), "2.0 4.0 6.0")
    val _ = U.eqs (v2s (V.addp (pt1, vx)), "2.0 2.0 3.0")
    val _ = U.eqs (v2s (V.addv (v1, vx)), "2.0 2.0 3.0")
    val _ = U.eqs (v2s (V.ncross (V.p2v ptx, V.p2v pty)), V.toString (ptz))
    val _ = U.eqs (V.toString (V.app (fn (x,y,z) => (x - 1.0, y+z, z*2.0)) pt1), "0.0 5.0 6.0")
in
    ()
end

end
