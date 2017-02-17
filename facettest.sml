structure FacetTest =
struct
structure V = Vector
structure U = UnitTest
structure F = Facet

fun test () = let
    (* Note that facets are three points in order, not a point and two vectors *)
    val pt1 = V.mkpoint (2.0, 0.0, 0.0)
    val pt2 = V.mkpoint (1.0, 0.0, 0.0)
    val pt3 = V.mkpoint (1.0, 1.0, 0.0)
    val f = F.mkfacet (pt1, pt2, pt3)

    val pt1' = V.mkpoint (3.0, ~3.0, 1.0)
    val pt2' = V.mkpoint (0.0, 0.0, 0.0)
    val pt3' = V.mkpoint (4.0, 9.0, 2.0)

    val f' = F.mkfacet (pt1', pt2', pt3')

    val normalf' = V.normalize(V.mkvector(~15.0, ~2.0, 39.0));
        
    val _ = U.eqs("facet normal 0.0 0.0 1.0\nouter loop\n  vertex 2.0 0.0 0.0\n  vertex 1.0 0.0 0.0\n  vertex 1.0 1.0 0.0\nendloop\nendfacet\n",
                  F.toString f)
    val _ = U.eqs("facet normal " ^ V.toString(V.v2p normalf') ^ "\nouter loop\n  vertex 3.0 -3.0 1.0\n  vertex 0.0 0.0 0.0\n  vertex 4.0 9.0 2.0\nendloop\nendfacet\n",
                  F.toString f')

    val (pmin, pmax) = F.bbox2pts(F.mergebboxes (F.getbbox f, F.getbbox f'))
    val pmin' = V.mkpoint(0.0, ~3.0, 0.0)
    val pmax' = V.mkpoint(4.0, 9.0, 2.0)
    val _ = U.eqs(V.toString pmin', V.toString pmin)
    val _ = U.eqs(V.toString pmax', V.toString pmax)
            
in
    ()
end

end
