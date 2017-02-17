structure QuadTest =
struct
structure Q = Quad
structure V = Vector
structure U = UnitTest
structure F = Facet

fun test() = let
    val p1 = V.mkpoint (0.0, 0.0, 0.0)
    val v1 = V.mkvector (1.0, 0.0, 0.0)
    val v2 = V.mkvector (0.0, 1.0, 0.0)

    val q1 = Q.mkquad (p1, v1, v2)
    val _ = U.eqs("quad 0.0 0.0 0.0, 1.0 0.0 0.0, 0.0 1.0 0.0\n",
                  Q.toString(q1))

    val facets = Q.mkfacets (q1, 0.5)
    val _ = U.eqi (8, length facets)
in
    ()
end
                                   
end
