structure Quad : sig
    type q
    val mkquad : (Vector.point * Vector.vector * Vector.vector) -> q
    val toString : q -> string
    val mkfacets : (q * real) -> Facet.f list
end = 
struct

structure V = Vector
structure F = Facet

datatype quad = QUAD of V.point * V.vector * V.vector
type q = quad

fun mkquad (p, v1, v2) = QUAD (p, v1, v2)

fun toString (QUAD (p, v1, v2)) = concat [ "quad ", V.toString p, ", ", V.toString (V.v2p v1), ", ", V.toString (V.v2p v2), "\n" ]

fun mkfacets (q, size) =
    let
        fun subdiv (q, acc, n) =
            case q of QUAD(p, v1, v2) =>
                      let
                          val half = V.scale 0.5
                      in
                          if (V.len v1 > size) then
                              let
                                  val halfv1 = half v1
                              in
                                  subdiv (QUAD (p, halfv1, v2),
                                          subdiv (QUAD (V.v2p (V.addp (p, halfv1)), halfv1, v2), acc, n+1), n+1)
                              end
                          else if (V.len v2 > size) then
                              let
                                  val halfv2 = half v2
                              in
                                  subdiv (QUAD (p, v1, halfv2),
                                          subdiv (QUAD (V.v2p (V.addp(p, halfv2)), v1, halfv2), acc, n+1), n+1)
                              end
                          else
                              [ F.mkfacet (p, V.v2p(V.addp(p, V.addv(v1, v2))), V.v2p(V.addp(p, v1))),
                                F.mkfacet (p, V.v2p(V.addp(p, v2)), V.v2p(V.addp(p, V.addv(v1, v2)))) ] @ acc
                      end
    in
        subdiv (q, [], 0)
    end
end
