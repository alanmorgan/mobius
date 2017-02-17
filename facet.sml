structure Facet : sig
    type f
    type bbox
    val toString: f -> string
    val mkfacet : (Vector.point * Vector.point * Vector.point) -> f
    val app : (real * real * real -> real * real * real) -> f -> f
    val getbbox : f -> bbox
    val bbox2pts : bbox -> (Vector.point * Vector.point)
    val mergebboxes : (bbox * bbox) -> bbox
end =
struct

structure V = Vector

(* Handy types *)
datatype triangle = TRIANGLE of V.point * V.point * V.point
datatype facet = FACET of V.vector * triangle
datatype bounding_box = BBOX of V.point * V.point

type f = facet
type bbox = bounding_box

fun pick f (p1, p2) = let
    val (x1, y1, z1) = V.unmkpoint p1
    val (x2, y2, z2) = V.unmkpoint p2
in
    V.mkpoint (f(x1, x2), f(y1, y2), f(z1,z2))
end

fun pickmax pts = pick (Real.max) pts
fun pickmin pts = pick (Real.min) pts
fun mergebboxes (BBOX(p1, p2), BBOX(p3, p4)) = BBOX(pickmin(p1, p3), pickmax(p2,p4))
fun bbox2pts (BBOX pts) = pts
fun getbbox (FACET (_, TRIANGLE(p1, p2, p3))) = BBOX(pickmin (p1, pickmin(p2, p3)), pickmax(p1, pickmax(p2, p3)))

fun toString f = let
    fun point_name_to_string name p = concat [ name, " ", V.toString p, "\n" ]

    val vertex_to_string = point_name_to_string "  vertex"

    fun normal_to_string n = point_name_to_string "normal" (V.v2p n)

    fun tri_to_string t =
        case t of TRIANGLE (v1, v2, v3) =>
                  concat [ "outer loop\n", vertex_to_string v1, vertex_to_string v2, vertex_to_string v3, "endloop\n" ]

in
    case f of FACET(n, t) =>
              concat [ "facet ", normal_to_string n, tri_to_string t, "endfacet\n" ]
end


fun mkfacet pts = let
    fun compute_normal (p1, p2, p3) = V.ncross (V.mkvector2 (p2, p1), V.mkvector2 (p2, p3))
in
    FACET(compute_normal pts, TRIANGLE pts)
end

fun app f fct = case fct of FACET (_, TRIANGLE (p1, p2, p3)) => mkfacet (V.app f p1, V.app f p2, V.app f p3)
end
              
