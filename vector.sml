structure Vector : sig
    type point
    type vector
    val v2p : vector -> point
    val p2v : point -> vector
    val mkpoint : (real * real * real) -> point
    val mkvector : (real * real * real) -> vector
    val mkvector2 : (point * point) -> vector
    val unmkpoint : point -> (real * real * real)
    val unmkvector : vector -> (real * real * real)
    val cross : (vector * vector) -> vector
    val ncross : (vector * vector) -> vector
    val normalize : vector -> vector
    val scale : real -> vector -> vector
    val addp : (point * vector) -> vector
    val addv : (vector * vector) -> vector
    val len : vector -> real
    val app : (real * real * real -> real * real * real) -> point -> point
    val toString : point -> string
end =
struct
datatype pt = POINT of real * real * real
datatype vc = VECTOR of real * real * real

type point = pt
type vector = vc

(* Remember that integers and reals use ~ instead of -.  God, I hate SML sometimes.
 * There is probably a way to do this with StringCvt.  Screw it.  Scan the string
 * and convert ~ to -
 *)
fun rtoString r = implode (map (fn c => if c = #"~" then #"-" else c) (explode (Real.toString r)))

fun mkpoint xyz = POINT xyz
fun mkvector xyz = VECTOR xyz
fun v2p (VECTOR v) = mkpoint v
fun p2v (POINT v) = mkvector v
fun unmkpoint (POINT xyz) = xyz
fun unmkvector (VECTOR xyz) = xyz
fun mkvector2 (POINT (x1, y1, z1), POINT(x2, y2, z2)) = VECTOR(x2-x1, y2-y1, z2-z1)

fun len v = case v of VECTOR (x, y, z) => Math.sqrt (x*x + y*y + z*z)
fun addp (POINT (x1, y1, z1), VECTOR(x2, y2, z2)) = VECTOR (x1+x2, y1+y2, z1+z2)
fun addv (VECTOR (x1, y1, z1), VECTOR(x2, y2, z2)) = VECTOR (x1+x2, y1+y2, z1+z2)

fun scale s (VECTOR (x, y, z)) = VECTOR(s * x, s * y, s * z)
fun normalize v = scale (1.0/(len v)) v

fun cross (VECTOR(a1, a2, a3), VECTOR(b1, b2, b3)) = VECTOR(a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1)
    
fun ncross vs = normalize (cross vs)

fun toString p = case p of POINT (r1, r2, r3) => concat [ rtoString r1, " ", rtoString r2, " ", rtoString r3 ]

fun app f (POINT p) = POINT (f p)


end
