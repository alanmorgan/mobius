structure DeformTest =
struct
structure D = Deform
structure V = Vector
structure U = UnitTest

fun toString ps = (V.toString (V.mkpoint ps))

fun test() = let
    val origin = (0.0, 0.0, 0.0)

    val _ = U.eqs(toString origin, toString (D.twist180 10.0 origin))
    val _ = U.eqs(toString (10.0, 0.0, 0.0), toString (D.twist180 10.0 (10.0, 0.0, 0.0)))
    val _ = U.eqs(toString (10.0, ~3.0, ~3.0), toString (D.twist180 10.0 (10.0, 3.0, 3.0)))
    val _ = U.eqs(toString (10.0, 3.0, 3.0), toString (D.twist180 5.0 (10.0, 3.0, 3.0)))

    val _ = U.eqs(toString origin, toString (D.bend 10.0 origin))
    val _ = U.eqs(toString origin, toString (D.bend 10.0 (10.0, 0.0, 0.0)))
in
    ()
end

end
