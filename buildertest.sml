structure BuilderTest =
struct
structure U = UnitTest
structure V = Vector
structure Q = Quad

fun test() = let
    val quads1 = Builder.buildit "test1.desc"
    val _ = U.eqi(1, length quads1)

    val quads2 = Builder.buildit "test2.desc"
    val _ = U.eqi(1, length quads2)
in
    ()
end

end
