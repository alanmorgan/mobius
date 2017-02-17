structure Deform =
struct
  (* Twist 180 degrees clockwise about the x axis for every d units of distance *)
  fun twist180 d = 
      let
          (* Angle of rotation at distance x *)
          fun theta x = Math.pi * (x/d)
          val sin_theta = (Math.sin o theta)
          val cos_theta = (Math.cos o theta)
      in
          fn (x, y, z) => (x, y * cos_theta x - z * sin_theta x, y * sin_theta x + z * cos_theta x)
      end

(* The strip is in the x/y plane.  We bend it around the z axis into a circle.  The strip is of length d, so the point (d, 0, 0)
 * ends up back at (0, 0, 0).  The circle is centered at (0, d/2pi, 0), which complicates the math.  I treat the angle from the
 * center to the origin as 0 radians, but that's actually 3pi/2 or -pi/2 radians in polar coordinates, so that's yet another
 * complication
 * 
 * Something is wrong with this.  The strip is getting "compressed" in the x-axis and it shouldn't be.  Or, at least, I don't
 * want it to be
 *)
  fun bend d =
      let
          fun theta x = 2.0 * Math.pi * (x/d) - Math.pi/2.0
          fun r z = d / (2.0 * Math.pi) - z
          val sin_theta = (Math.sin o theta)
          val cos_theta = (Math.cos o theta)
          val r0 = r 0.0
      in
          fn (x, y, z) => let
                 val new_x = if (Real.== (x,d)) then 0.0 else ((r z) * (cos_theta x))
             in
                 (new_x, y, (r z) * (sin_theta x) + r0)
             end
      end

  fun twistbend d = (bend d) o (twist180 d)
end

