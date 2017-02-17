structure Mobius =
struct
  structure V = Vector
  structure F = Facet
  structure D = Deform

  val thresh = 1.0

  fun write facets outfile = let
      val f = TextIO.openOut outfile
  in
      TextIO.output (f, "solid block\n");
      TextIO.output (f, concat (map F.toString facets));
      TextIO.output (f, "endsolid block\n");
      TextIO.closeOut f
  end

  fun teststrip () = let
      val xlength = 3.5
      val ylength = 0.2
      val zlength = 2.0
      val xvec = V.mkvector (xlength, 0.0, 0.0)
      val yvec = V.mkvector (0.0, ylength, 0.0)
      val zvec = V.mkvector (0.0, 0.0, zlength)
      val qtop = Quad.mkquad (V.mkpoint (0.0, ylength/2.0, ~zlength/2.0), zvec, xvec)
      val qbottom = Quad.mkquad (V.mkpoint (0.0, ~ylength/2.0, ~zlength/2.0), xvec, zvec)
      val qfront = Quad.mkquad (V.mkpoint (0.0, ~ylength/2.0, zlength/2.0), xvec, yvec)
      val qback = Quad.mkquad (V.mkpoint (0.0, ~ylength/2.0, ~zlength/2.0), yvec, xvec)
      val qleft = Quad.mkquad (V.mkpoint (0.0, ~ylength/2.0, ~zlength/2.0), zvec, yvec);
      val qright = Quad.mkquad (V.mkpoint (xlength, ~ylength/2.0, ~zlength/2.0), yvec, zvec);

      val base = List.concat (map (fn q => Quad.mkfacets (q, 0.25)) [qfront, qback, qtop, qbottom, qleft, qright])
                 
      val facets = List.concat [ base,
                                 map (F.app (D.twist180 (xlength*2.0))) base,
                                 map (F.app ((fn (x, y, z) => (x, y+5.0, z)) o (D.bend (xlength*2.0)))) base,
                                 map (F.app ((fn (x, y, z) => (x, y+10.0, z)) o (D.twistbend (xlength*2.0)))) base]
  in
      write facets "teststrip.stl"
  end

  val xstretch = 1.5

  fun go (infile, outfile)  = let
      val facets = Builder.buildit infile
      val bbox = foldl (fn (e, b) => F.mergebboxes (F.getbbox e, b)) (F.getbbox (hd facets)) facets
      val (pmin, pmax) = F.bbox2pts bbox
      val (xmin, ymin, zmin) = V.unmkpoint pmin
      val (xmax, ymax, zmax) = V.unmkpoint pmax

      fun shift (x, y, z) = ((x-xmin)*xstretch, y-ymin-(ymax-ymin)/2.0, z-zmin-(zmax-zmin)/2.0)

      val f' = map (F.app ((D.twistbend (xstretch*(xmax-xmin))) o shift)) facets
  in
      write f' outfile
  end

  fun run infile = go (infile ^ ".desc", infile ^ ".stl")
end
