datatype fold = Horiz of int | Vert of int

fun pointCmp ((x1,y1),(x2,y2)) =
  (case Int.compare (x1,x2) of
    EQUAL => Int.compare (y1,y2)
  | c => c)

val (grid, folds) =
  let
    val lines = InputHelper.getInput "content"
    val (pointLines, foldLines) = List.partition (fn s => String.sub (s, 0) <> #"f") lines

    fun stringToPoint s =
      let
        val [x,y] = String.tokens (fn c => c = #",") s
        val (SOME x', SOME y') = (Int.fromString x, Int.fromString y)
      in
        (x', y')
      end

    fun stringToFold s =
      let
        val [_,_,q] = String.tokens Char.isSpace s
      in
        (case (String.tokens (fn c => c = #"=") q) of
          ["x",n] => Vert (valOf (Int.fromString n))
        | ["y",n] => Horiz (valOf (Int.fromString n)))
      end

    val (points, folds) = (List.map stringToPoint pointLines, List.map stringToFold foldLines)
  in
    (points, folds)
  end

local
  (* If a point is below the y line, fold it up *)
  fun horizFold yLine (x,y) =
    if (y > yLine)
    then (x, 2*yLine - y)
    else (x, y)

  (* if a point is right of the x line, fold it left *)
  fun vertFold xLine (x,y) =
    if (x > xLine)
    then (2*xLine - x, y)
    else (x, y)
in
  fun foldPaper (F, G) =
    let
      val mapFn =
        (case F of
          Horiz y => horizFold y
        | Vert y => vertFold y)
    in
      ListHelper.sortNoDups pointCmp (map mapFn G)
    end
end

val theCode =
  let
    (* lol *)
    val messagePoints = List.foldl foldPaper grid folds

    val (maxX,maxY) = List.foldl (fn ((x1,y1),(x2,y2)) => (Int.max (x1,x2),Int.max(y1,y2))) (0,0) messagePoints

    val plot = Array2.array (maxY+1, maxX+1, " ")

    val _ = List.app (fn (x,y) => Array2.update (plot,y,x,"#")) messagePoints
  in
    ArrayHelper.print2D Fn.id plot
  end