type point = int * int
type line = point * point

type board = int Array2.array

infix 9 ++
fun op++ ((x1,y1),(x2,y2)) = (x1+x2, y1+y2)

(* Not very interesting: just turns each x,y -> z,w into ((x,y),(z,w)) *)
val lineList : line list =
  let
    val lines = InputHelper.getInput "input"

    fun convertPoint s =
      (case (String.tokens (fn c => c = #",") s) of
        [x,y] => 
          (case (Int.fromString x, Int.fromString y) of
            (SOME x', SOME y') => SOME (x',y')
          | _ => NONE)
      | _ => NONE)

    fun processLine s =
      let
        val toks = String.tokens Char.isSpace s
        val [p1,p2] = List.mapPartial convertPoint toks
      in
        (p1, p2)
      end
  in
    map processLine lines
  end

(* A line is perpendicular if the x coords/y coords are the same *)
val filterPerpendiculars =
  List.filter (fn (((x1,y1),(x2,y2)) : line) => (x2 = x1) orelse (y2 = y1))

(* Given a set of lines, counts number of points covered by two lines *)
fun countOverlaps lines =
  let
    (* imperative, but it's the only way to do it nicely *)
    val board = Array2.array (1000,1000,0)
    val doubleCrosses = ref 0

    (* Increase the number of lines passing through point (x,y), incrementing
      dc if it's the first time we cross over twice (we don't care afterward) *)
    fun incPoint (x,y) =
      let
        val oldA = Array2.sub (board, x, y)
      in
        (case oldA of
          0 => Array2.update (board, x, y, oldA + 1)
        | 1 => (doubleCrosses := (!doubleCrosses) + 1;
                Array2.update (board, x, y, oldA + 1))
        | _ => ())
      end

    (* Traces along the line given, incrementing double crosses as it passes *)
    fun plotLine (A as (x1,y1),B as (x2,y2)) =
      let
        val dx = Int.sign (x2 - x1)
        val dy = Int.sign (y2 - y1)

        fun iterLoop (P as (x,y)) =
          if P = B
          then incPoint P
          else (incPoint P; iterLoop (P ++ (dx,dy)))
      in
        iterLoop A
      end
    in
      (List.app (plotLine) lines; !doubleCrosses)
    end

val countOverlaps1 = (countOverlaps o filterPerpendiculars) lineList

val countOverlaps2 = countOverlaps lineList