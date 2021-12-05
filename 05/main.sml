type point = int * int
type line = point * point

type board = int Array2.array

infix 9 ++
fun op++ ((x1,y1),(x2,y2)) = (x1+x2, y1+y2)

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

val filterPerpendiculars =
  List.filter (fn (((x1,y1),(x2,y2)) : line) => (x2 = x1) orelse (y2 = y1))

fun incPoint B (x,y) =
  let
    val oldA = Array2.sub (B, x, y)
  in
    Array2.update (B, x, y, oldA + 1)
  end

fun pointToString (x,y) = "(" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")"

fun plotLine Bd (A as (x1,y1),B as (x2,y2)) =
  let
    val dx = Int.sign (x2 - x1)
    val dy = Int.sign (y2 - y1)

    fun iterLoop (P as (x,y)) =
      if P = B
      then incPoint Bd P
      else (incPoint Bd P; iterLoop (P ++ (dx,dy)))
  in
    iterLoop A
  end

fun countDoubleCross B =
  let
    fun foldFn (crosses, sum) = if crosses >= 2 then sum+1 else sum
  in
    Array2.fold Array2.RowMajor foldFn 0 B
  end

val countOverlaps1 =
  let
    val initBoard = Array2.array (1000,1000,0)

    val cleanedLines = filterPerpendiculars lineList

    val _ = List.app (plotLine initBoard) cleanedLines
  in
    countDoubleCross initBoard
  end

val countOverlaps2 =
  let
    val initBoard = Array2.array (1000,1000,0)

    val _ = List.app (plotLine initBoard) lineList
  in
    countDoubleCross initBoard
  end