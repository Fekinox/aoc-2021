structure PQ =
LeftPriorityQFn(
  type priority = int
  fun compare (a,b) = Int.compare (b,a)

  type item = ((int * int) * int)
  fun priority (_,p) = p
)

val (riskGrid, gRows, gCols) =
  let
    val lines = InputHelper.getInput "input"

    val processLine = (List.mapPartial (Int.fromString o Char.toString)) o String.explode

    val init = Array2.fromList (map processLine lines)

    val tabulated = Array2.tabulate Array2.RowMajor
      (Array2.nRows init, Array2.nCols init, (fn (i,j) =>
        ((i, j), Array2.sub (init, i, j))))
  in
    (tabulated, Array2.nRows init, Array2.nCols init)
  end

fun haveVisited visited (i,j) =
  Array2.sub (visited,i,j)

fun dijkstra nborfunc posPrint S T =
  let
    val furthest = ref (~1,~1)
    fun printFurthest (i,j) =
      let
        val (curI,curJ) = !furthest
      in
        if (curI + curJ < i+j)
        then (print ("furthest: " ^ posPrint (i,j) ^ "\n"); furthest := (i,j))
        else ()
      end

    val visited = Array2.array (#1 T + 1, #2 T + 1, false)

    fun dijk Q =
      if (PQ.isEmpty Q)
      then NONE
      else
        let
          val ((position as (i,j),r),Q') = PQ.remove Q
          val _ = printFurthest position
        in
          if (haveVisited visited position) then dijk Q'
          else (
          if (position = T)
          then SOME r
          else 
            (let
              val _ = Array2.update (visited,i,j,true)
              val nbors = nborfunc (position,r)
              val newQueue = List.foldl (fn (x : ((int * int) * int),q) => PQ.insert (x,q)) Q' nbors
            in
              dijk newQueue
            end))
        end
  in
    dijk (PQ.singleton ((0,0),0))
  end

(* part 1: regular nbor function *)
fun neighborsAndRiskTo ((i,j),r) =
  let
    fun inBounds (k,l) =
      (0 <= k andalso k < gRows) andalso
      (0 <= l andalso l < gCols)
    val possible = List.filter inBounds [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]
  in
    List.map (fn (k,l) => let val (_,r2) = Array2.sub (riskGrid,k,l) in ((k,l),r+r2) end) possible
  end

(* part 2: super nbor function *)
fun bonusRisk (i,j,r) = 1 + (((r-1) + (i div gRows) + (j div gCols)) mod 9)
fun superNeighbors ((i,j),r) =
  let
    fun inBounds (k,l) =
      (0 <= k andalso k < gRows*5) andalso
      (0 <= l andalso l < gCols*5)
    val possible = List.filter inBounds [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]
  in
    List.map (fn (k,l) =>
      let val (_,r2) = Array2.sub (riskGrid,k mod gRows,l mod gCols)
      in ((k,l),r + (bonusRisk (k,l,r2))) end) possible
  end

(* val riskToBottomRight = dijkstra neighborsAndRiskTo ((0,0),0) (gRows - 1, gCols - 1) *)
val superRiskToBottomRight = dijkstra superNeighbors (fn (i,j) => (Int.toString i) ^ ", " ^ (Int.toString j))((0,0),0) (5*gRows - 1, 5*gCols - 1)