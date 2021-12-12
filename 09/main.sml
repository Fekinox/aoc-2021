val heightMap =
  let
    val lines = InputHelper.getInput "input"

    val processLine = ((List.mapPartial (Int.fromString o Char.toString) o String.explode))
  in
    Array2.fromList (map processLine lines)
  end

fun isSafe B (i, j) =
  ((0 <= i andalso i < Array2.nRows B) andalso
  (0 <= j andalso j < Array2.nCols B))

fun safeIdx B (i, j) =
  if isSafe B (i, j)
  then SOME (Array2.sub (B, i, j))
  else NONE

fun neighbors (i, j) B =
  List.mapPartial (safeIdx B) [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

fun neighborIdx (i, j) B =
  List.filter (isSafe B) [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

(* If (i, j) is a low point return SOME (risk level) otws NONE *)
fun riskLevel (i, j) B =
  let
    val height = Array2.sub (B, i, j)
  in
    if (foldl Int.min 9 (neighbors (i, j) B) > height)
    then SOME (height + 1)
    else NONE
  end

val lowPoints =
  let
    val allIndices = List.concat (
        List.tabulate (Array2.nRows heightMap, (fn i =>
          List.tabulate (Array2.nCols heightMap, (fn j =>
            (i, j)))))
      )
    fun isLowPoint (i, j) =
      (foldl Int.min 9 (neighbors (i, j) heightMap)) > (Array2.sub (heightMap, i, j))
  in
    List.filter isLowPoint allIndices
  end

val sumRisks = ((foldl (fn (a,acc) => acc + (1+a)) 0) o (List.mapPartial (safeIdx heightMap))) lowPoints

fun quicksort cmp [] = []
  | quicksort cmp (x::xs) =
    let
      val (A, B) = List.partition (fn i => (cmp (x, i)) = LESS) xs
    in
      (quicksort cmp A) @ (x :: (quicksort cmp B))
    end

fun removeDups cmp L =
  let
    val sorted = quicksort cmp L

    fun dups (x::y::zs) =
      if (cmp (x,y) = EQUAL)
      then dups (y::zs)
      else x :: (dups (y::zs))
      | dups L = L
  in
    dups sorted
  end

fun pointCmp ((i,j),(k,l)) =
  (case Int.compare (i,k) of
    EQUAL => Int.compare (j,l)
  | x => x)

val basins =
  let
    val visitedSet = Array2.array (Array2.nRows heightMap, Array2.nCols heightMap, false)

    fun heightAt (i, j) = Array2.sub (heightMap, i, j)

    (* Gets the neighbors with strictly higher height *)
    fun increasingNbors P =
      let
        val nbors = neighborIdx P heightMap
      in
        List.filter (fn N => 
          (heightAt N <> 9) andalso
          ((heightAt N) > (heightAt P))) nbors
      end

    fun bfs F bas =
      let
        val cleanedFrontier = List.filter (fn (i,j) => not (Array2.sub (visitedSet, i, j))) F
      in
        (case F of
          [] => bas
        | _  =>
          let
            val _ = List.app (fn (i,j) => Array2.update (visitedSet, i, j, true)) cleanedFrontier
            val newF = ((removeDups pointCmp) o List.concat o (map increasingNbors)) cleanedFrontier 
          in
            bfs newF (cleanedFrontier @ bas)
          end)
      end
  in
    map (fn p => bfs [p] []) lowPoints
  end

val prodThreeLargest =
  let
    val basinSizes = map (List.length) basins
    val (x::y::z::_) = quicksort Int.compare basinSizes
  in
    x*y*z
  end