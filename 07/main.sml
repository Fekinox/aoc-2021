val crabPositions =
  let
    val [line] = InputHelper.getInput "input"
  in
    ((List.mapPartial Int.fromString) o (String.tokens (fn c => c = #","))) line
  end

fun moveFuelCostLinear i =
  foldl (fn (pos, total) => total + (abs (pos - i))) 0

fun moveFuelCostQuad i =
  let
    fun foldFn (pos, total) = let val d = abs (pos - i) in total + (d * (d+1) div 2) end
  in
    foldl foldFn 0
  end

fun quickselect k [] = NONE
  | quickselect k (L as (x::xs)) =
    let
      val (lesser, greater) = List.partition (fn i => i <= x) L
      val (lesser, pivot) = List.partition (fn i => i < x) lesser
      val (lessLen,pivLen) = (List.length lesser, List.length pivot)
    in
      (case (k < lessLen, k < lessLen + pivLen) of
        (true, _) => quickselect k lesser
      | (_, true) => SOME x
      | _         => quickselect (k - lessLen - pivLen) greater)
    end

fun median L = quickselect (List.length L div 2) L

fun mean L = (foldl op+ 0 L) div (List.length L)