(* Observe that none of the timers ever exceed 9.
   Instead of modeling each fish individually, we can just
   store a number of fish with internal timers at a particular value
   and treat it as a set of equations.
   
   a_0' = a_1
   a_1' = a_2
   a_2' = a_3
   ...
   a_6' = a_7 + a_0
   a_7' = a_8
   a_8' = a_0 *)

val fishArray =
  let
    val [line] = InputHelper.getInput "input"
    val exploded = String.tokens (fn c => c = #",") line

    val resultLine = Array.array (9,0)

    fun addToResLine i = ArrayHelper.mutate (resultLine, i, (fn j => j+1))
  in
    (((List.app addToResLine) o List.mapPartial Int.fromString) exploded; (Array.vector resultLine))
  end

fun processDay fishVec =
  let
    val numSpawn = (Vector.sub (fishVec, 0))
    (* Shift everything down except for 6 and 8, where we add numSpawn *)
    fun mapFn (6,_) = numSpawn + Vector.sub (fishVec, 7)
      | mapFn (8,_) = numSpawn
      | mapFn (i,_) = Vector.sub (fishVec, i+1)
  in
    Vector.mapi mapFn fishVec
  end

fun loopDays 0 fv = fv
  | loopDays i fv = loopDays (i-1) (processDay fv)

val AOC061 = Vector.foldl op+ 0 (loopDays 80 fishArray)