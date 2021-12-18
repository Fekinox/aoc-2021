datatype boundStatus =
    BELOW
  | INSIDE
  | ABOVE

val ((minX, maxX), (minY, maxY)) =
  let
    val [_,_,xpos, ypos] = String.tokens (fn c => c = #" " orelse c = #":" orelse c = #",") (
      hd (InputHelper.getInput "input2")
    )

    fun ppToIntPair s =
      let 
        val [_,coords] = String.tokens (fn c => c = #"=") s
        val [min, max] = String.tokens (fn c => c = #".") coords
      in
        (valOf (Int.fromString min), valOf (Int.fromString max))
      end
  in
    (ppToIntPair xpos, ppToIntPair ypos)
  end

fun compareBounds (min, max) x =
  if x < min
  then BELOW  
  else if x > max
  then ABOVE else INSIDE

val (initX, initY) = (0,0)

fun xPosAfterN xvel xinit n = 
  xinit + (if n >= xvel
  then xvel * (xvel + 1) div 2
  else n * (2*xvel - n + 1) div 2)

fun maxXPosAfterN xvel xinit = xPosAfterN xvel xinit xvel

fun yPosAfterN yvel yinit n =
  yinit + (n * (2*yvel - n + 1) div 2)

(* at some point, we'll always come back to the point (0,0) with y-velocity
  * ~v + 1, so we want to get the highest v where ymin <= ~v - 1. the v that
  satisfies this is ~minY + 1
  Credit to Random Guy JCI#7029 in RDL for finding this *)
val maxYVel = ~minY + 1
val maxHeight = (maxYVel * (maxYVel + 1)) div 2

val validYVels =
  let
    val minvel = minY

    fun runner yvel accums times t =
      if (yvel > maxYVel)
      then List.rev accums
      else 
        (case (compareBounds (minY, maxY) (yPosAfterN yvel initY t)) of
          ABOVE => runner yvel accums times (t+1)
        | INSIDE => runner yvel accums (t::times) (t+1)
        | BELOW =>
          (case times of
            [] => runner (yvel+1) accums [] 1
          | _ => runner (yvel+1) ((yvel, List.rev times)::accums) [] 1) )
  in
    runner minvel [] [] 1 
  end

(* Given a stream of yvels and times t where the y position is in bounds, 
   compute (xvel, yvel) where the x position with init vel xvel is in bounds
   at some time step *)

(* Basically, now that we know the min amd max x and y velocities,
   it suffices to just brute force search all possiblities *)
(* This brute force solver does do a couple optimizations, such as
   prematurely stopping once we get an x position that's in bounds
   or if we go out of bounds *)
fun numValidVelocities (yvel : int, times : int list) : int = 
  let
    fun getMinXVel n = if maxXPosAfterN n initX < minX then getMinXVel (n+1) else n
    val minvel = getMinXVel 1 

    fun runner xvel count T =
      if xvel > maxX
      then count 
      else (case T of
        [] => runner (xvel+1) count times
      | (t::ts) =>
        (case (compareBounds (minX, maxX) (xPosAfterN xvel initX t)) of
          BELOW => runner xvel count ts
        | INSIDE => runner (xvel+1) (count + 1) times
        | ABOVE => runner (xvel+1) count times))
  in
    runner minvel 0 times
  end

val allVelocities = ((foldl op+ 0) o (List.map (numValidVelocities))) validYVels