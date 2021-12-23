(* you could feasibly generalize this to any dimension of
   vectors by making it an int list instead of a vec3, but that's
   work *)
type vec3 = int * int * int
(* minimum coordinate, maximum coordinate *)
type aabb = vec3 * vec3

type instruction = bool * aabb

datatype intersection = Disjoint | Section of aabb

fun size ((xm,ym,zm),(xM,yM,zM)) =
  (xM-xm) * (yM-ym) * (zM-zm)

fun printPoint (x,y,z) = String.concatWith "," (map Int.toString [x,y,z])
fun printAABB ((pm,pM)) =
  "(" ^ printPoint pm ^ "),(" ^ printPoint pM ^ ")"

fun parseInstruction (s : string) : instruction =
  let
    val [status,ranges] = String.tokens (fn c => c = #" ") s
    val status = if status = "on" then true else false
    val (_::xmin::xmax::_::ymin::ymax::_::zmin::zmax::_) =
      String.tokens (fn c => c = #"." orelse c = #"=" orelse c = #",") ranges

    val conv = valOf o Int.fromString
  in
    (* We add 1 just to include the cubes at the boundary *)
    (status, ((conv xmin, conv ymin, conv zmin), (conv xmax + 1, conv ymax + 1, conv zmax + 1)))
  end

(* Splits a square AABB at the given point *)
fun splitAABB (sx,sy,sz) ((xmin,ymin,zmin),(xmax,ymax,zmax)) =
  let
    fun cart ([],_) = []
      | cart (_,[]) = []
      | cart (l::L,R) = ((map (fn r => (l,r))) R) @ (cart (L,R))

    (* if the z axis for instance doesn't split the cube,
       just return it; otherwise split it into [min,s],[s,max] *)
    fun splitR (min,max,s) =
      if (s <= min orelse s >= max)
      then [(min,max)]
      else [(min,s),(s,max)]

    fun cmb ((xm,xM),((ym,yM),(zm,zM))) =
      ((xm,ym,zm),(xM,yM,zM))

    val (xR,yR,zR) = (splitR (xmin,xmax,sx),splitR (ymin,ymax,sy), splitR (zmin,zmax,sz))

    val carted = cart (xR, cart (yR,zR))
  in
    map cmb carted
  end

(* Tests if A is fully contained within B *)
fun isSubset (A as ((xm1,ym1,zm1),(xM1,yM1,zM1)),
              B as ((xm2,ym2,zm2),(xM2,yM2,zM2))) =
  let
    fun contained (min1,min2,max1,max2) =
      (min2 <= min1) andalso (max1 <= max2)
  in
    (contained (xm1,xm2,xM1,xM2) andalso
    contained (ym1,ym2,yM1,yM2) andalso
    contained(zm1,zm2,zM1,zM2))
  end

(* Computes the area of intersection if it exists *)
fun intersectAABB (A as ((xm1,ym1,zm1),(xM1,yM1,zM1)),
                   B as ((xm2,ym2,zm2),(xM2,yM2,zM2))) =
  let
    fun separating (min1,min2,max1,max2) =
      (min1 < max2) andalso (min2 < max1)
  in
    if (separating (xm1,xm2,xM1,xM2) andalso
        separating (ym1,ym2,yM1,yM2) andalso
        separating (zm1,zm2,zM1,zM2))
    then
      let
        val imax = (Int.min (xM1,xM2),
                    Int.min (yM1,yM2),
                    Int.min (zM1,zM2))
        val imin = (Int.max (xm1,xm2),
                    Int.max (ym1,ym2),
                    Int.max (zm1,zm2))
      in
        Section (imin, imax)
      end
    else Disjoint 
  end

(* The basic gist of the algorithm is that we take the cuboid
   we're adding, a, remove all the disjoint cubes contained
   within a, and either add a itself (if we're turning on)
   or remove a (if we're turning off). Of course not every
   cube in our set of cubes is going to be either completely
   disjoint or completely inside a, so in those cases we pop
   off that cube, split it across every separating plane of A,
   and re-add the new cubes. Each cube made from the splitting
   is either disjoint or contained *)
fun runInstruction ((true,a),[]) = [a]
  | runInstruction ((false,a),[]) = []
  | runInstruction (A as (_,a),(c::cs)) =
    (case (intersectAABB (a,c), isSubset (c,a)) of
      (Disjoint,_) => c::(runInstruction (A,cs))
    | (_,true) => runInstruction (A,cs)
    | (Section (m,M),_) =>
      let
        val splitC =
          (List.concat o
          (List.map (splitAABB M)) o
          (splitAABB m)) c
      in
        runInstruction (A,splitC@cs)
      end)

val instructions = map parseInstruction (InputHelper.getInput "input")

val smalls = List.filter
  (fn (_,((xm,ym,zm),(xM,yM,zM))) =>
    50 >= (((foldl Int.max 0) o (map Int.abs)) [xm,ym,zm,xM,yM,zM])) instructions


val runAllInstructions = foldl runInstruction []

val totalVolume = foldl (fn (c,s) => size c + s) 0

val part1 = (totalVolume o runAllInstructions) smalls
val part2 = (totalVolume o runAllInstructions) instructions