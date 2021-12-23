datatype axis = X | Y | Z
type axisDir = axis * bool

type vec3 = int * int * int
type orientation = vec3 * vec3 * vec3
type xform = orientation * vec3
fun vectorCmp ((x1,y1,z1),(x2,y2,z2)) = List.collate Int.compare ([x1,y1,z1],[x2,y2,z2])

structure Set =
RedBlackSetFn(
  type ord_key = vec3

  val compare = vectorCmp
)

fun axisToVector (X, d) : vec3 = (if d then 1 else ~1, 0, 0)
  | axisToVector (Y, d) = (0, if d then 1 else ~1, 0)
  | axisToVector (Z, d) = (0, 0, if d then 1 else ~1)

val allAxes : axisDir list = [(X,true),(X,false),(Y,true),(Y,false),(Z,true),(Z,false)]
fun upVectors (axis : axis) : (axisDir list) = List.filter (fn (a,d) => a <> axis) allAxes

fun vectorAdd (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)
fun vectorMul s (x,y,z) = (s*x,s*y,s*z)
fun vectorSub (x1,y1,z1) (x2,y2,z2) = (x1-x2,y1-y2,z1-z2)

fun cross ((x1,y1,z1),(x2,y2,z2)) = (y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 - y1*x2)

fun listPrint p L =
  let
    fun lpRunner [] = ""
      | lpRunner [x] = p x
      | lpRunner (x::xs) = (p x) ^ "," ^ (lpRunner xs)
  in
    "[" ^ (lpRunner L) ^ "]"
  end

fun vec3Print (x,y,z) = listPrint Int.toString [x,y,z]

val allOrients : orientation list =
  let
    (* Given a forward vector, pairs it with all its up vectors *)
    fun axesToList (a,d) = List.map (fn (ua,ud) => (axisToVector (a,d), axisToVector (ua,ud))) (upVectors a)

    fun addzVec (fV, uV) = (fV, uV, cross (fV, uV))
  in
    List.concat (map ((map addzVec) o axesToList) allAxes)
  end

val xRot90CW = (axisToVector (Y, true), axisToVector (X, false), axisToVector (Z, true))

fun transformPoint (fV, uV, zV) (x,y,z) =
  vectorAdd (vectorAdd (vectorMul x fV) (vectorMul y uV)) (vectorMul z zV)

fun combineOrients (A, (f2,u2,z2)) =
  (transformPoint A f2, transformPoint A u2, transformPoint A z2)

fun rotatePoints A = map (transformPoint A)
fun transformPoints (A, disp) = Set.map ((vectorAdd disp) o transformPoint A)

val scannerList : (Set.set) Array.array =
  let
    val lines = InputHelper.getInput "input"

    fun parse3DVector s =
      let
        val [x,y,z] = String.tokens (fn c => c = #",") s
      in
        (valOf (Int.fromString x), valOf (Int.fromString y), valOf (Int.fromString z))
      end

    fun parseScanPoint (_::L) =
      let
        fun runner [] = ([], [])
          | runner (l::ls) =
            if l = ""
            then ([], ls) 
            else
              let
                val (points, rest) = runner ls
              in
                ((parse3DVector l) :: points, rest)
              end
      in
        runner L
      end

    fun parseAll [] = []
      | parseAll (ls) =
        let
          val (first, ls2) = parseScanPoint ls
          val rest = parseAll ls2
        in
          (Set.fromList first)::rest
        end
  in
    Array.fromList (parseAll lines)
  end

(* Gets all the ways to map points in ps2 onto points in ps1 with one fixed point *)
fun allDisplacements (ps1 : Set.set) (ps2 : Set.set) =
  let
    val (ls1,ls2) = (Set.listItems ps1, Set.listItems ps2)

    fun runner [] _ res = res
      (* now let's see all the ways to map points in res onto the 2nd point in l... *)
      | runner (_::L) [] res = runner L ls2 res
      (* let's add the map putting point r on point l, and then add all other such maps for rs *)
      | runner (l::L) (r::R) res = runner (l::L) R ((vectorSub l r)::res)
  in
    runner ls1 ls2 []
  end

(* we consider things being relative to scan1 *)
(* If the length is greater than 12, we halt and return the following data:
   - Displacement vector
   - All points transformed to be wrt scanl's position *)
fun mostCommonPoints (scan1, scan2) =
  let
    fun translationsWithOrient A =
      let
        val xformed = Set.map (transformPoint A) scan2
        val displacements = allDisplacements scan1 xformed
      in
        List.map (fn disp => (A,disp)) displacements
      end

    val allXforms = List.concat (List.map translationsWithOrient allOrients)

    fun iterate [] = NONE
      | iterate ((orient,disp)::rest) =
        let
          val xformedPoints = (transformPoints (orient, disp)) scan2
          val commonPoints = Set.intersection (scan1, xformedPoints)
        in
          if Set.numItems commonPoints < 12
          then iterate rest
          else (SOME (disp, xformedPoints))
        end
  in
    iterate allXforms
  end

fun beaconMap scanArray =
  let
    val count = Array.length scanArray
    val pointArray : ((Set.set) option) Array.array = Array.array(count, NONE)
    val _ = Array.update (pointArray, 0, SOME (Array.sub(scanArray, 0)))

    val scannerArray : (vec3 option) Array.array = Array.array(count, NONE)
    val _ = Array.update (scannerArray, 0, SOME (0,0,0))

    val beacons = ref (Array.sub (scanArray, 0))

    fun ins S = 
      beacons := Set.union (!beacons, S)

    fun getUnexplored () =
      ((List.filter (fn i => not (isSome (Array.sub (pointArray, i)))))
      o (List.tabulate)) (count, Fn.id)

    fun findSource [] _ = NONE
      | findSource (s::sources) u =
        (case mostCommonPoints (valOf (Array.sub (pointArray, s)), Array.sub (scanArray, u)) of
          NONE => findSource sources u
        | SOME (disp, comm) => SOME (s,u,disp,comm))

    fun loop [] =
      let
        val totalHits = Array.foldl (fn (v,s) => if isSome v then s+1 else s) 0 pointArray
      in
        if (totalHits <> count)
        then raise Fail "didn't hit every point"
        else ()
      end
      | loop F =
        let
          val unexplored = getUnexplored ()

          val newFoundPoints = List.mapPartial (findSource F) unexplored
            
          val newF = ref []

          fun addPoint (s,d,disp,newPoints : Set.set) =
            let
              val _ = print ("adding " ^ Int.toString d ^ "\n")
              val _ = ins newPoints
              val _ = newF := d::(!newF)
              val sourcedisp = valOf (Array.sub (scannerArray, s))
              val _ = Array.update (scannerArray, d, SOME disp)
            in
              Array.update(pointArray, d, SOME newPoints)
            end

          val _ = List.app addPoint newFoundPoints
        in
          loop (!newF)
        end

    val _ = loop [0]
  in
    (!beacons, ((map valOf) o Array.toList) scannerArray)
  end

fun manhattan ((x1,y1,z1),(x2,y2,z2)) = abs (x1-x2) + abs (y1-y2) + abs(z1-z2)

fun maxManhattan L =
  let
    fun runner [] mx = mx
      | runner (v::vs) mx = runner vs (List.foldl (fn (vk, m) => Int.max (manhattan (v,vk), m)) mx vs)
  in
    runner L
  end