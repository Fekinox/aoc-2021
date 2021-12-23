type amphi = int

(* The LHS represents the hallway, the RHS represents the lists
   of each column *)
type gamestate = (amphi option Vector.vector) * (amphi list Vector.vector)

structure PQ =
LeftPriorityQFn(
  type priority = int
  fun compare (a,b) = Int.compare (b,a)

  type item = (gamestate * int)
  fun priority (_,p) = p
)

fun optionCmp (NONE, NONE) = EQUAL
  | optionCmp (NONE,_) = LESS
  | optionCmp (_,NONE) = GREATER
  | optionCmp (SOME a, SOME b) = Int.compare (a,b)

structure Set =
RedBlackSetFn(
  type ord_key = gamestate

  fun compare ((h1,r1),(h2,r2)) =
    (case Vector.collate optionCmp (h1,h2) of
      EQUAL =>
        Vector.collate (List.collate Int.compare) (r1,r2)
    | c => c)
)

val initState : gamestate =
  (Vector.tabulate (11, fn _ => NONE),
  Vector.fromList
  [
      [1,3],
      [2,3],
      [2,0],
      [1,0]
  ])

val initState2 : gamestate =
  (Vector.tabulate (11, fn _ => NONE),
  Vector.fromList
  [
      [1,3,3,3],
      [2,2,1,3],
      [2,1,0,0],
      [1,0,2,0]
  ])

val initState3 : gamestate =
  (Vector.tabulate (11, fn _ => NONE),
  Vector.fromList
  [
      [1,3,3,0],
      [2,2,1,3],
      [1,1,0,2],
      [3,0,2,0]
  ])

fun printAm 0 = "A"
  | printAm 1 = "B"
  | printAm 2 = "C"
  | printAm 3 = "D"

fun printState (hall,rooms) =
  let
    val _ = (Vector.app (fn NONE => print "." | (SOME a) => print (printAm a)) hall; print "\n")
    fun printRoom rm =
      print ((String.concatWith ", " (map printAm rm)) ^ "\n")
  in
    Vector.app (printRoom) rooms
  end

fun roomIndexToCol i = 2 + (2*i)
val possibleHallwayCols =
  [0,1,3,5,7,9,10]

fun amScore 0 = 1
  | amScore 1 = 10
  | amScore 2 = 100
  | amScore 3 = 1000

(* Cost of moving between room i, position n (from the bottom of the room)
   to slot j in the hallway with the given am *)
fun moveScore am roomSize roomIdx n hallIdx =
  let
    val distanceRoomHall = roomSize - n
    val distanceHall = Int.abs ((roomIndexToCol roomIdx)-hallIdx)
  in
    (amScore am) * (distanceRoomHall + distanceHall)
  end

fun clearPathInHall h (hallCol,roomCol) =
  let
    val dir = Int.sign (hallCol - roomCol)
    fun runner i =
      if hallCol=i
      then true
      else (not (isSome (Vector.sub (h, i)))) andalso runner (i+dir)
  in
    runner roomCol
  end

(* Enumerates all the possible hallway moves and their scores *)
fun hallwayMoves ((hall,rooms) : gamestate, roomSize : int) : (gamestate * int) list =
  let
    fun hallwayMovesRoom i =
      (case (Vector.sub (rooms,i), List.all (fn a => a=i) (Vector.sub (rooms,i))) of
        ([],_) => []
      | (_,true) => []
      | (am::amphs,false) =>
      let
        val newRooms = Vector.update (rooms, i, amphs)
        val possibleHallIndices =
          List.filter (fn j => not (isSome (Vector.sub (hall,j))) andalso clearPathInHall hall (j,roomIndexToCol i)) possibleHallwayCols

        val newHalls = map
          (fn j => (Vector.update (hall, j, SOME am),
                    moveScore am roomSize i (List.length amphs) j))
          possibleHallIndices
      in
        List.map (fn (h,s) => ((h, newRooms), s)) newHalls
      end)
  in
    List.concat (List.tabulate (4, hallwayMovesRoom))
  end

fun roomMoves ((hall,rooms) : gamestate, roomSize : int) : (gamestate * int) list  =
  let
    val roomsWithAms : (int * amphi) list =
      List.mapPartial (fn j => (case Vector.sub (hall, j) of NONE => NONE | SOME a => SOME (j,a))) possibleHallwayCols

    fun movesFor (j, am) =
      let
        val amphs = Vector.sub (rooms, am)
      in
        if List.exists Fn.id
          [List.length amphs = roomSize,
          List.exists (fn a => a <> am) amphs,
          not (clearPathInHall hall (j, roomIndexToCol am))]
        then NONE
        else
          let
            val newHall = Vector.update (hall, j, NONE)
            val newRooms = Vector.update (rooms, am, am::amphs)
            val score = moveScore am roomSize am (List.length amphs) j
            (* val _ = printState (newHall, newRooms) *)
          in
            SOME ((newHall, newRooms), score)
          end
      end
  in
    List.mapPartial movesFor roomsWithAms
  end

fun allMoves S = (hallwayMoves S) @ (roomMoves S)

fun isTerminalState ((hall, rooms),roomSize) =
  let
    fun roomSatisfied am =
      let
        val room = Vector.sub (rooms, am)
      in
        (List.length room = roomSize) andalso (List.all (fn a => am = a) room)
      end
  in
    List.all roomSatisfied [0,1,2,3]
  end

fun minScore (state,rs) =
  let
    val initVis = Set.empty
    val initQ = PQ.singleton (state, 0)

    fun dijkstra (V, Q) =
    if (PQ.isEmpty Q)
    then NONE
    else
    let
      val ((state,score),Q') = PQ.remove Q
    in
      (case (Set.member (V, state), isTerminalState (state,rs)) of
        (true, _) => dijkstra (V, Q')
      | (_, true) => SOME score
      | _ =>
        let
          val V' = Set.add (V, state)
          fun enq ((s,c),q) = PQ.insert ((s,c+score),q)
          val Q'' = foldl enq Q' (allMoves (state,rs))
        in
          dijkstra (V',Q'')
        end)
    end
  in
    dijkstra (initVis, initQ)
  end