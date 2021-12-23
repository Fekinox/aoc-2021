val (p1Start, p2Start) =
  let
    val [p1s,p2s] = InputHelper.getInput "input" 
    fun convert s = valOf (Int.fromString (List.sub (String.tokens Char.isSpace s, 4)))
  in
    (convert p1s, convert p2s)
  end

signature DICE =
sig
  type t

  val init : t
  val roll : t -> int * t
  val rollN : (t * int) -> (int list * t)
end

structure DetDice :> DICE =
struct
  type t = int

  val init = 0
  fun roll d = (d+1, ((d+1) mod 100))
  fun rollN (d, n) =
    let
      fun runner (d,0) r = (List.rev r,d)
        | runner (d,n) r =
            let val (v,d') = roll d in runner (d',n-1) (v::r) end
    in
      runner (d,n) []
    end
end

fun dirac (p1Pos, p2Pos) =
  let
    val initDice = DetDice.init

    val rolls = ref 0

    val p1pos = ref p1Pos
    val p2pos = ref p2Pos
    val p1score = ref 0
    val p2score = ref 0

    fun runner dice p1Turn =
      let
        val (roll, d1) = DetDice.rollN (dice, 3)
        val newDistance = foldl op+ 0 roll
        val _ = rolls := (!rolls) + 3
      in
        if p1Turn
        then
          let
            val _ = p1pos := (((!p1pos) + newDistance - 1) mod 10) + 1
            val _ = p1score := (!p1score) + (!p1pos)
          in
            if (!p1score >= 1000)
            then (!p2score) * (!rolls)
            else runner d1 (not p1Turn)
          end
        else
          let
            val _ = p2pos := (((!p2pos) + newDistance - 1) mod 10) + 1
            val _ = p2score := (!p2score) + (!p2pos)
          in
            if (!p2score >= 1000)
            then (!p1score) * (!rolls)
            else runner d1 (not p1Turn)
          end
      end
  in
    runner initDice true
  end

(* ok, turns out part 1 has hilariously little to do with
   part 2 *)

(* only depends on the initial position now, the dice
   are now irrelevant *)
fun realDirac scoreLimit positions (p1Start, p2Start) =
  let
    (* trinomials yuck *)
    val diracRolls =
      Vector.map Int.toLarge (Vector.fromList [1,3,6,7,6,3,1])

    (* Maintain a cache indexed by the following data:
       pos1, pos2: position of current player,
                   position of other player
       score1, score2: score of current, score of other
       turn: current player to move: 0 for p1, 1 for p2 *)
    fun freshCache () = Array.array (positions*positions*scoreLimit*scoreLimit*2, Int.toLarge 0)

    val cache = freshCache ()
    val p1Wins = ref 0
    val p2Wins = ref 0

    fun addW count v = count := (!count) + v

    (* cope *)
    fun cacheIdx (pos1, pos2, score1, score2, turn) =
      pos1 +
      (pos2*positions) +
      (score1*positions*positions) +
      (score2*scoreLimit*positions*positions) +
      (turn*scoreLimit*scoreLimit*positions*positions)

    fun idxToPos i =
      (i mod positions,
       (i mod (positions*positions)) div positions,
       (i mod (scoreLimit*positions*positions)) div (positions*positions),
       (i mod (scoreLimit*scoreLimit*positions*positions)) div (scoreLimit*positions*positions),
       (i div (scoreLimit*scoreLimit*positions*positions)))

    fun cacheSub C S =
      Array.sub (cache, cacheIdx S)

    fun cacheAdd C (S, v) =
      ArrayHelper.mutate (cache, cacheIdx S, (fn k => (k+v)))

    (* Start by adding one point for the initial state
       (players at starting positions, scores 0, P1 to move *)
    val _ = cacheAdd cache ((p1Start - 1, p2Start - 1, 0, 0, 0), 1)

    fun flip 1 = 0
      | flip 0 = 1

    fun runTurn (i, 0, deltas) = deltas
      | runTurn (i, k, deltas) =
      let
        (* Convert the index into the data *)
        val (S as (pos1,pos2,score1,score2,turn)) = idxToPos i

        (* There are now k fewer universes where the game is in
           state S *)
        val _ = cacheAdd deltas (S, ~k)

        (* Iterate for each roll of the die *)
        fun appFn (rollValue, rollTimes) =
          let
            val newPos = (pos1+rollValue+3) mod 10
            val newScore = score1 + (newPos + 1)
            val newUniverses = rollTimes * k

            (* We swap the players: the other player
               is now the one to move, and the current
               player has an updated position/score *)
            val newState = 
              (pos2, newPos,
               score2, newScore,
               flip turn)
          in
            (case (newScore >= scoreLimit, turn) of
              (true, 0) => addW p1Wins newUniverses
            | (true, 1) => addW p2Wins newUniverses
            | _ =>
              cacheAdd deltas
                (newState, newUniverses))
          end

        val _ = Vector.appi appFn diracRolls
      in
        deltas
      end

    (* Run one step of evaluation *)
    (* For each element of the cache we have so far,
       run a turn on it, accumulating a deltas of all the
       changes to make *)
    fun runStep () =
      let
        val C = Array.foldli runTurn (freshCache ()) cache
      in
        Array.modifyi (fn (i,k) => k + Array.sub (C, i))
      end

    (* If everything in the cache is empty we've calculated 
       all the universes *)
    fun canMove () =
      (Array.foldl op+ 0 cache) <> 0

    fun runAll () =
      if canMove ()
      then (runStep (); runAll ())
      else IntInf.max (!p1Wins, !p2Wins)
  in
    runAll ()
  end