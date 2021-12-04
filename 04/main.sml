(* Turn string lines into a list of the numbers and a list of 2D lists
   representing the bingo boards *)

val (numberList, bingoBoards) =
  let
    val (numListLine::_::bingoBoardLines) = InputHelper.getInput "input"

    val convertedNumberList =
      let
        val exploded = String.tokens (fn c => c = #",") numListLine
      in
        map (valOf o Int.fromString) exploded
      end

    fun splitAtEmpty [] = ([],[])
      | splitAtEmpty ("\n"::s) = ([],s)
      | splitAtEmpty (x::s) =
      let
        val (a,b) = splitAtEmpty s
      in
        (x::a,b)
      end

    fun repeatSplit L =
      (case splitAtEmpty L of
        ([],_) => []
      | (B,s) => B::(repeatSplit s))

    val asBoard =
      let
        fun rowToBoardRow r =
          let
            val exploded = String.tokens (fn c => c = #" ") r
          in
            (map (fn s => (false, valOf (Int.fromString s)))) exploded
          end

        val rowsConverted = map (map rowToBoardRow) (repeatSplit bingoBoardLines)

        val boardsConverted = map Array2.fromList rowsConverted
      in
        boardsConverted
      end
  in
    (convertedNumberList, asBoard)
  end

(* Mark a particular number in the board *)
fun markNumber n board =
  let
    fun modFn (b,x) = if x = n then (true, x) else (b, x)
  in
    Array2.modify Array2.RowMajor modFn board
  end

(* Checks a board for a win and returns it *)
local
  fun checkLine (r,c,dr,dc) board =
    let
      fun tabFn i =
        let
          val (b,_) = Array2.sub (board, r+i*dr, c+i*dc)
        in
          b
        end
    in
      Vector.all Fn.id (Vector.tabulate (5, tabFn))
    end

  fun checkHorizLines board =
    Vector.exists Fn.id (
      Vector.tabulate (5, (fn i => checkLine (i,0,0,1) board))
    )

  fun checkVerticalLines board =
    Vector.exists Fn.id (
      Vector.tabulate (5, (fn i => checkLine (0,i,1,0) board))
    )

  fun checkDiagLines board =
    (checkLine (0,0,1,1) board) orelse (checkLine (0,4,1,~1) board)
in
  fun checkWin board =
    List.exists Fn.id [checkHorizLines board, checkVerticalLines board, checkDiagLines board]
end

fun boardScore winningBoard lastNumber =
  let
    fun foldFn ((true,x),sum) = sum | foldFn ((false,x),sum) = x+sum
  in
    (Array2.fold Array2.RowMajor foldFn 0 winningBoard) * lastNumber
  end

fun firstBingoWin [] boards = raise Fail "no more numbers!"
  | firstBingoWin (n::ns) boards =
    let
      (* Call out a number and mark it on all boards *)
      val _ = List.app (markNumber n) boards

      (* Check for a winner *)
      val winner = List.find checkWin boards
    in
      (case winner of
        NONE => firstBingoWin ns boards
      | (SOME w) => boardScore w n)
    end

fun lastBingoWin [] boards = raise Fail "no more numbers!"
  | lastBingoWin (n::ns) boards =
    let
      (* Call out a number and mark it on all boards *)
      val _ = List.app (markNumber n) boards

      (* If we filter and there's nothing left, then there is exactly
         one board left; that board's the last winner *)
      val remainingBoards = List.filter (fn b => not (checkWin b)) boards

      val _ = print ((Int.toString (List.length remainingBoards)) ^ " boards left\n")
    in
      (case remainingBoards of
        (_::_) => lastBingoWin ns remainingBoards
      | _ => let val [finalBoard] = boards in boardScore finalBoard n end)
    end