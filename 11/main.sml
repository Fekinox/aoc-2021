val octoGrid =
  let
    val lines = InputHelper.getInput "input"

    val processLine = (List.mapPartial (Int.fromString o Char.toString)) o String.explode
  in
    Array2.fromList (map processLine lines)
  end

fun copy () =
  let
    val newGrid = Array2.tabulate Array2.RowMajor
      (Array2.nRows octoGrid, Array2.nCols octoGrid,
        (fn (i,j) => Array2.sub (octoGrid, i, j))) 
  in
    newGrid
  end

fun isValid B (i, j) = 
  (0 <= i andalso i < Array2.nRows B) andalso
  (0 <= j andalso j < Array2.nCols B)

local
  val drValues = [~1, 0, 1]
  val deltas = List.concat ((map (fn dr => map (fn dc => (dr, dc)) drValues)) drValues)
  val deltas = List.filter (fn (0,0) => false | _ => true) deltas
in
  fun nborIdx B (i, j) =
    let
      val allAdj = map (fn (dr,dc) => (i+dr,j+dc)) deltas
    in
      List.filter (isValid B) allAdj
    end
end

(* Get all indices satisfying a property *)
fun gridFilter p B =
  let
    val indices = List.concat(
      List.tabulate (Array2.nRows B, (fn dr =>
       List.tabulate (Array2.nCols B, (fn dc => (dr, dc)))))
    )
  in
    List.filter p indices
  end

fun octoStep B =
  let
    val flashTable = Array2.array (Array2.nRows B, Array2.nCols B, false)
    fun alreadyFlashed (i, j) = Array2.sub (flashTable, i, j)

    fun increment (i, j) = ArrayHelper.mutate2D (B, i, j, (fn x => x+1))
    fun reset (i, j) = ArrayHelper.mutate2D (B, i, j, (fn _ => 0))

    (* First increment the energy of all octopi by 1 *)
    val _ = Array2.modify Array2.RowMajor (fn x => x+1) B

    (* Then infinitely loop while we still have octopi with >9 energy
       who haven't flashed yet *)
    fun processFlash (i, j) =
      (Array2.update (flashTable, i, j, true);
       List.app increment (nborIdx B (i, j)))

    fun loop () =
      let
        val newFlashes = gridFilter (fn (i,j) => (Array2.sub (B, i, j) > 9 andalso (not (alreadyFlashed (i, j))))) B
      in
        case newFlashes of
           [] => []
         | L => (List.app processFlash newFlashes; newFlashes @ (loop ()))
      end

    val flashes = loop ()

    val _ = List.app reset flashes
  in
    List.length flashes
  end

fun flashesAfterSteps 0 B = 0
  | flashesAfterSteps n B = (octoStep B) + (flashesAfterSteps (n-1) B)

val oneHundredSteps =
  let
    val B = copy ()
  in
    flashesAfterSteps 100 B
  end

fun findSimultaneousFlash B =
  let
    val numFlashes = octoStep B
  in
    if (numFlashes = (Array2.nRows B * Array2.nCols B))
    then 1
    else 1 + (findSimultaneousFlash B)
  end

val firstSimultaneousFlash =
  findSimultaneousFlash (copy ())