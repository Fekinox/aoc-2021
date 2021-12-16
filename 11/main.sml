val octoGrid =
  let
    val lines = InputHelper.getInput "11-1000-2.in"

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
    val tmp = ref []
    val _ = Array2.appi Array2.RowMajor
      (fn (i,j,_) => if p (i,j) then tmp := (i,j)::(!tmp) else ())
      {base=B,row=0,col=0,nrows=NONE,ncols=NONE}
  in
    !tmp
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
           [] => 0 
         | L => (List.app processFlash newFlashes; (List.length newFlashes) + (loop ()))
      end

    val flashes = loop ()
  in
    flashes
  end

fun flashesAfterSteps 0 B = 0
  | flashesAfterSteps n B = (print ("step " ^ Int.toString n ^ "\n"); (octoStep B) + (flashesAfterSteps (n-1) B))

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
    then (print "simultaneous flash!\n"; 1)
    else (print (Int.toString numFlashes ^ " flashes\n"); 1 + (findSimultaneousFlash B))
  end

val firstSimultaneousFlash =
  findSimultaneousFlash (copy ())