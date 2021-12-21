fun charToBit #"#" = 1
  | charToBit #"." = 0 

val transformLine = (map charToBit) o String.explode

val (iea, image) =
  let
    val (ieaLine::_::gridLines) = InputHelper.getInput "input"
  in
    (Array.fromList (transformLine ieaLine),
     Array2.fromList (map transformLine gridLines))
  end

val binToDec = foldl (fn (b,s) => s*2 + b) 0

fun transform nborlist = Array.sub (iea, binToDec nborlist)
fun transformInt n = Array.sub (iea, n)

fun nborList P (i,j) boundary =
  let
    fun safeIdx (i,j) =
      if (0 <= i andalso i < Array2.nRows P) andalso
         (0 <= j andalso j < Array2.nCols P)
      then Array2.sub (P, i, j)
      else boundary

    val drValues = [~1, 0, 1]
    val deltas = List.concat ((map (fn dr => map (fn dc => (dr, dc)) drValues)) drValues)
  in
    map (fn (dr,dc) => safeIdx (i+dr,j+dc)) deltas
  end

fun printIm im =
  let
    fun printLine ln =
      (Vector.foldl (fn (0,s) => s^"." | (1,s) => s^"#") "" ln) ^ "\n"
  in
    (foldr op^ "" (List.tabulate (Array2.nRows im, (fn i => printLine (Array2.row (im, i))))))
  end

fun runTwoSteps im =
  let
    (* val _ = print (printIm im) *)
    val stepOne = Array2.tabulate Array2.RowMajor
      (Array2.nRows im + 2, Array2.nCols im + 2,
       fn (i,j) => transform (nborList im (i-1,j-1) 0))

    val newBdd = transformInt 0
    (* val _ = print ("new boundary is " ^ Int.toString newBdd ^ "\n")
    val _ = print (printIm stepOne) *)

    val stepTwo = Array2.tabulate Array2.RowMajor
      (Array2.nRows stepOne + 2, Array2.nCols stepOne + 2,
       fn (i,j) => transform (nborList stepOne (i-1,j-1) newBdd))
    (* val _ = print (printIm stepTwo) *)
  in
    stepTwo
  end

fun runNSteps 0 im = im
  | runNSteps n im = (print ("step " ^ Int.toString n ^ "\n"); runNSteps (n-2) (runTwoSteps im))

val numLitPixels = Array2.fold Array2.RowMajor op+ 0