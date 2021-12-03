fun threeMezSums (x::y::z::ws) = (x+y+z) :: (threeMezSums (y::z::ws))
  | threeMezSums _ = []

fun depthCount [] = 0
    | depthCount (init::xs) =
    let
        fun depthCount' d prev [] = d
        | depthCount' d prev (y::ys) =
            if y > prev
            then (print "increased\n"; depthCount' (d+1) y ys)
            else (print "decrased\n"; depthCount' d y ys)
    in
        depthCount' 0 init xs
    end

fun getIntLines filename =
  let
    val lines = InputHelper.getInput input

    val ints =
        (map (valOf o Int.fromString)) lines

  in
    ints
  end


val AOC011 =
  let
    val ints = getIntLines "input"
  in
    depthCount ints
  end

val AOC012 =
  let
    val ints = getIntLines "input"
    val sums = threeMezSums ints
  in
    depthCount sums
  end