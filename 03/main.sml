val (numLines, bitLines) : int * int list list =
  let
    val lines = InputHelper.getInput "input"
    val len = List.length lines

    fun toBitList s =
      let
        val exploded = String.explode s
      in
        ((map (fn c => Char.ord c - 48)) o (List.filter Char.isDigit)) exploded
      end
  in
    (len, map toBitList lines)
  end

fun zipWith f ([],_) = []
  | zipWith f (_,[]) = []
  | zipWith f (x::xs,y::ys) = (f (x,y))::(zipWith f (xs,ys))

fun bitListToInt S =
  let
    fun cmb (b, (res, coeff)) = (res + b*coeff, coeff*2)

    val (r,_) = foldr cmb (0,1) S
  in
    r
  end

val mostCommonBits =
  let
    val mid = numLines div 2
    val cols = List.length (hd bitLines)

    val init = List.tabulate (cols, (fn _ => 0))
    
    val sums = foldl (zipWith op+) init bitLines

    val greaterBits = map (fn ct => if ct >= mid then 1 else 0) sums
  in
    greaterBits
  end

val linewiseSum =
  let
    val gamma = bitListToInt mostCommonBits
    val epsilon = bitListToInt (map (fn g => 1 - g) mostCommonBits)
  in
    gamma * epsilon
  end

fun iterateToSingleton _ [] = NONE
  | iterateToSingleton _ [s] = SOME s
  | iterateToSingleton f ls = iterateToSingleton f (f ls)

fun getAdvRating crit =
  let
    val doubled = map (fn L => (L,L)) bitLines

    fun mostCommonHeadBit (LL : (int list * int list) list) =
      let
        val mid = (List.length LL) div 2
        fun cmb ((_,l::_), sm) = sm+l
      in
        if (foldl cmb 0 LL) >= mid then 1 else 0 
      end


    fun iterFn (LL : (int list * int list) list) = 
      let
        val bitToCheck = crit (mostCommonHeadBit LL)
        val popped = map (fn (L,l::ls) => (L,l,ls)) LL
        val filtered  = List.filter (fn (_,b,_) => b = bitToCheck) popped
      in
        map (fn (L,_,ls) => (L,ls)) filtered
      end

    val (SOME (res,_)) = iterateToSingleton iterFn doubled
  in
    res
  end

val AOC032 =
  let
    val oxyRating = getAdvRating Fn.id
    val co2Rating = getAdvRating (fn c => 1 - c)
  in
    (bitListToInt oxyRating) * (bitListToInt co2Rating)
  end