type polymer = (char * char * int) list
type rule = (char * char * char)

fun charPairCmp ((l1,r1),(l2,r2)) =
  (case Char.compare (l1,l2) of
    EQUAL => Char.compare (r1,r2)
  | c => c)

fun polymerCmp ((x1,y1,_),(x2,y2,_)) = charPairCmp ((x1,y1),(x2,y2))

fun insertPoly (p, poly) = 
  (ListHelper.insert polymerCmp (fn ((x1,y1,n1),(x2,y2,n2)) => (x1,y1,n1+n2)) poly p)

fun insertFreqCount (count, freqc) =
  (ListHelper.insert (fn ((c1,_),(c2,_)) => Char.compare (c1,c2)) (fn ((c1,x1),(c2,x2)) => (c1,x1+x2)) freqc count)

val (initPolymer, initFreqCount, rules) =
  let
    val (polyLine::ruleLines) = InputHelper.getInput "input"

    val thePolymer = ref []

    fun readPolyLine (x::y::ys) =
      (thePolymer := insertPoly ((x,y,1), !thePolymer); readPolyLine (y::ys))
      | readPolyLine _ = ()

    val _ = readPolyLine (String.explode (polyLine))

    fun readRule s =
      let
        val [rulechars, _, result] = String.tokens Char.isSpace s
        val [x,y] = String.explode rulechars
      in
        (x,y,String.sub (result,0))
      end

    val allRules = map readRule ruleLines

    val collected = ListHelper.collect Char.compare (String.explode polyLine)

    fun charListToCount L = (List.hd L, List.length L)
  in
    (!thePolymer, map charListToCount collected, allRules)
  end

fun polyStep (poly, freqcount) =
  let
    val deltaPolymer = ref []

    val newFreqCount = ref freqcount

    fun polyStepOne (x,y,n) =
      (case List.find (fn (x1,y1,_) => (x,y)=(x1,y1)) rules of
        NONE => ()
      | (SOME (_,_,z)) =>
          (deltaPolymer := (List.foldl insertPoly (!deltaPolymer) [
            (x,y,~n),
            (x,z,n),
            (z,y,n)
          ]); newFreqCount := (insertFreqCount ((z,n), !newFreqCount)))
        )

    val _ = List.app polyStepOne poly
  in
    (List.foldl insertPoly poly (!deltaPolymer), !newFreqCount)
  end

fun mostAndLeastCommon counts =
  let
    fun freqMax (L as (c1,x1), R as (c2,x2)) = if x1 > x2 then L else R
    fun freqMin (L as (c1,x1), R as (c2,x2)) = if x1 > x2 then R else L
    fun foldl1 f (x::xs) = foldl f x xs
  in
    (foldl1 freqMax counts, foldl1 freqMin counts)
  end

fun countsAfterN n =
  let
    fun iter (p,c) 0 = (p,c)
      | iter (p,c) n = iter (polyStep (p,c)) (n-1)

    val (_,counts) = iter (initPolymer, initFreqCount) n
  in
    mostAndLeastCommon counts
  end

fun difference n =
  let
    val ((_,most),(_,least)) = countsAfterN n
  in
    most - least
  end