datatype snailnum = Regular of (int ref) | Pair of snailnum * snailnum

(* value at finger, left child, right child, ancestors *)

fun parseSnailnum s =
  let
    fun parser (#"["::cs) =
      let
        val (lhs, _::cs1) = parser cs
        val (rhs, _::cs2) = parser cs1
      in
        (Pair (lhs, rhs), cs2)
      end
      | parser (c::cs) =
        (Regular (ref (Char.ord c - 48)), cs)
      | parser _ = raise Fail "unknown"
  in
    (#1 (parser (String.explode s)))
  end

val snailNumList =
  ((map (parseSnailnum)) o InputHelper.getInput) "input"

fun printSnail (Regular (ref n)) = Int.toString n
  | printSnail (Pair (p1,p2)) = "[" ^ (printSnail p1) ^ "," ^ (printSnail p2) ^ "]"

fun copySnail (Regular (ref n)) = Regular (ref n)
  | copySnail (Pair (p1,p2)) = Pair (copySnail p1, copySnail p2)

fun leftmost (Pair (l,_)) = leftmost l
  | leftmost reg = reg

fun rightmost (Pair (_,r)) = rightmost r
  | rightmost reg = reg

(* Assuming the input is the list of paths from the root
   node of the snail number to some Regular value, finds
   the closest Regular to the left *)
(* Each element of the list is a bool which is true
   if it's a left parent of the current node
   and false otws *)
fun prev [] = NONE
  | prev ((true, _)::ps) = prev ps
  | prev ((false,Pair (p1,p2))::ps) = SOME (rightmost p1)

fun next [] = NONE
  | next ((false, _)::ps) = next ps
  | next ((true,Pair (p1,p2))::ps) = SOME (leftmost p2)

(* The traverser works as follows:
   - It will look for a pair to explode. If it finds such a pair,
     it explodes it and restarts.
   - It then looks for a number to split. if it finds such a number,
     it splits it and restarts.
   - If it finds neither it halts. *)
fun reduceStep S sc fc =
  let
    fun addIfSome _ NONE = ()
      | addIfSome n (SOME (Regular k)) = k := !k + n

    fun explode (l,r) ancs =
      let
        val _ = addIfSome l (prev ancs)
        val _ = addIfSome r (next ancs)
      in
        (Regular (ref 0))
      end

    fun split n =
      let val m = n div 2
      in Pair (Regular (ref m), Regular (ref (n-m))) end

    (* look for the leftmost explodable pair, and explode it,
       leaving the rest of the DS intact *)
    fun tryExplode depth ancs (Regular _) sc fc = fc ()
      | tryExplode depth ancs (Pair (Regular (ref l), Regular (ref r))) sc fc =
        if depth >= 4
        then sc (explode (l,r) ancs)
        else fc ()
      | tryExplode depth ancs (P as Pair (p1, p2)) sc fc =
        tryExplode (depth+1) ((true,P)::ancs) p1
        (fn p1' => sc (Pair (p1',p2)))
        (fn _ =>
          tryExplode (depth+1) ((false,P)::ancs) p2
          (fn p2' => sc (Pair (p1,p2')))
          fc)

    val tryExpRunner = tryExplode 0 []

    fun trySplit (Regular (ref n)) sc fc =
      if n >= 10
      then sc (split n)
      else fc ()
      | trySplit (Pair (p1,p2)) sc fc =
        trySplit p1
        (fn p1' => sc (Pair (p1',p2)))
        (fn _ =>
          trySplit p2
          (fn p2' => sc (Pair (p1,p2')))
          fc)
  in
    tryExpRunner S
    sc
    (fn _ => trySplit S sc fc)
  end

fun reduceFull S =
  reduceStep S (fn S' => reduceFull S') (fn _ => S)

fun snailAdd (s1,s2) =
  let
    (* side effects bad *)
    val (c1,c2) = (copySnail s1, copySnail s2)
  in 
    reduceFull (Pair (c1,c2))
  end

fun magnitude (Regular (ref r)) = r
  | magnitude (Pair (p1,p2)) = (3*magnitude p1) + (2*magnitude p2)

val totalMag =
  let
    val theSnail = foldl (fn (s,sum) => snailAdd (sum, s)) (hd snailNumList) (tl snailNumList)
  in
    magnitude theSnail
  end

fun highestMag S =
  let
    fun bestMag (s1,s2) =
      let
        val (m1,m2) =
          (magnitude (snailAdd (s1,s2)), magnitude (snailAdd (s2,s1)))
      in
        Int.max (m1,m2)
      end

    fun highestMag' m [] = m
      | highestMag' m (s::L) =
        let
          val maxWithFirst =
            List.foldl (fn (sn, mx) => Int.max (mx, bestMag (s,sn))) 0 L
        in
          highestMag' (Int.max(m, maxWithFirst)) L
        end
  in
    highestMag' 0 S
  end