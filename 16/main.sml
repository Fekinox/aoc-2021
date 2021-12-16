datatype packet = Literal of int * int | Operator of int * int * packet list

fun takeDrop n L = 
  let
    fun takeDrop' 0 L R = (List.rev R, L)
      | takeDrop' n [] R = raise Fail "out of bounds"
      | takeDrop' n (l::L) R = takeDrop' (n-1) L (l::R)
  in
    takeDrop' n L []
  end

fun decToBin n = [(n div 8) mod 2, (n div 4) mod 2, (n div 2) mod 2, n mod 2]

fun hexToBin c =
  let
    val or = Char.ord c
  in
    if (48 <= or andalso or <= 57)
    then decToBin(or - 48)
    else decToBin((or-65)+10)
  end

local
  fun binToDecLsb ([] : int list) : int = 0
    | binToDecLsb (b::bs) = b + 2*(binToDecLsb bs)
in
  val binToDec = binToDecLsb o List.rev
end

val strToBitstring = List.concat o (map hexToBin) o String.explode

val bitstring =
  let
    val [line] = InputHelper.getInput "input"
  in
    strToBitstring line
  end

fun parseLiteralNumber L =
  let
    fun loop q L =
      let
        (* Chop off 5 bits: if the first one is 0, stop recursing *)
        val ((isDone::rest), L') = takeDrop 5 L
        (* Build up the number from the msb first*)
        val newQ = 16*q + (binToDec rest)
      in
        if isDone = 0 then (newQ, L') else loop newQ L'
      end

    val (res, finalL) = loop 0 L
  in
    (res, finalL)
  end

fun parsePacket ([] : int list) : (packet * int list) option = NONE
  | parsePacket (L : int list) : (packet * int list) option =
  let
    val (version, L1) = takeDrop 3 L
    val (packID, L2) = takeDrop 3 L1

    val versInt = binToDec version
    val packInt = binToDec packID

    (* Gets all adjacent packets in the substring S *)
    fun getPacketsLenMode S =
      (case parsePacket S of
        NONE => []
      | SOME (p, S') => p::(getPacketsLenMode S'))

    (* Gets n packets from S *)
    fun getPacketsCountMode S 0 = ([], S)
      | getPacketsCountMode S n =
        let
          val SOME (p, S') = parsePacket S
          val (ps, S'') = getPacketsCountMode S' (n-1)
        in
          (p::ps, S'') 
        end
  in
    (* Parse a literal *)
    if packInt = 4
    then
      let
        val (vl, L3) = parseLiteralNumber L2
      in
        SOME (Literal (versInt, vl), L3)
      end
    else
      let
        val (isLengthMode::L3) = L2
      in
        if isLengthMode = 0
        then
          let
            val (count, L4) = takeDrop 15 L3
            val countN = binToDec count
            val (S, L5) = takeDrop countN L4 
            val subpacks = getPacketsLenMode S
          in
            SOME (Operator (versInt, packInt, subpacks), L5)
          end
        else
          let
            val (count, L4) = takeDrop 11 L3
            val countN = binToDec count
            val (subpacks, L5) = getPacketsCountMode L4 countN 
          in
            SOME (Operator (versInt, packInt, subpacks), L5)
          end
      end
  end

val SOME (thePacket, _) = parsePacket bitstring

fun versNumberSum (Literal (v,_)) = v
  | versNumberSum (Operator (v,_,ps)) = List.foldl (fn (p,s) => s + (versNumberSum p)) v ps

fun eval (Literal (_,vl)) = vl
  | eval (Operator (_,0,a::ps)) = foldl (fn (p,q) => (eval p) + q) (eval a) ps
  | eval (Operator (_,1,a::ps)) = foldl (fn (p,q) => (eval p) * q) (eval a) ps
  | eval (Operator (_,2,a::ps)) = foldl (fn (p,q) => Int.min(eval p, q)) (eval a) ps
  | eval (Operator (_,3,a::ps)) = foldl (fn (p,q) => Int.max(eval p, q)) (eval a) ps
  | eval (Operator (_,5,[a,b])) = if (eval a) > (eval b) then 1 else 0
  | eval (Operator (_,6,[a,b])) = if (eval a) < (eval b) then 1 else 0
  | eval (Operator (_,7,[a,b])) = if (eval a) = (eval b) then 1 else 0