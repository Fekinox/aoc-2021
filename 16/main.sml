datatype packet = Literal of int * int | Operator of int * int * packet list

fun takeDrop n L = 
  let
    fun takeDrop' 0 R L = (List.rev R, L)
      | takeDrop' n R [] = raise Fail "out of bounds"
      | takeDrop' n R (l::L) = takeDrop' (n-1) (l::R) L
  in
    takeDrop' n [] L
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

val binToDec = foldl (fn (b,s) => b + 2*s) 0

val strToBitstring = List.concat o (map hexToBin) o String.explode

val bitstring = (strToBitstring o hd o InputHelper.getInput) "input"

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
  in
    loop 0 L
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
            (* Build n consecutive packets *)
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
  | eval (Operator (_,0,L)) = cmb op+ L
  | eval (Operator (_,1,L)) = cmb op* L
  | eval (Operator (_,2,L)) = cmb Int.min L
  | eval (Operator (_,3,L)) = cmb Int.max L
  | eval (Operator (_,5,L)) = cmp op> L
  | eval (Operator (_,6,L)) = cmp op< L
  | eval (Operator (_,7,L)) = cmp op= L
and cmb f (a::A) = foldl (fn (p,q) => f (eval p, q)) (eval a) A
and cmp f [a,b] = if f (eval a, eval b) then 1 else 0