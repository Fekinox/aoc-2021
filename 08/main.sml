val patternList =
  let
    val lines = InputHelper.getInput "input"

    fun processPattern s =
      let
        val arr = Array.array (7, false)
      in
        (List.app (fn c => Array.update (arr, (Char.ord c - 97), true)) (String.explode s); Array.vector arr)
      end

    val splitBar = String.tokens (fn c => c = #"|")
    val splitSpace = String.tokens Char.isSpace

    fun processLine l =
      let
        val [patterns, output] = splitBar l

        val proc = (map processPattern) o splitSpace
      in
        (proc patterns, proc output)
      end
  in
    map processLine lines
  end

val numTrues = Vector.foldl (fn (true, sum) => sum+1 | (false, sum) => sum) 0

fun num1478InOutput (_, output) =
  let
    val toNumTrues = map numTrues output
    fun isValid sz = List.exists (fn x => x = sz) [2,3,4,7]
    val filtered = List.filter isValid toNumTrues
  in
    List.length filtered
  end

val all1478s =
  let
    val linewise = map num1478InOutput patternList
  in
    foldl op+ 0 linewise
  end

fun parseDigitCanonical digit =
  (case digit of
    [0,1,2,4,5,6] => 0
  | [2,5] => 1
  | [0,2,3,4,6] => 2
  | [0,2,3,5,6] => 3
  | [1,2,3,5] => 4
  | [0,1,3,5,6] => 5
  | [0,1,3,4,5,6] => 6
  | [0,2,5] => 7
  | [0,1,2,3,4,5,6] => 8
  | [0,1,2,3,5,6] => 9
  | _ => raise Fail "invalid")

fun parseNumberCanonical digList =
  let
    fun parseNumberRunner sum [] = sum
      | parseNumberRunner sum (d::ds) =
        parseNumberRunner (10 * sum + (parseDigitCanonical d)) ds
  in
    parseNumberRunner 0 digList
  end

(* Starting from the first line of the input, we have a general
   strategy of deducing the canonical mapping from letters to
   bits of the seven-segment display.
    0
   1 2
    3
   4 5
    6

  - (7) Suppose we have 'ec' and 'ecb' in the output. The differing bit, b,
    must map to 0 in the canonical mapping.
  - (6) Suppose we have 'ec' and 'dbagef'. The missing bit in the second, 'c',
    must map to 2, and 'e' must map to 5.
  - (9) Suppose we have 'cdef' and 'feabcd', where 'cdef' is completely contained
    in the second string. The missing bit in that string, 'g', must map to 4.
  - (0) Suppose we have 'ec', 'cdef', and 'caegdb', where 'caegdb' is missing one of
    'df'. The missing bit, 'f', must map to 3, and therefore 'd' maps to 1.
  - After doing all this, 'a' maps to 6.
*)

(* R[i] = V1[i] and not V2[i] *)
fun subtract V1 V2 = Vector.tabulate (Vector.length V1, (fn i => Vector.sub(V1, i) andalso (not (Vector.sub(V2, i)))))
fun uniqueBit V = let val (SOME (i,_)) = Vector.findi (fn (_,x) => x) V in i end

fun getCanonicalMap uniquePatterns =
  let
    val (SOME one) = List.find (fn v => numTrues v = 2) uniquePatterns
    val (SOME four) = List.find (fn v => numTrues v = 4) uniquePatterns
    val (SOME seven) = List.find (fn v => numTrues v = 3) uniquePatterns
    val (SOME eight) = List.find (fn v => numTrues v = 7) uniquePatterns

    val foursUnique = subtract four one
    val deductibles = List.filter (fn v => numTrues v = 6) uniquePatterns
    val 3 = List.length deductibles

    val map = Array.array (7, 0)

    fun addMap (i, j) = Array.update (map, i, j)

    (* Deduce (7) *)
    val bitZero = uniqueBit (subtract seven one)
    val _ = addMap (0, bitZero)

    (* Deduce other bits *)
    fun deduce target =
      let
        (* Let's try deducing (6) *)
        val sixDed = subtract one target
      in
        (case (numTrues sixDed) of
          1 =>
            let
              val bitTwo = uniqueBit sixDed
              val bitFive = uniqueBit (subtract one sixDed)
            in
              (addMap (2, bitTwo); addMap (5, bitFive))
            end
        | _ =>
          let
            (* Let's deduce (9) and (0) instead *)
            val nextDed = subtract four target
          in
            (case (numTrues nextDed) of
              0 =>
                let
                  val bitFour = uniqueBit (subtract eight target)
                  val bitSix = uniqueBit (subtract (subtract target four) seven)
                in
                  (addMap (4, bitFour); addMap (6, bitSix))
                end
            | 1 =>
                let
                  val bitThree = uniqueBit (subtract eight target)
                  val bitOne = uniqueBit (subtract foursUnique nextDed)
                in
                  (addMap (3, bitThree); addMap (1, bitOne))
                end)
          end)
      end

    val _ = List.app deduce deductibles
  in
    Array.vector map
  end

fun convertWithMap m V =
  List.filter (fn i => Vector.sub (V, Vector.sub (m, i))) (List.tabulate (7, Fn.id))

fun decodePatternValue (patterns, output) =
  let
    val m = getCanonicalMap patterns
    val converted = parseNumberCanonical (map (convertWithMap m) output)
  in
    converted
  end

val sumAllOutputs =
  let
    fun decodePatternValue (patterns, output) =
      let
        val m = getCanonicalMap patterns
        val converted = parseNumberCanonical (map (convertWithMap m) output)
      in
        converted
      end
  in
    ((foldr op+ 0) o (map decodePatternValue)) patternList
  end