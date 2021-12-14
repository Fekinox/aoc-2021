val graph =
  let
    val lines = InputHelper.getInput "input"

    fun getStringPairs s =
      let
        val strippedWs = (String.implode o (List.filter Char.isGraph) o String.explode) s
        val [l,r] = String.tokens (fn c => c = #"-") strippedWs
      in
        [(l,r),(r,l)]
      end

    val allPairs = List.concat (map getStringPairs lines)

    val collected = ListHelper.collect (fn ((s1,_),(s2,_)) => String.compare (s1,s2)) allPairs

    val cleaned = List.map (fn ((s,q)::ns) => (s, q::(map (fn (_,x) => x) ns))) collected
  in
    cleaned
  end

fun neighbors G s =
  let
    val N = List.find (fn (x,_) => x=s) G
  in
    (case N of
      NONE => raise Fail "whoops"
    | SOME (_,ns) => ns)
  end

fun isBig s = Char.isUpper (String.sub (s, 0))
fun isSmall s = Char.isLower (String.sub (s, 0))

fun allPaths maxRevisits =
  let
    val paths = ref 0
    (* imperative, yes, but it's way cleaner than threading a state variable
       through all the functions *)

    (* A cave is a valid neighbor to visit in the DFS if it's big or if we
       haven't visited it already *)
    fun validNeighbor route s =
      (isBig s) orelse (not (List.exists (fn v => v = s) route))

    (* If we hit the end halt immediately *)
    fun dfs route revis "end" = paths := (!paths + 1)
      | dfs route revis v =
      let
        val nbors = (neighbors graph v)

        (* If it's a valid neighbor, just visit it *)
        (* Otherwise, it's some small cave we've visited already.
           If we haven't hit the max limit of revisits and this
           node isn't the start or the end, we may revisit it *)
        fun tryNbor n =
          if (validNeighbor route n)
          then dfs (v::route) revis n
          else
            if (revis < maxRevisits andalso n <> "start" andalso n <> "end")
            then dfs (v::route) (revis+1) n
            else ()
      in
        List.app tryNbor nbors
      end

    val _ = dfs [] 0 "start"
  in
    !paths
  end