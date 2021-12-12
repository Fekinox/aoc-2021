local
  fun split [] = ([], [])
    | split [x] = ([x], [])
    | split (x::y::ys) = let val (A,B) = split ys in (x::A, y::B) end

  fun merge cmp (L, []) = L
    | merge cmp ([], R) = R
    | merge cmp (l::L, r::R) =
    (case cmp (l, r) of
       LESS => l :: (merge cmp (L, r::R))
     | _ => r :: (merge cmp (l::L, R)))
in
  fun mergesort cmp [] = []
    | mergesort cmp [x] = [x]
    | mergesort cmp L =
    let
      val (A, B) = split L
    in
      merge cmp (mergesort cmp A, mergesort cmp B)
    end
end

fun collect cmp L =
  let
    val sorted = mergesort cmp L

    fun collect' [] [] = []
      | collect' S [] = S
      | collect' [] (x::L) = collect' [[x]] L
      | collect' ((h::x)::xs) (y::L) =
        (case cmp (h, y) of
           EQUAL => collect' ((y::h::x)::xs) L
         | _ => collect' ([y]::(h::x)::xs) L)
  in
    collect' [] sorted
  end

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

    val collected = collect (fn ((s1,_),(s2,_)) => String.compare (s1,s2)) allPairs

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

val isBig = (List.all Char.isUpper) o String.explode
val isSmall = (List.all Char.isLower) o String.explode

val allPaths =
  let
    val paths = ref []

    (* Do a DFS, keeping track of all the small caves we visit in a list *)
    fun validNeighbor route s =
      (isBig s) orelse (not (List.exists (fn v => v = s) route))

    fun dfs route "end" = paths := (List.rev ("end"::route) :: !paths)
      | dfs route v =
      let
        val nbors = List.filter (validNeighbor route) (neighbors graph v)
      in
        List.app (fn n => dfs (v::route) n) nbors
      end

    val _ = dfs [] "start"
  in
    !paths
  end

val allPathsWithRevisit =
  let
    val paths = ref []

    (* Do a DFS, keeping track of all the small caves we visit in a list *)
    fun validNeighbor route s =
      (isBig s) orelse (not (List.exists (fn v => v = s) route))

    fun dfs route revisit "end" = paths := (List.rev ("end"::route) :: !paths)
      | dfs route revisit v =
      let
        val nbors = List.filter (validNeighbor route) (neighbors graph v)
        val revisits =
          if revisit
          then List.filter (fn x => (not (validNeighbor route x)) andalso x <> "start" andalso x <> "end") (neighbors graph v)
          else []
      in
        (List.app (fn n => dfs (v::route) revisit n) nbors;
        List.app (fn n => dfs (v::route) false n) revisits)
      end

    val _ = dfs [] true "start"
  in
    !paths
  end