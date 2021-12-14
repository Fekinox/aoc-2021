signature INPUTHELPER =
sig
    (* Returns input filename as a list of strings *)
    val getInput : string -> string list
end

structure InputHelper : INPUTHELPER =
struct
  val stripWs = String.implode o (List.filter Char.isPrint) o String.explode
  fun getInput filename =
    let
      val input = TextIO.openIn filename
      fun read strm =
        (case (TextIO.inputLine strm) of
          NONE => []
        | (SOME s) => (stripWs s)::(read strm))
    in
      read input
    end
end

signature ARRAYHELPER =
sig
  val mutate : ('a Array.array * int * ('a -> 'a)) -> unit
  val mutate2D : ('a Array2.array * int * int * ('a -> 'a)) -> unit

  val printVec : ('a -> string) -> 'a Vector.vector -> unit
  val print2D : ('a -> string) -> 'a Array2.array -> unit
end

structure ArrayHelper : ARRAYHELPER =
struct
  fun mutate (A, i, f) =
    let
      val z = Array.sub (A, i)
    in
      Array.update (A, i, f z)
    end

  fun mutate2D (A, i, j, f) =
    let
      val z = Array2.sub (A, i, j)
    in
      Array2.update (A, i, j, f z)
    end

  fun printVec toStr V = Vector.app (print o toStr) V
  fun print2D toStr A =
    let
      fun printRow i = (printVec toStr (Array2.row (A,i)); print "\n")
      fun iter i =
        if (i = Array2.nRows A)
        then ()
        else (printRow i; iter (i+1))
    in
      iter 0
    end
end

signature LISTHELPER =
sig
  val mergesort : ('a * 'a -> order) -> 'a list -> 'a list
  val merge : ('a * 'a -> order) -> ('a list * 'a list) -> 'a list
  val split : 'a list -> ('a list * 'a list)
  val collect : ('a * 'a -> order) -> 'a list -> 'a list list
  val sortNoDups : ('a * 'a -> order) -> 'a list -> 'a list

  val quickselect : ('a * 'a -> order) -> int -> 'a list -> 'a option
  val median : ('a * 'a -> order) -> 'a list -> 'a option

  val insert : ('a * 'a -> order) -> ('a * 'a -> 'a) -> 'a list -> 'a -> 'a list
end

structure ListHelper : LISTHELPER =
struct
  fun split [] = ([], [])
    | split [x] = ([x], [])
    | split (x::y::ys) = let val (A,B) = split ys in (x::A, y::B) end

  fun merge cmp (L, []) = L
    | merge cmp ([], R) = R
    | merge cmp (l::L, r::R) =
    (case cmp (l, r) of
       LESS => l :: (merge cmp (L, r::R))
     | _ => r :: (merge cmp (l::L, R)))

  fun mergesort cmp [] = []
    | mergesort cmp [x] = [x]
    | mergesort cmp L =
    let
      val (A, B) = split L
    in
      merge cmp (mergesort cmp A, mergesort cmp B)
    end

  fun sortNoDups cmp L =
    let
      val sorted = mergesort cmp L
      
      fun dups (x::y::ys) =
        (case cmp (x,y) of
          EQUAL => dups (y::ys)
        | _ => x::(dups (y::ys)))
        | dups L = L
    in
      dups sorted
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
      List.rev (collect' [] sorted)
    end

  fun quickselect cmp k [] = NONE
    | quickselect cmp k (L as (x::xs)) =
      let
        val (lesser, greater) = List.partition (fn i => cmp(i,x) <> GREATER) L
        val (lesser, pivot) = List.partition (fn i => cmp(i,x) = LESS) lesser
        val (lessLen,pivLen) = (List.length lesser, List.length pivot)
      in
        (case (k < lessLen, k < lessLen + pivLen) of
          (true, _) => quickselect cmp k lesser
        | (_, true) => SOME x
        | _         => quickselect cmp (k - lessLen - pivLen) greater)
      end

  fun median cmp L = quickselect cmp (List.length L div 2) L

  fun insert cmp cmb [] x = [x]
    | insert cmp cmb (y::ys) x =
    (case cmp (x,y) of
      GREATER => y::(insert cmp cmb ys x)
    | EQUAL => (cmb (x,y))::ys
    | LESS => x::y::ys)
end