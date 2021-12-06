signature INPUTHELPER =
sig
    (* Returns input filename as a list of strings *)
    val getInput : string -> string list
end

structure InputHelper : INPUTHELPER =
struct
  fun getInput filename =
    let
      val input = TextIO.openIn filename
      fun read strm =
        (case (TextIO.inputLine strm) of
          NONE => []
        | (SOME s) => s::(read strm))
    in
      read input
    end
end

signature ARRAYHELPER =
sig
  val mutate : ('a Array.array * int * ('a -> 'a)) -> unit
  val mutate2D : ('a Array2.array * int * int * ('a -> 'a)) -> unit
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
end