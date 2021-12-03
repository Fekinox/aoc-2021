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