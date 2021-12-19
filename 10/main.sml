val lines = InputHelper.getInput "input"

fun charToScore #")" = 3
  | charToScore #"]" = 57
  | charToScore #"}" = 1197
  | charToScore #">" = 25137
  | charToScore c = raise Fail (String.str c)

fun isOpen c = List.exists (fn x => x = c) [#"(", #"[", #"{", #"<"]
fun isMatch (#"(", #")") = true
  | isMatch (#"[", #"]") = true
  | isMatch (#"{", #"}") = true
  | isMatch (#"<", #">") = true
  | isMatch _ = false

datatype SynError = Corrupted of char | Incomplete of char list | Other

(* Returns the corrupted ending character if it exists *)
fun stringErrorStatus (line : string) =
  let
    fun syntaxScore' [] [] = Other
      | syntaxScore' stack [] = Incomplete stack
      | syntaxScore' stack (#"\n"::cs) = syntaxScore' stack cs
      | syntaxScore' stack (c::cs) =
        if (isOpen c)
        then syntaxScore' (c::stack) cs
        else
          (case stack of
            [] => Other
          | (s::ss) =>
            if isMatch (s, c)
            then syntaxScore' ss cs
            else (Corrupted c))
  in
    syntaxScore' [] (String.explode line)
  end

fun corrLineScore (Corrupted c) = charToScore c
  | corrLineScore _ = 0

fun incLineScore (Incomplete st) =
  let
    fun incCharToScore #"(" = 1
      | incCharToScore #"[" = 2
      | incCharToScore #"{" = 3
      | incCharToScore #"<" = 4

    fun foldFn (c, score) = (score * 5 + (incCharToScore c))
  in
    foldl foldFn 0 st
  end
  | incLineScore _ = 0

val corruptedScores = ((foldl op+ 0) o (map (corrLineScore o stringErrorStatus))) lines
val incompleteMiddleScore = 
  let
    val incScores = List.filter (fn s => s <> 0) ((map (incLineScore o stringErrorStatus)) lines)
  in
    ListHelper.median Int.compare incScores
  end
