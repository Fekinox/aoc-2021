datatype instruction =
    Forward of int
  | Down of int
  | Up of int

val instList =
  let
    val lines = InputHelper.getInput "input"

    fun stringToInst s =
      let
        val [dir,dist] = String.tokens Char.isSpace s
        val (SOME dist) = Int.fromString dist
        val dirCtor =
          (case dir of
            "forward" => Forward
          | "down" => Down
          | "up" => Up
          | _ => raise Fail "invalid input")
      in
        dirCtor dist
      end handle Bind => raise Fail "invalid input"

    val tokenized = map stringToInst lines
  in
    tokenized
  end

val AOC021 =
  let
    fun iterate (x,y) [] = (x,y)
      | iterate (x,y) ((Forward i)::ists) = iterate (x+i, y) ists
      | iterate (x,y) ((Down i)::ists)    = iterate (x, y+i) ists
      | iterate (x,y) ((Up i)::ists)      = iterate (x, y-i) ists
  in
    iterate (0,0) instList
  end

val AOC022 =
  let
    fun iterate (x,y,a) [] = (x,y)
      | iterate (x,y,a) ((Forward i)::ists) = iterate (x+i, y+(i*a), a) ists
      | iterate (x,y,a) ((Down i)::ists)    = iterate (x, y, a+i) ists
      | iterate (x,y,a) ((Up i)::ists)      = iterate (x, y, a-i) ists
  in
    iterate (0,0,0) instList
  end