module MathDisplay.MathAtom.LaTeXCommand

[<Struct>]
type LaTeXArgument =
    Normal | AllBefore | AllAfter | Delimiter | String | Comment
[<Struct>] type LaTeXCommand = { Name:string; Args:LaTeXArgument list }
let Normal name nargs =
    { Name = name; Args = List.replicate nargs Normal }
let Symbol name = Normal name 0
let Custom name args = { Name = name; Args = args }