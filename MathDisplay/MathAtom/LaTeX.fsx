module MathDisplay.MathAtom.LaTeX

[<Struct>] type internal Read = Until of char | OneArgument | All

let toAtom latex =
    let (|Alphabet|NonAlphabet|) c = if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') then Alphabet else NonAlphabet
    let rec read until chars list =
        match chars with
        | [] -> list |> Row |> Result.Ok
        | '\\'::cs ->
            let cmd = 
                match cs with
                | (NonAlphabet & c)::_ -> string c
                | cs -> List.takeWhile (function Alphabet -> true | _ -> false) cs |> System.String.Concat
            match cmd with
            | "1" -> TestResult___ "It's 1!" |> Result.Ok
            //Implement commands
            | _ -> "Unrecognized command: \\" + cmd |> Result.Error
        | '^'::cs ->
            read OneArgument cs list |> Result.map Superscript
        | '_'::cs ->
            read OneArgument cs list |> Result.map Subscript
    read All (List.ofSeq latex) [] |> List.rev