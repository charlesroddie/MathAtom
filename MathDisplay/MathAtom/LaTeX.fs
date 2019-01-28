module MathDisplay.MathAtom.LaTeX

[<Struct>] type internal Read = Until of char | OneArgument | All

let toAtom latex =
    let (|Alphabet|NonAlphabet|) c = if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') then Alphabet else NonAlphabet
    let rec read until chars list =
        match chars with
        | [] ->
            List.rev list |> Result.Ok
        | c::_ when (match until with Until u -> c = u | _ -> false) ->
            List.rev list |> Result.Ok
        | '\\'::cs ->
            let cmd = 
                match cs with
                | (NonAlphabet & c)::_ -> string c
                | cs -> List.takeWhile (function Alphabet -> true | _ -> false) cs |> System.String.Concat
            match cmd with
            | "1" -> TestResult___ "It's 1!"::list |> Result.Ok
            //Implement commands
            | _ -> "Unrecognized command: \\" + cmd |> Result.Error
        | '^'::cs ->
            read OneArgument cs list |> Result.map (Row >> Superscript >> fun x -> x::list)
        | '_'::cs ->
            read OneArgument cs list |> Result.map (fun x -> (Row x |> Subscript)::list)
        | '{'::cs ->
            read (Until '}') cs list
        | '}'::_ ->
            Result.Error "Missing opening brace"

        
    read All (List.ofSeq latex) []