module MathDisplay.MathAtom.LaTeX

open MathDisplay.DataTypes

[<Struct>] type internal Read = Until of char | OneArgument | All
exception ImplementationHasUnreadCharactersException of InputLaTeX:string * UnreadChars:char list * UnfinishedAtomList:MathAtom list
    with override this.Message = "The implementation has not read some characters yet, despite succeeding. The input LaTeX was: \n\n" + this.InputLaTeX
                               + "\n\nThe unread characters were: \n\n" + this.UnreadChars.ToString()
                               + "\n\nThe Unfinished atom list was: \n\n" + this.UnfinishedAtomList.ToString()

let toAtom latex =
    let inline (|Alphabet|NonAlphabet|) c = if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') then Alphabet else NonAlphabet
    let inline collapse xs = match xs with [x] -> x | x -> Row x
    let rec read until chars list =
        let inline continueRead cs atom =
            let list = atom::list
            match until with
            | OneArgument -> (cs, List.rev list) |> Result.Ok
            | Until _ | All -> read until cs list
        let inline readBlock until atomCase chars =
            read until chars list |> Result.bind (fun (rest, x) -> atomCase x |> continueRead rest)
        let inline readArg atomCase chars = readBlock OneArgument atomCase chars

        match chars with
        | [] ->
            match until with
            | All -> ([], List.rev list) |> Result.Ok
            | OneArgument -> "Unexpected end of input, missing argument" |> Result.Error
            | Until '}' -> "Missing closing brace" |> Result.Error
            | Until c -> "Expected character not found: " + c.ToString() |> Result.Error
        | c::cs when (match until with Until u -> c = u | OneArgument | All -> false) ->
            (cs, List.rev list) |> Result.Ok
        
        //* No Result.Ok nor read in this function after this point or you risk ImplementationHasUnreadCharactersException *
        //* Use continueRead, readBlock or readArg! *

        | '\\'::cs ->
            let cmd, rest = 
                match cs with
                | (NonAlphabet & c)::rest -> string c, rest
                | cs' ->
                    let ab, rest = List.partitionWhile (function Alphabet -> true | _ -> false) cs'
                    System.String.Concat ab, rest
            match cmd with
            | "1" -> TestResult___ "It's 1!" |> continueRead rest
            //Implement commands
            | _ -> "Unrecognized command: \\" + cmd |> Result.Error
        | '^'::cs -> readArg (collapse >> Superscript) cs
        | '_'::cs -> readArg (collapse >> Subscript) cs
        | '{'::cs -> readBlock (Until '}') Row cs
        | '}'::_ -> Result.Error "Missing opening brace"
        //| '&'::cs ->
        | '\''::cs ->
            let primes, rest = List.partitionWhile ((=) '\'') cs //primes do not include the one already matched
            List.length primes + 1 |> Primes |> continueRead rest
        | c::cs -> string c |> Ordinary |> continueRead cs
            
    match read All (List.ofSeq latex) [] with
    | Ok ([], atoms) -> collapse atoms |> Result.Ok
    | Error e -> Result.Error e
    | Ok (unreadChars, atoms) -> ImplementationHasUnreadCharactersException (latex, unreadChars, atoms) |> raise