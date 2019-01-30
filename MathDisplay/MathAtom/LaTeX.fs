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
        ///Command has 0 arguments
        let inline arg0 cs atom =
            let list = atom::list
            match until with
            | OneArgument -> (cs, List.rev list) |> Result.Ok
            | Until _ | All -> read until cs list
        ///Command has 1 argument
        let inline arg1Block until atomMaker useAtom cs =
            read until cs list |> Result.bind (fun (rest, x) -> atomMaker x |> useAtom rest)
        ///Command has 1 argument
        let inline arg1 atomMaker cs = arg1Block OneArgument (collapse >> atomMaker) arg0 cs
        ///Read an optional argument
        let inline argoption cs = arg1Block (Until ']') collapse (fun cs atom -> Result.Ok (cs, atom)) cs
        ///Command has 2 arguments
        let inline arg2 atomMaker cs =
            read OneArgument cs list |> Result.bind (fun (cs, arg1) ->
            read OneArgument cs list |> Result.bind (fun (cs, arg2) ->
            atomMaker (collapse arg1) (collapse arg2) |> arg0 cs))
        ///Command has 3 arguments
        let inline arg3 atomMaker cs =
            read OneArgument cs list |> Result.bind (fun (cs, arg1) ->
            read OneArgument cs list |> Result.bind (fun (cs, arg2) ->
            read OneArgument cs list |> Result.bind (fun (cs, arg3) ->
            atomMaker (collapse arg1) (collapse arg2) (collapse arg3) |> arg0 cs)))

        match chars with
        | [] ->
            match until with
            | All -> ([], List.rev list) |> Result.Ok
            | OneArgument -> "Unexpected end of input, missing argument" |> Result.Error
            | Until '}' -> "Missing closing brace" |> Result.Error
            | Until c -> "Expected character not found: " + c.ToString() |> Result.Error
        | c::cs when (match until with Until u -> c = u | _ -> false) ->
            (cs, List.rev list) |> Result.Ok
        
        //* No Result.Ok nor read in this function after this point or you risk ImplementationHasUnreadCharactersException *
        //* Use the arg functions! *

        | '\\'::cs ->
            let cmd, cs = 
                match cs with
                | (NonAlphabet & c)::rest -> string c, rest
                | _ ->
                    let ab, cs = List.partitionWhile (function Alphabet -> true | _ -> false) cs
                    System.String.Concat ab, cs
            match cmd with
            | "1" -> TestResult___ "It's 1!" |> arg0 cs
            | "frac" -> arg2 (fun n d -> Fraction (n, d, Center, Center, ValueNone)) cs
            | "binom" -> arg2 (fun n d -> Fraction (n, d, Center, Center, ValueSome 0.)) cs
            | "sqrt" -> match cs with
                        | '['::cs -> argoption cs |> Result.map (fun degree -> arg1 (fun radicand -> Radical (ValueSome degree, radicand))) cs
                        | _ -> arg1 (fun radicand -> Radical (ValueNone, radicand) cs
            | _ -> "Unrecognized command: \\" + cmd |> Result.Error
        | '^'::cs -> arg1 Superscript cs
        | '_'::cs -> arg1 Subscript cs
        | '{'::cs -> arg1Block (Until '}') Row arg0 cs
        | '}'::_ -> Result.Error "Missing opening brace"
        //| '&'::cs ->
        | '\''::cs ->
            let primes, rest = List.partitionWhile ((=) '\'') cs //primes do not include the one already matched
            List.length primes + 1 |> Primes |> arg0 rest
        | c::cs -> string c |> Ordinary |> arg0 cs
            
    match read All (List.ofSeq latex) [] with
    | Ok ([], atoms) -> collapse atoms |> Result.Ok
    | Error e -> Result.Error e
    | Ok (unreadChars, atoms) -> ImplementationHasUnreadCharactersException (latex, unreadChars, atoms) |> raise