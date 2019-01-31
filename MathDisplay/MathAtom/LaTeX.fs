module MathDisplay.MathAtom.LaTeX

open MathDisplay.DataTypes

[<Struct>] type internal Read = Until of char | OneArgument | RightDelimiter | All
exception ImplementationHasUnreadCharactersException of InputLaTeX:string * UnreadChars:char list * UnfinishedAtomList:MathAtom list
    with override this.Message = "The implementation has not read some characters yet, despite succeeding. The input LaTeX was: \n\n" + this.InputLaTeX
                               + "\n\nThe unread characters were: \n\n" + this.UnreadChars.ToString()
                               + "\n\nThe Unfinished atom list was: \n\n" + this.UnfinishedAtomList.ToString()

let toAtom latex (settings: MathDisplay.MathAtom.LaTeXDefaultMaps.LaTeXOptions) =
    let inline (|Alphabet|NonAlphabet|) c = if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') then Alphabet else NonAlphabet
    let inline (|Command|) cs = match cs with
                                | (NonAlphabet & c)::rest -> string c, rest
                                | _ ->
                                    let ab, cs = List.partitionWhile (function Alphabet -> true | _ -> false) cs
                                    System.String.Concat ab, skipSpaces cs
    let inline collapse xs = match xs with [x] -> x | x -> Row x
    let inline skipSpaces cs = List.skipWhile System.Char.IsWhiteSpace cs
    let rec read until chars list =
        let inline RETURN cs list = (cs, List.rev list) |> Result.Ok
        let inline argDelimiter cs =
            match cs with
            | [] -> Result.Error
            | '\\'::'|'::'|'::cs -> (AliasMap.tryFindValue "||" settings.Delimiters) |> Option.map (fun d -> )
        ///Reads an argument that is a block and applies it to the functions provided
        let inline argBlock until atomMaker useAtom cs =
            read until cs list |> Result.bind (fun (cs, arg) -> atomMaker arg |> useAtom cs)
        ///Reads an optional argument and returns it, cs should start with '[' to be recognized
        let inline argOption cs =
            match skipSpaces cs with
            | '['::cs -> argBlock (Until ']') collapse (fun cs atom -> Result.Ok (cs, atom)) cs |> ValueSome
            | _ -> ValueNone
        ///Command has (N+1) arguments
        let inline argPlus1 argN cs atomMaker = argBlock OneArgument (collapse >> atomMaker) argN cs
        ///Command has 0 arguments
        let inline arg0 cs atom =
            let list = atom::list
            match until with
            | OneArgument -> (cs, List.rev list) |> Result.Ok
            | _ -> read until cs list
        ///Command has 1 argument
        let arg1 = argPlus1 arg0
        ///Command has 2 arguments
        let arg2 = argPlus1 arg1
        
        //* No Result.Ok nor read in this function after this point or you risk ImplementationHasUnreadCharactersException *
        //* Use the arg functions, or if really necessary, RETURN! *

        match skipSpaces chars with
        | [] ->
            match until with
            | All -> RETURN [] list
            | OneArgument -> "Unexpected end of input, missing argument" |> Result.Error
            | RightDelimiter -> "Missing \\right" |> Result.Error
            | Until '}' -> "Missing closing brace" |> Result.Error
            | Until c -> "Expected character not found: " + c.ToString() |> Result.Error
        | c::cs when (match until with Until u -> c = u | _ -> false) -> RETURN cs list
        | '\\'::cs ->
            let cmd, cs = 
                match cs with
                | (NonAlphabet & c)::rest -> string c, rest
                | _ ->
                    let ab, cs = List.partitionWhile (function Alphabet -> true | _ -> false) cs
                    System.String.Concat ab, skipSpaces cs
            match cmd with
            | "1" -> TestResult___ "It's 1!" |> arg0 cs
            | "frac" -> (fun n d -> Fraction (n, d, Center, Center, ValueNone)) |> arg2 cs
            | "binom" -> (fun n d -> Fraction (n, d, Center, Center, ValueSome 0.)) |> arg2 cs
            | "sqrt" -> match argOption cs with
                        | ValueSome (Result.Ok (cs, degree)) -> arg1 cs (fun radicand -> Radical (ValueSome degree, radicand))
                        | ValueSome (Result.Error e) -> Result.Error e
                        | ValueNone -> arg1 cs (fun radicand -> Radical (ValueNone, radicand))
            | "left" -> match 
            | "right" -> match until with
                         | RightDelimiter -> RETURN cs list
                         | _ -> "Missing \\left" |> Result.Error
            | _ -> "Unrecognized command: \\" + cmd |> Result.Error
        | '^'::cs -> arg1 cs Superscript
        | '_'::cs -> arg1 cs Subscript
        | '{'::cs -> argBlock (Until '}') Row arg0 cs
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