module MathDisplay.MathAtom.LaTeX

open MathDisplay.DataTypes

[<Struct>] type internal Read = Until of char | UntilRightDelimiter | OneArgument | All
type TableEnvironment = { Name:string voption; Ended:bool; NumRows:int }
type Options = {
    Delimiters: AliasMap<string, Delimiter>
} with
    static member Default = {
        Delimiters = LaTeXDefaultMaps.delimiters
    }
exception ImplementationHasUnreadCharactersException of InputLaTeX:string * UnreadChars:char list * UnfinishedAtomList:MathAtom list
    with override this.Message = "The implementation has not read some characters yet, despite succeeding. The input LaTeX was: \n\n" + this.InputLaTeX
                               + "\n\nThe unread characters were: \n\n" + this.UnreadChars.ToString()
                               + "\n\nThe Unfinished atom list was: \n\n" + this.UnfinishedAtomList.ToString()

let toAtom (settings: Options) latex =
    let errorArgMissing = Error "Unexpected end of input, missing argument"
    let inline errorDelimMissing cmd = Error (cmd + " was not found in delimiter map")
    let inline collapse xs = match xs with [x] -> x | x -> Row x
    let inline skipSpaces cs = List.skipWhile System.Char.IsWhiteSpace cs
    let inline (|Alphabet|NonAlphabet|) c = if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') then Alphabet else NonAlphabet
    let inline (|PartitionAlphabets|) cs = List.partitionWhile (function Alphabet -> true | NonAlphabet -> false) cs
    let inline (|CommandName|) cs = match cs with
                                    | (NonAlphabet & c)::cs -> string c, cs
                                    | PartitionAlphabets (ab, cs) -> System.String.Concat ab, skipSpaces cs
    let rec read tableEnv until cs list =
        let inline RETURN tableEnv cs list = (tableEnv, cs, List.rev list) |> Ok
        ///Reads a delimiter
        let readDelimiter cs =
            match cs with
            | [] -> errorArgMissing
            | '\\'::(CommandName (cmd, cs)) ->
                let cmd = match cmd with "|" -> "||" | _ -> cmd
                match AliasMap.tryFindValue cmd settings.Delimiters with
                | Some v -> Ok (v, cs)
                | None -> errorDelimMissing cmd
            | c::cs ->
                let c = string c
                match AliasMap.tryFindValue c settings.Delimiters with
                | Some v -> Ok (v, cs)
                | None -> errorDelimMissing c
        ///Reads an environment
        let readEnvironment cs =
            match cs with
            | '{'::PartitionAlphabets (ab, '}'::cs) -> Ok (System.String.Concat ab, cs)
            | c::cs -> Ok (string c, cs) //Seems allowed by LaTeX?
            | _ -> "Invalid environment, contains non-A-to-Z characters: " + System.String.Concat cs |> Error
        ///Reads an argument that is a block and applies it to the functions provided
        let inline readBlock until atomMaker useAtom cs =
            read tableEnv until cs list |> Result.bind (fun (cs, arg) -> atomMaker arg |> useAtom cs)
        ///Reads an optional argument and returns it, cs should start with '[' to be recognized
        let readOption cs =
            match cs with
            | '['::cs -> readBlock (Until ']') collapse (fun cs atom -> (cs, atom) |> ValueSome |> Ok) cs
            | _ -> Ok ValueNone
        let readTable name cs =
            let rec innerReadTable rows currentRow cs =
                read (ValueSome { Name = name; Ended = false; NumRows = 0 }) until cs list
                |> Result.bind (fun (cs, atoms) ->
                    let atom = collapse atoms
                    match cs with
                    | '&'::cs -> innerReadTable 
                )
        ///Processes (N+1) arguments, then continues reading
        let inline argPlus1 argN cs atomMaker = readBlock OneArgument (collapse >> atomMaker) argN cs
        ///Processes 0 arguments, then continues reading
        let inline arg0 cs atom =
            let list = atom::list
            match until with
            | OneArgument -> RETURN tableEnv cs list
            | _ -> read tableEnv until cs list
        ///Processes 1 argument, then continues reading
        let arg1 = argPlus1 arg0
        ///Processes 2 arguments, then continues reading
        let arg2 = argPlus1 arg1
        
        //* No Ok nor read in this function after this point or you risk ImplementationHasUnreadCharactersException *
        //* Use the arg functions, or if really necessary, RETURN! *

        match skipSpaces cs with
        | [] ->
            match until with
            | All | TableRow -> RETURN tableEnv [] list
            | OneArgument -> errorArgMissing
            | UntilRightDelimiter -> @"Missing \right" |> Error
            | Until '}' -> "Missing closing brace" |> Error
            | Until c -> "Expected character not found: " + c.ToString() |> Error
        | c::cs when (match until with Until u -> c = u | _ -> false) -> RETURN tableEnv cs list
        | '\\'::(CommandName (cmd, cs)) ->
            let inline infixFracCmd hasRule cs delim =
                match readBlock until collapse (fun cs atom -> Ok(struct(cs, atom))) cs with
                | Ok (struct(cs, denom)) ->
                    let numer = List.rev list
                    let frac =
                        Fraction (numer, denom, Center, Center, if hasRule then ValueNone else ValueSome 0.)
                    match delim with
                    | ValueSome struct(left, right) ->
                        match settings.Delimiters.[left] with
                        | Some left ->
                            match settings.Delimiters.[right] with
                            | Some right -> Delimited (left, frac, right) |> Ok
                            | None -> errorDelimMissing right
                        | None -> errorDelimMissing left
                    | ValueNone -> frac |> Ok
                | Error e -> Error e
            match cmd with
            | "1" -> TestResult___ "It's 1!" |> arg0 cs
            //Commands that return
            | "right" -> match until with
                         | UntilRightDelimiter -> RETURN tableEnv cs list
                         | _ -> Error @"Missing \left"
            | "over" -> ValueNone |> infixFracCmd true cs
            | "atop" -> ValueNone |> infixFracCmd false cs
            | "choose" -> ValueSome struct("(", ")") |> infixFracCmd false cs
            | "brack" -> ValueSome struct("[", "]") |> infixFracCmd false cs
            | "brace" -> ValueSome struct("{", "}") |> infixFracCmd false cs
            | "atopwithdelims" ->
                readDelimiter cs |> Result.bind (fun (left, cs) ->
                readDelimiter cs |> Result.bind (fun (right, cs) ->
                ValueSome(struct(left, right)) |> infixFracCmd cs))
            | @"\" | "cr" ->
                match tableEnv with
                | ValueSome env -> RETURN { env with NumRows = env.NumRows + 1 } cs list
                | ValueNone -> readTable ValueNone cs
            | "frac" -> (fun n d -> Fraction (n, d, Center, Center, ValueNone)) |> arg2 cs
            | "binom" -> (fun n d -> Fraction (n, d, Center, Center, ValueSome 0.)) |> arg2 cs
            | "sqrt" -> match readOption cs with
                        | Ok (ValueSome (cs, degree)) -> arg1 cs (fun radicand -> Radical (ValueSome degree, radicand))
                        | Ok ValueNone -> arg1 cs (fun radicand -> Radical (ValueNone, radicand))
                        | Error e -> Error e
            | "left" -> match readDelimiter cs with
                        | Ok (left, cs) -> 

                            readBlock UntilRightDelimiter collapse (fun cs inner ->
                               match readDelimiter cs with
                               | Ok (right, cs) -> Delimited (left, inner, right) |> arg0 cs
                               | Error e -> Error e
                            ) cs
                        | Error e -> Error e
            | "overline" -> arg1 cs Overlined
            | "underline" -> arg1 cs Underlined
            | "begin" -> match readEnvironment cs with
                         | Ok (env, cs) -> failwith "not implemented"
                         | Error e -> Error e
            | _ -> @"Unrecognized command: \" + cmd |> Error
        | '^'::cs -> arg1 cs Superscript
        | '_'::cs -> arg1 cs Subscript
        | '{'::cs -> readBlock (Until '}') Row arg0 cs
        | '}'::_ -> Error "Missing opening brace"
        //| '&'::cs ->
        | '\''::cs ->
            let primes, cs = List.partitionWhile ((=) '\'') cs //primes do not include the one already matched
            List.length primes + 1 |> Primes |> arg0 cs
        | c::cs -> string c |> Ordinary |> arg0 cs

    match read ValueNone All (List.ofSeq latex) [] with
    | Ok ([], atoms) -> collapse atoms |> Ok
    | Error e -> Error e
    | Ok (unreadChars, atoms) -> ImplementationHasUnreadCharactersException (latex, unreadChars, atoms) |> raise