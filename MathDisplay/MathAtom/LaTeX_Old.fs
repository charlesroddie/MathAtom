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
    let errorDelimMissing cmd = Error (cmd + " was not found in delimiter map")
    let collapse xs = match xs with [x] -> x | x -> Row x
    let skipSpaces cs = List.skipWhile System.Char.IsWhiteSpace cs
    let isAlphabet c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
    let (|PartitionAlphabets|) cs = List.partitionWhile isAlphabet cs
    let (|CommandName|) cs = match cs with
                             | c::cs when not <| isAlphabet c -> string c, cs
                             | PartitionAlphabets (ab, cs) -> System.String.Concat ab, skipSpaces cs
    let rec read tableEnv until cs list =
        ///Reads a delimiter
        let readDelimiter cs =
            match cs with
            | [] -> errorArgMissing
            | '\\'::CommandName(cmd, cs) ->
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
        let readBlock until atomMaker useAtom cs =
            read tableEnv until cs [] |> Result.bind (fun (_, cs, arg) -> atomMaker arg |> useAtom cs)
        ///Reads an optional argument and returns it, cs should start with '[' to be recognized
        let readOption cs =
            match cs with
            | '['::cs -> readBlock (Until ']') collapse (fun cs atom -> (cs, atom) |> ValueSome |> Ok) cs
            | _ -> Ok ValueNone
        let readTable name cs = Error "ghvgjhkjnlbvgcfy"
            //let rec innerReadTable rows currentRow cs =
                //read (ValueSome { Name = name; Ended = false; NumRows = 0 }) until cs list
                //|> Result.bind (fun (cs, atoms) ->
                //    let atom = collapse atoms
                //    match cs with
                //    | '&'::cs -> innerReadTable 
                //)
        ///Processes (N+1) arguments, then continues reading
        let inline argPlus1 argN cs atomMaker = readBlock OneArgument (collapse >> atomMaker) argN cs
        ///Processes 0 arguments, then continues reading
        let arg0 cs atom =
            let list = atom::list
            match until with
            | OneArgument -> (tableEnv, cs, list) |> Ok
            | _ -> read tableEnv until cs list
        ///Processes 1 argument, then continues reading
        let arg1 = argPlus1 arg0
        ///Processes 2 arguments, then continues reading
        let arg2 = argPlus1 arg1

        //* No calls to read in this function after this point or you risk ImplementationHasUnreadCharactersException *
        //* Use the arg functions!

        match skipSpaces cs with
        | [] ->
            match until with
            | All -> (tableEnv, [], List.rev list) |> Ok
            | OneArgument -> errorArgMissing
            | UntilRightDelimiter -> @"Missing \right" |> Error
            | Until '}' -> "Missing closing brace" |> Error
            | Until c -> "Expected character not found: " + c.ToString() |> Error
        | c::cs when (match until with Until u -> c = u | _ -> false) -> (tableEnv, cs, List.rev list) |> Ok
        | '\\'::CommandName (cmd, cs) ->
            let infixFracCmd hasRule cs delims =
                match readBlock until collapse (fun cs atom -> Ok struct(cs, atom)) cs with
                | Ok struct(cs, denom) ->
                    let numer = List.rev list |> collapse
                    let frac = Fraction (numer, denom, Center, Center, if hasRule then ValueNone else ValueSome 0.)
                    Ok (tableEnv, cs,
                        [match delims with
                         | ValueSome struct(left, right) -> yield Delimited (left, frac, right)
                         | ValueNone -> yield frac])
                | Error e -> Error e
            match cmd with
            | "1" -> TestResult___ "It's 1!" |> arg0 cs
            //Commands that return
            | "right" -> match until with
                         | UntilRightDelimiter -> (tableEnv, cs, List.rev list) |> Ok
                         | _ -> Error @"Missing \left"
            | "over" -> ValueNone |> infixFracCmd true cs
            | "atop" -> ValueNone |> infixFracCmd false cs
            | "choose" -> ValueSome struct(Delimiter "(", Delimiter ")") |> infixFracCmd false cs
            | "brack" -> ValueSome struct(Delimiter "[", Delimiter "]") |> infixFracCmd false cs
            | "brace" -> ValueSome struct(Delimiter "{", Delimiter "}") |> infixFracCmd false cs
            | "atopwithdelims" ->
                readDelimiter cs |> Result.bind (fun (left, cs) ->
                readDelimiter cs |> Result.bind (fun (right, cs) ->
                ValueSome(struct(left, right)) |> infixFracCmd false cs))
            | @"\" | "cr" ->
                match tableEnv with
                | ValueSome env -> (ValueSome { env with NumRows = env.NumRows + 1 }, cs, List.rev list) |> Ok
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
    | Ok (ValueNone, [], atoms)
    | Ok (ValueSome { Name = ValueNone }, [], atoms) ->
        collapse atoms |> Ok
    | Ok (ValueSome { Name = ValueSome envName }, [], _) ->
        (@"Missing \end{" + envName + "}") |> Error
    | Error e -> Error e
    | Ok (_, unreadChars, atoms) -> ImplementationHasUnreadCharactersException (latex, unreadChars, atoms) |> raise