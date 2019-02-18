module MathDisplay.MathAtom.LaTeX

open System.Collections.Generic
open MathDisplay.DataTypes

type Options = {
    Delimiters: AliasDictionary<string, Delimiter>
    Commands: AliasDictionary<string, MathAtom>
} with
    static member Default = {
        Delimiters = LaTeXDefaultMaps.Delimiters
        Commands = LaTeXDefaultMaps.Commands
    }
    

[<Struct>] type internal Read = Until of char | UntilRightDelimiter | OneArgument | All
type TableEnvironment = { Name:string voption; Ended:bool; NumRows:int }
exception ImplementationHasUnreadCharactersException of InputLaTeX:string * UnreadChars:char list * UnfinishedAtomList:MathAtom list
    with override this.Message = "The implementation has not read some characters yet, despite succeeding. The input LaTeX was: \n\n" + this.InputLaTeX
                               + "\n\nThe unread characters were: \n\n" + this.UnreadChars.ToString()
                               + "\n\nThe Unfinished atom list was: \n\n" + this.UnfinishedAtomList.ToString()

let ToAtom (settings: Options) latex =
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
                match settings.Delimiters.TryGetValueFSharp cmd with
                | true, v -> Ok (v, cs)
                | false, _ -> errorDelimMissing cmd
            | c::cs ->
                let c = string c
                match settings.Delimiters.TryGetValueFSharp c with
                | true, v -> Ok (v, cs)
                | false, _ -> errorDelimMissing c
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

        let processCommand cmd cs =
            match settings.Commands.TryGetValueFSharp cmd with
            | true, atom ->
                let inline readArgUntil' readArgUntil id until argDict addToArgDict tableEnv cs = //Inline for tail recursive optimizations
                    match read tableEnv until cs [] with
                    | Ok (tableEnv, cs, atoms) ->
                        collapse atoms |> addToArgDict
                        readArgUntil id tableEnv argDict cs
                    | Error e -> Error e
                let rec readArgUntil id tableEnv (argDict:LaTeXArgumentDictionary) cs =
                    match argDict.Required id with
                    | ValueSome arg -> Ok (tableEnv, arg)
                    | ValueNone ->
                        match cs with
                        | '['::cs -> readArgUntil' readArgUntil id (Until ']') argDict argDict.AddRequired tableEnv cs
                        | _ -> readArgUntil' readArgUntil id OneArgument argDict argDict.AddRequired tableEnv cs
                let rec readOptionalArgUntil id tableEnv (argDict:LaTeXArgumentDictionary) cs =
                    match argDict.Optional id with
                    | ValueSome arg -> Ok (tableEnv, ValueSome arg)
                    | ValueNone ->
                        match cs with
                        | '['::cs -> readArgUntil' readOptionalArgUntil id (Until ']') argDict argDict.AddOptional tableEnv cs
                        | _ -> Ok (tableEnv, ValueNone)

                let rec replaceArguments atom state =
                    //Not working...
                    //let replaceNp1 replaceN atomMaker (_, argDict, _ as state) atom = Result.bind (fun (tableEnv, atom', cs) -> replaceN (atomMaker atom') (tableEnv, argDict, cs)) (replaceArguments atom state)
                    let replace1 atomMaker state atom = Result.map (fun (tableEnv, atom', cs) -> (tableEnv, atomMaker atom', cs)) (replaceArguments atom state)
                    let replace2 atomMaker (_, argDict, _ as state) atom1 atom2 =
                        Result.bind (fun (tableEnv, atom', cs) -> replace1 (atomMaker atom') (tableEnv, argDict, cs) atom2) (replaceArguments atom1 state)
                    //let replace3 atomMaker (_, argDict, _ as state) atom1 atom2 atom3 =
                    //    Result.bind (fun (tableEnv, atom', cs) -> replace2 (atomMaker atom') (tableEnv, argDict, cs) atom3 atom2) (replaceArguments atom1 state)
                    let replaceList argDict = List.mapFoldResult (fun struct(tableEnv, cs) arg -> match replaceArguments arg (tableEnv, argDict, cs) with
                                                                                                  | Ok (tableEnv, result, cs) -> Ok (result, struct(tableEnv, cs))
                                                                                                  | Error e -> Error e)
                    match atom with
                    | Argument id ->
                        let tableEnv, argDict, cs = state
                        match readArgUntil id tableEnv argDict cs with
                        | Ok (tableEnv, atom) -> Ok (tableEnv, atom, cs)
                        | Error e -> Error e
                    | Argument_Optional (id, defaultValue) ->
                        let tableEnv, argDict, cs = state
                        match readOptionalArgUntil id tableEnv argDict cs with
                        | Ok (tableEnv, ValueSome atom) -> Ok (tableEnv, atom, cs)
                        | Ok (tableEnv, ValueNone) -> Ok (tableEnv, defaultValue, cs)
                        | Error e -> Error e
                    | Argument_AllAtoms dir ->
                        match dir with
                        | Backwards -> Ok (tableEnv, List.rev list |> collapse, cs)
                        | Forwards ->
                            match readBlock until collapse (fun cs atom -> Ok (cs, atom)) cs with
                            | Ok (cs, atom) -> Ok (tableEnv, atom, cs)
                            | Error e -> Error e
                    | Row list ->
                        let tableEnv, argDict, cs = state
                        match replaceList argDict struct(tableEnv, cs) list with
                        | Ok (list, struct(tableEnv, cs)) -> Ok (tableEnv, Row list, cs)
                        | Error e -> Error e
                    | Number _ | Variable _ | UnaryOperator _ | Ordinary _
                    | BinaryOperator _ | BinaryRelationalOperator _ | OpenBracket _ | CloseBracket _
                    | LargeOperator _ | Punctuation _ | PlaceholderInput | Primes _ | Space _ as atom -> Ok (tableEnv, atom, cs)
                    | Fraction (num, den, nAlign, dAlign, thickness) -> replace2 (fun num den -> Fraction (num, den, nAlign, dAlign, thickness)) state num den
                    | Radical (degree, radicand) -> replace2 (fun degree radicand -> Radical (degree, radicand)) state degree radicand
                    | Superscripted atom -> replace1 Superscripted state atom
                    | Subscripted atom -> replace1 Subscripted state atom
                    | Offsetted (atom, x, y) -> replace1 (fun atom -> Offsetted(atom, x, y)) state atom
                    | Delimited (left, atom, right) -> replace1 (fun atom -> Delimited(left, atom, right)) state atom
                    | Underlined atom -> replace1 Underlined state atom
                    | Overlined atom -> replace1 Overlined state atom
                    | Accented (atom, accent) -> replace1 (fun atom -> Accented(atom, accent)) state atom
                    | Styled (atom, style) -> replace1 (fun atom -> Styled(atom, style)) state atom
                    | Colored (atom, color) -> replace1 (fun atom -> Colored(atom, color)) state atom
                    | Table (atomss, interColumnSpacing, interRowAdditionalSpacing, columnAlignments) ->
                        let tableEnv, argDict, cs = state
                        match List.mapFoldResult (replaceList argDict) struct(tableEnv, cs) atomss with
                        | Ok(atomss, struct(tableEnv, cs)) -> Ok(tableEnv, Table(atomss, interColumnSpacing, interRowAdditionalSpacing, columnAlignments), cs)
                        | Error e -> Error e
                replaceArguments atom (tableEnv, LaTeXArgumentDictionary(), cs)
            | false, _ -> @"Unrecognized command: " + cmd |> Error

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
            match processCommand cmd cs with
            | Ok (tableEnv, atom, cs) -> arg0 cs atom
            | Error e -> Error e
        | '^'::cs -> arg1 cs Superscripted
        | '_'::cs -> arg1 cs Subscripted
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