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
    /// Simplifies a Row by replacing a Row with a single item x with x.
    let collapseRow(xs:MathAtom list) = match xs with [x] -> x | x -> Row x
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
                match settings.Delimiters.TryGetValue cmd with
                | Some v -> Ok (v, cs)
                | None -> errorDelimMissing cmd
            | c::cs ->
                let c = string c
                match settings.Delimiters.TryGetValue c with
                | Some v -> Ok (v, cs)
                | None -> errorDelimMissing c
        ///Reads an environment
        let readEnvironment cs =
            match cs with
            | '{'::PartitionAlphabets (ab, '}'::cs) -> Ok (System.String.Concat ab, cs)
            | c::cs -> Ok (string c, cs) //Seems allowed by LaTeX?
            | _ -> "Invalid environment, contains non-A-to-Z characters: " + System.String.Concat cs |> Error
        let readTable name cs = Error "ghvgjhkjnlbvgcfy"
            //let rec innerReadTable rows currentRow cs =
                //read (ValueSome { Name = name; Ended = false; NumRows = 0 }) until cs list
                //|> Result.bind (fun (cs, atoms) ->
                //    let atom = collapse atoms
                //    match cs with
                //    | '&'::cs -> innerReadTable 
                //)
        
        let processAtom atom cs =
            let rec readArgsUntilId id tableEnv (argDict:LaTeXArgumentDictionary) cs =
                match argDict.Required id with
                | ValueSome arg -> Ok (tableEnv, arg, cs)
                | ValueNone ->
                    match cs with
                    | '['::cs ->
                        read tableEnv (Until ']') cs []
                        |> Result.bind (fun (tableEnv, cs, atoms) ->
                            collapseRow atoms |> argDict.AddOptional
                            readArgsUntilId id tableEnv argDict cs)
                    | [] -> Error "Unexpected end of text, argument missing"
                    | _ ->
                        read tableEnv OneArgument cs []
                        |> Result.bind (fun (tableEnv, cs, atoms) ->
                            collapseRow atoms |> argDict.AddRequired
                            readArgsUntilId id tableEnv argDict cs)
            let rec readOptionalArgsUntilId id tableEnv (argDict:LaTeXArgumentDictionary) cs =
                match argDict.Optional id with
                | ValueSome arg -> Ok (tableEnv, ValueSome arg, cs)
                | ValueNone ->
                    match cs with
                    | '['::cs ->
                        read tableEnv (Until ']') cs []
                        |> Result.bind (fun (tableEnv, cs, atoms) ->
                            collapseRow atoms |> argDict.AddOptional
                            readOptionalArgsUntilId id tableEnv argDict cs)
                    | _ -> Ok (tableEnv, ValueNone, cs)

            ///<summary>
            ///Looks for <see cref="Argument"/> and <see cref="Argument_Optional"/> and replaces them with arguments from LaTeX input
            ///</summary>
            let rec replaceArguments atom state =
                //Not working...
                //let replaceNp1 replaceN atomMaker (_, argDict, _ as state) atom = Result.bind (fun (tableEnv, atom', cs) -> replaceN (atomMaker atom') (tableEnv, argDict, cs)) (replaceArguments atom state)
                let replace1 atomMaker state atom = Result.map (fun (tableEnv, atom', cs) -> (tableEnv, atomMaker atom', cs)) (replaceArguments atom state)
                let replace2 atomMaker (_, argDict, _ as state) atom1 atom2 =
                    Result.bind (fun (tableEnv, atom', cs) -> replace1 (atomMaker atom') (tableEnv, argDict, cs) atom2) (replaceArguments atom1 state)
                //let replace3 atomMaker (_, argDict, _ as state) atom1 atom2 atom3 =
                //    Result.bind (fun (tableEnv, atom', cs) -> replace2 (atomMaker atom') (tableEnv, argDict, cs) atom3 atom2) (replaceArguments atom1 state)
                let replaceList argDict = List.mapFoldResult (fun struct(tableEnv, cs) arg ->
                    replaceArguments arg (tableEnv, argDict, cs) |> Result.map (fun (tableEnv, result, cs) -> result, struct(tableEnv, cs)))
                match atom with
                | Argument id ->
                    let tableEnv, argDict, cs = state
                    readArgsUntilId id tableEnv argDict cs
                | Argument_Optional (id, defaultValue) ->
                    let tableEnv, argDict, cs = state
                    match readOptionalArgsUntilId id tableEnv argDict cs with
                    | Ok (tableEnv, ValueSome atom, cs) -> Ok (tableEnv, atom, cs)
                    | Ok (tableEnv, ValueNone, cs) -> Ok (tableEnv, defaultValue, cs)
                    | Error e -> Error e
                | Row list ->
                    let tableEnv, argDict, cs = state
                    replaceList argDict struct(tableEnv, cs) list
                    |> Result.map (fun (list, struct(tableEnv, cs)) -> tableEnv, Row list, cs)
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
                    List.mapFoldResult (replaceList argDict) struct(tableEnv, cs) atomss
                    |> Result.map (fun (atomss, struct(tableEnv, cs)) -> tableEnv, Table(atomss, interColumnSpacing, interRowAdditionalSpacing, columnAlignments), cs)
            replaceArguments atom (tableEnv, LaTeXArgumentDictionary(), cs)

        let continueReading (tableEnv, atom, cs) =
            let list = atom::list
            match until with
            | OneArgument -> (tableEnv, cs, list) |> Ok
            | _ -> read tableEnv until cs list

        let processAtomCommand cmd cs =
            match settings.Commands.TryGetValue cmd with
            | Some atom -> processAtom atom cs
            | None -> @"Unrecognized command: " + cmd |> Error

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
            match cmd with
            | "left" ->
                readDelimiter cs
                |> Result.bind (fun (left, cs) -> 
                read tableEnv UntilRightDelimiter cs []
                |> Result.bind (fun (tableEnv, cs, list) ->
                readDelimiter cs
                |> Result.bind (fun (right, cs) ->
                continueReading (tableEnv, Delimited (left, collapseRow list, right), cs))))
            | "right" ->
                match until with
                | UntilRightDelimiter -> (tableEnv, cs, List.rev list) |> Ok
                | _ -> Error @"Missing \left"
            | _ -> processAtomCommand cmd cs |> Result.bind continueReading
        | '^'::cs -> processAtom (Superscripted (Argument 1)) cs |> Result.bind continueReading
        | '_'::cs -> processAtom (Subscripted (Argument 1)) cs |> Result.bind continueReading
        | '{'::cs -> read tableEnv (Until '}') cs [] |> Result.bind (fun (tableEnv, cs, list) -> continueReading (tableEnv, Row list, cs))
        | '}'::_ -> Error "Missing opening brace"
        //| '&'::cs ->
        | '\''::cs ->
            let primes, cs = List.partitionWhile ((=) '\'') cs //primes do not include the one already matched
            continueReading (tableEnv, List.length primes + 1 |> Primes, cs)
        | c::cs -> continueReading (tableEnv, string c |> Ordinary, cs)
    match read ValueNone All (List.ofSeq latex) [] with
    | Ok (ValueNone, [], atoms)
    | Ok (ValueSome { Name = ValueNone }, [], atoms) ->
        collapseRow atoms |> Ok
    | Ok (ValueSome { Name = ValueSome envName }, [], _) ->
        (@"Missing \end{" + envName + "}") |> Error
    | Error e -> Error e
    | Ok (_, unreadChars, atoms) -> ImplementationHasUnreadCharactersException (latex, unreadChars, atoms) |> raise