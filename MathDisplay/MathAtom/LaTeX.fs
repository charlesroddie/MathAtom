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

        let processCommand cmd cs =
            match settings.Commands.TryGetValueFSharp cmd with
            | true, atom ->
                let rec readArgUntil id argMax (argDict:Dictionary<int, MathAtom>) cs =
                    if id <= argMax
                    then Ok cs
                    else
                        match read tableEnv OneArgument cs [] with
                        | Ok (tableEnv, cs, atoms) ->
                            let argMax = argMax + 1
                            let atom = collapse atoms
                            argDict.Add(argMax, atom)
                            readArgUntil id argMax argDict cs
                        | Error e -> Error e
                let rec replaceArguments atom state =
                    match atom with
                    | Argument id ->
                        let argMax, argDict, cs = state
                        if id <= argMax
                        then argDict.[id]
                        else
                            match readArgUntil id argMax argDict cs with
                            | Ok cs -> Ok (id, argDict.[id], cs)
                    | Argument_AllAtoms dir ->
                        //WIP!!!!!!!!!!!!!!!!!!!!!
                        match dir with
                        | Backwards -> List.rev list
                        | Forwards ->
                            match readBlock until collapse (fun cs atom -> Ok (cs, atom)) cs with
                            | Ok tup -> Ok tup
                            | Error e -> Error e
                    | Row list -> List.unfold (function
                                               | item::items -> match replaceArguments item state with
                                                                | Success _ as res -> Some (res, items)
                                                                | Error _ as res -> Some (res, [])
                                               | _ -> None) list
                    | Number _ | Variable _ | UnaryOperator _ | Ordinary _
                    | BinaryOperator _ | BinaryRelationalOperator _ | OpenBracket _ | CloseBracket _ as atom -> Ok atom
                    | LargeOperator (op, lower, upper) ->
                        match lower with
                        | ValueSome lower ->
                            match upper with
                            | ValueSome upper ->
                                match replaceArguments lower state with
                                | 
                        LargeOperator (op, ValueOption.map (fun low -> replaceArguments low state) lower, ValueOption.map (fun up -> replaceArguments up state) upper)
                    | Fraction (num, den, nAlign, dAlign, thickness) -> 
                    | Radical of degree:MathAtom voption * radicand:MathAtom
                    | Punctuation of char
                    | PlaceholderInput
                    //Scripts of previous atom
                    | Superscript of MathAtom
                    | Subscript of MathAtom
                    | Offsetted of x:float * y:float
                    | Delimited of left:Delimiter * atom:MathAtom * right:Delimiter
                    | Underlined of MathAtom
                    | Overlined of MathAtom
                    | Accented of MathAtom * Accent
                    | Primes of count:int
                    //| Boundary (changed to Delimiter)
                    | Space of float<mu>
                    ///Style changes during rendering
                    | Styled of Style * MathAtom
                    | Text of string
                    | Colored of System.Drawing.Color * MathAtom
                    ///A table. Not part of TeX.
                    | Table of MathAtom list list * interColumnSpacing:float<mu> * interRowAdditionalSpacing:float<mu> * columnAlignments: Alignment list
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
        | '\\'::CommandName (cmd, cs) -> processCommand (@"\" + cmd)
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