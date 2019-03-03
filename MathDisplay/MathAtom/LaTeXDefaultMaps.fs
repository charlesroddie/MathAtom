module MathDisplay.MathAtom.LaTeXDefaultMaps

open MathDisplay.DataTypes

//Use (Alt+Left mouse) drag to create multiple cursors so that spaces can be inputted simultaneously
let Delimiters =
    [   ["."], Delimiter.Empty // . means no delimiter
        ["("], Delimiter.LBracket
        [")"], Delimiter.RBracket
        ["["], Delimiter.LSquareBracket
        ["]"], Delimiter.RSquareBracket
        ["{";"lbrace"], Delimiter.LCurlyBracket
        ["}";"rbrace"], Delimiter.RCurlyBracket
        ["<";"langle"], Delimiter.LAngle
        [">";"rangle"], Delimiter.RAngle
        ["/"], Delimiter.ForwardSlash
        ["\\";"backslash"], Delimiter.BackSlash
        ["|";"vert"], Delimiter.Vert
        ["||";"Vert"], Delimiter.DoubleVert
        ["uparrow"], Delimiter.UpArrow
        ["downarrow"], Delimiter.DownArrow
        ["updownarrow"], Delimiter.UpDownArrow
        ["Uparrow"], Delimiter.UpArrow
        ["Downarrow"], Delimiter.DoubleDownArrow
        ["Updownarrow"], Delimiter.UpDownArrow
        ["lgroup"], Delimiter.LGroup
        ["rgroup"], Delimiter.RGroup
        ["lceil"], Delimiter.LCeil
        ["rceil"], Delimiter.RCeil
        ["lfloor"], Delimiter.LFloor
        ["rfloor"], Delimiter.RFloor]
        
    |> AliasDictionary

let MatrixEnvironments =
    [   ["matrix"], (Delimiter.Empty, Delimiter.Empty)
        ["pmatrix"], (Delimiter.LBracket, Delimiter.RBracket)
        ["bmatrix"], (Delimiter.LBracket, Delimiter.RBracket)
        ["Bmatrix"], (Delimiter.LCurlyBracket, Delimiter.RCurlyBracket)
        ["vmatrix"], (Delimiter.Vert, Delimiter.Vert)
        ["Vmatrix"], (Delimiter.DoubleVert, Delimiter.DoubleVert)]
    |> AliasDictionary
    
let ``(charToAtom) <--- Unused for now....`` c =
    let (|Space|_|) s = if System.Char.IsControl s || System.Char.IsWhiteSpace s then Some Space else None
    match c with
    | _ when '0' <= c && c <= '9' -> string c |> Number |> ValueSome
    | _ when ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') -> Variable c |> ValueSome
    | '$' | '%' | '#' | '&' | '~' | '\'' | '^' | '_' | '{' | '}' | '\\' | Space -> ValueNone // All these are special characters we don't support.
    | '(' | '[' -> OpenBracket c |> ValueSome
    | ')' | ']' | '!' | '?' -> CloseBracket c |> ValueSome
    | ',' | ';' -> Punctuation c |> ValueSome
    | '<' | '=' | '>' -> BinaryRelationalOperator c |> ValueSome
    | ':' | '\u2236' -> BinaryRelationalOperator '\u2236' |> ValueSome // Colon is a ratio. Regular colon is \colon
    | '+' | '*' -> BinaryOperator c |> ValueSome
    | '-' | '\u2212' -> BinaryOperator '\u2212' |> ValueSome // use the math minus sign
    | '.' -> string c |> Number |> ValueSome
    | '"' | '/' | '@' | '`' | '|' | _ -> string c |> Ordinary |> ValueSome
    
let Commands =
    [   ["frac"], Fraction (Argument 1, Argument 2, Center, Center, ValueNone)
        ["sqrt"], Radical (Argument_Optional (1, Row []), Argument 1)
        ["1"], Ordinary "1"]
    |> AliasDictionary