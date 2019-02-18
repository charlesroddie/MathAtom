module MathDisplay.MathAtom.LaTeXDefaultMaps

open MathDisplay.DataTypes

//Use (Alt+Left mouse) drag to create multiple cursors so that spaces can be inputted simultaneously
[<CompiledName "Delimiters">]
let Delimiters =
   [".",           [],            "" // . means no delimiter
    "(",           [],            "("
    ")",           [],            ")"
    "[",           [],            "["
    "]",           [],            "]"
    "{",           ["lbrace"],    "{"
    "}",           ["rbrace"],    "}"
    "<",           ["langle"],    "\u2329"
    ">",           ["rangle"],    "\u232A"
    "/",           [],            "/"
    "\\",          ["backslash"], "\\"
    "|",           ["vert"],      "|"
    "||",          ["Vert"],      "\u2016"
    "uparrow",     [],            "\u2191"
    "downarrow",   [],            "\u2193"
    "updownarrow", [],            "\u2195"
    "Uparrow",     [],            "\u21D1"
    "Downarrow",   [],            "\u21D3"
    "Updownarrow", [],            "\u21D5"
    "lgroup",      [],            "\u27EE"
    "rgroup",      [],            "\u27EF"
    "lceil",       [],            "\u2308"
    "rceil",       [],            "\u2309"
    "lfloor",      [],            "\u230A"
    "rfloor",      [],            "\u230B"]
        
    |> aliasDictValueMap Delimiter

[<CompiledName "MatrixEnvironments">]
let matrixEnvironments =
    ["matrix",  [], (".", ".")
     "pmatrix", [], ("(", ")")
     "bmatrix", [], ("[", "]")
     "Bmatrix", [], ("{", "}")
     "vmatrix", [], ("|", "|")
     "Vmatrix", [], ("||", "||")]
    |> aliasDictValueMap (fun (l, r) -> (Delimiters.[l], Delimiters.[r]))
    
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

open MathDisplay.MathAtom.LaTeXCommand

let Commands =
    [@"frac", [], Fraction (Argument 1, Argument 2, Center, Center, ValueNone)
     @"1", [], Ordinary "1"]
    |> aliasDict