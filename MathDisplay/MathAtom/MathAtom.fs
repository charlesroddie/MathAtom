namespace MathDisplay.MathAtom
open MathDisplay.DataTypes

[<Struct>] type Accent = Accent of char
[<Struct>] type Operator = Operator of char
type Style = class end
[<Struct>][<RequireQualifiedAccess>]
type Delimiter =
    | Empty
    | LBracket
    | RBracket
    | LSquareBracket
    | RSquareBracket
    | LCurlyBracket
    | RCurlyBracket
    | LAngle
    | RAngle
    | ForwardSlash
    | BackSlash
    | Vert
    | DoubleVert
    | UpArrow
    | DownArrow
    | DoubleUpArrow
    | DoubleDownArrow
    | UpDownArrow
    | LGroup
    | RGroup
    | LCeil
    | RCeil
    | LFloor
    | RFloor
    member t.Char =
        match t with
        | Empty -> ""
        | LBracket -> "("
        | RBracket -> ")"
        | LSquareBracket -> "["
        | RSquareBracket -> "]"
        | LCurlyBracket -> "{"
        | RCurlyBracket -> "}"
        | LAngle -> "\u2329"
        | RAngle -> "\u232A"
        | ForwardSlash -> "/"
        | BackSlash -> "\\"
        | Vert -> "|"
        | DoubleVert -> "\u2016"
        | UpArrow -> "\u2191"
        | DownArrow -> "\u2193"
        | DoubleUpArrow -> "\u21D1"
        | DoubleDownArrow -> "\u21D3"
        | UpDownArrow -> "\u21D5"
        | LGroup -> "\u27EE"
        | RGroup -> "\u27EF"
        | LCeil -> "\u2308"
        | RCeil -> "\u2309"
        | LFloor -> "\u230A"
        | RFloor -> "\u230B"

[<Measure>] type mu

type MathAtom =
    | Argument of id:int
    | Argument_Optional of id:int * defaultValue:MathAtom
    | Row of MathAtom list
    (*| Ordinary = Number | Variable | UnaryOperator*)
    | Number of string
    | Variable of char
    | UnaryOperator of char
    | Ordinary of string
    /// sin/cos, integral, etc.
    | LargeOperator of Operator
    | BinaryOperator of char
    | BinaryRelationalOperator of char
    //Bracket characters, need not be balanced.
    | OpenBracket of char
    | CloseBracket of char
    | Fraction of numerator:MathAtom * denominator:MathAtom * nXAlign:Alignment * dXAlign:Alignment * customRuleThickness:float voption
    //(Row []) to indicate empty degree
    | Radical of degree:MathAtom * radicand:MathAtom
    | Punctuation of char
    | PlaceholderInput
    //Scripts of previous atom
    | Superscripted of MathAtom
    | Subscripted of MathAtom
    | Offsetted of MathAtom * x:float * y:float
    | Delimited of left:Delimiter * atom:MathAtom * right:Delimiter
    | Underlined of MathAtom
    | Overlined of MathAtom
    | Accented of MathAtom * Accent
    | Primes of count:int
    //| Boundary (changed to Delimiter)
    | Space of float<mu>
    ///Style changes during rendering
    | Styled of MathAtom * Style
    //| Text of string -> | Ordinary of string
    | Colored of MathAtom * System.Drawing.Color
    ///A table. Not part of TeX.
    | Table of MathAtom list list * interColumnSpacing:float<mu> * interRowAdditionalSpacing:float<mu> * columnAlignments: Alignment list