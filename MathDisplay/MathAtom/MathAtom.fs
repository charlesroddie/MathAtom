namespace MathDisplay.MathAtom
open MathDisplay.DataTypes

[<Struct>] type Accent = Accent of char
[<Struct>] type Operator = Operator of char
[<Struct>] type FontStyle = 
               Default | Roman | Bold | Caligraphic
               ///i.e. Monospace
               | Typewriter | Italic | SansSerif | Fraktur | Blackboard | BoldItalic
//LineStyle wrong, Size correct. See https://en.wikibooks.org/wiki/LaTeX/Advanced_Mathematics#Changing_font_size
[<Struct>] type Size = Display | Text | Script | ScriptScript
[<Struct>] type Delimiter = Delimiter of string

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
    ///Size changes during rendering
    | Resized of MathAtom * Size
    | Styled of MathAtom * FontStyle
    //| Text of string -> | Ordinary of string
    | Colored of MathAtom * System.Drawing.Color
    ///A table. Not part of TeX.
    | Table of MathAtom list list * interColumnSpacing:float<mu> * interRowAdditionalSpacing:float<mu> * columnAlignments: Alignment list