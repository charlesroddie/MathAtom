namespace MathDisplay.MathAtom
open MathDisplay.DataTypes

type Accent = class end
type Operator = class end
type Style = class end
[<Struct>] type Delimiter = Delimiter of string

[<Measure>] type mu

type MathAtom =
    | Row of MathAtom list
    (*| Ordinary = Number | Variable | UnaryOperator*)
    | Number of string
    | Variable of char
    | UnaryOperator of char
    /// sin/cos, integral, etc.
    | LargeOperator of Operator * lowerLimit:MathAtom * upperLimit:MathAtom
    | BinaryOperator of char
    | BinaryRelationalOperator of char
    //Bracket characters, need not be balanced.
    | LeftBracket of char
    | RightBracket of char
    | Fraction of numerator:MathAtom * denominator:MathAtom * nXAlign:Alignment * dXAlign:Alignment * customRuleThickness:float option
    | Radical of degree:MathAtom option * radicand:MathAtom
    | Punctuation of char
    | PlaceholderInput
    | Scripts of baseAtom:MathAtom * subscriptAtom: MathAtom option * superscriptAtom: MathAtom option
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