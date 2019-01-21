namespace MathDisplay.MathAtom
open MathDisplay.DataTypes
open MathDisplay

type Accent = class end
type Operator = class end

type MathAtom =
    | Row of MathAtom list
    (*| Ordinary = *)
    | Number of string
    | Variable of char
    | UnaryOperator of char
    /// sin/cos, integral, etc.
    | LargeOperator of Operator * lowerLimit:MathAtom * upperLimit:MathAtom
    | BinaryOperator of char
    | BinaryRelationalOperator of char
    | Bracketed of LeftBracket option * MathAtom * RightBracket option
    | Fraction of numerator:MathAtom * denominator:MathAtom * nXAlign:Alignment * dXAlign:Alignment * customRuleThickness:float option
    | Radical of degree:MathAtom option * radicand:MathAtom
    | Punctuation of char
    | PlaceholderInput
    | Scripts of baseAtom:MathAtom * subscriptAtom: MathAtom option * superscriptAtom: MathAtom option
    | Offsetted of x:float * y:float
    | Delimited of left:char * atom:MathAtom * right:char
    | Underlined of MathAtom
    | Overlined of MathAtom
    | Accented of MathAtom * Accent
    | Primes of count:int
    //| Boundary (changed to Delimiter)
    | Space of Space
    ///Style changes during rendering
    | Styled of MathAtom * LineStyle
    | Colored of MathAtom * System.Drawing.Color
    ///A table. Not part of TeX.
    | Table of MathAtom list list * interColumnSpacing:Space * interRowAdditionalSpacing:Space * columnAlignments: Alignment list