namespace MathDisplay.MathAtom

open MathDisplay

type MathAtom =
    | Row of MathAtom list
    (*| Ordinary = *) | Number of chars | Variable of chars | UnaryOperator of chars
    /// sin/cos, integral, etc.
    | LargeOperator
    | BinaryOperator of unichar
    | BinaryRelationalOperator of unichar
    | OpenBracket of unichar
    | CloseBracket of unichar
    | Fraction of numerator:MathAtom * denominator:MathAtom * nAlign:XAlignment * dAlign:XAlignment * customRuleThickness:float option
    | Radical of degree:MathAtom option * radicand:MathAtom
    | Punctuation of unichar
    | PlaceholderInput
    | Superscripted of MathAtom
    | Subscripted of MathAtom
    | Offsetted of x:float * y:float
    | Delimited of left:Delimiter * atom:MathAtom * right:Delimiter
    | Underlined of MathAtom
    | Overlined of MathAtom
    | Accented of MathAtom
    | Primes of count:int
    //| Boundary (changed to Delimiter)
    | Space
    ///Style changes during rendering
    | Style
    | Colored of MathAtom * Color
    ///A table. Not part of TeX.
    | Table of MathAtom list list * interColumnSpacing:float<mu> * interRowAdditionalSpacing:float<mu> * columnAlignments: XAlignment list