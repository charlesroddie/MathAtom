namespace MathDisplay.MathAtom
open MathDisplay.DataTypes

[<Struct>] type Accent = Accent of char
[<Struct>] type Operator = Operator of char
type Style = class end
[<Struct>] type Delimiter = Delimiter of string

[<Measure>] type mu

[<RequireQualifiedAccess>]
type MathAtom =
    | Row of MathAtom list
    (*| Ordinary = Number | Variable | UnaryOperator*)
    | Number of string
    | Variable of char
    | UnaryOperator of char
    | Ordinary of string
    /// sin/cos, integral, etc.
    | LargeOperator of Operator * lowerLimit:MathAtom voption * upperLimit:MathAtom voption
    | BinaryOperator of char
    | BinaryRelationalOperator of char
    //Bracket characters, need not be balanced.
    | OpenBracket of char
    | CloseBracket of char
    | Fraction of numerator:MathAtom * denominator:MathAtom * nXAlign:Alignment * dXAlign:Alignment * customRuleThickness:float voption
    | Radical of degree:MathAtom voption * radicand:MathAtom
    | Punctuation of char
    | PlaceholderInput
    | Scripts of baseAtom:MathAtom * superscript:MathAtom * subscript:MathAtom
    //| Offsetted of x:float * y:float
    | Delimited of left:Delimiter * atom:MathAtom * right:Delimiter
    | Underlined of MathAtom
    | Overlined of MathAtom
    | Accented of MathAtom * Accent
    | Primes of count:int
    //| Boundary (changed to Delimiter)
    //| Space of float<mu>
    //| Styled of Style * MathAtom
    //| Text of string
    //| Colored of System.Drawing.Color * MathAtom
    ///A table. Not part of TeX.
    //| Table of MathAtom list list * interColumnSpacing:float<mu> * interRowAdditionalSpacing:float<mu> * columnAlignments: Alignment list


/// A sigle step of indexing, describing the position of an immediate strict subAtom of an Atom
[<RequireQualifiedAccess>]
type AtomIndexStep =
    | RowIndex of int
    | Numerator
    | Denominator
    | BaseAtom
    | Superscript
    | Subscript
    | RadicalDegree
    static member SubAtom(a:MathAtom, ai:AtomIndexStep) =
        match a, ai with
        | MathAtom.Row l, RowIndex i ->
            match l |> List.tryItem i with
            | Some a -> ValueSome a
            | None -> ValueNone
        | MathAtom.Fraction(n,_,_,_,_), Numerator ->
            ValueSome n
        | MathAtom.Fraction(_,d,_,_,_), Denominator ->
            ValueSome d
        | MathAtom.Radical(_,r), BaseAtom ->
            ValueSome r
        | MathAtom.Scripts(b,_,_), BaseAtom ->
            ValueSome b
        | MathAtom.Scripts(_,superscript,_), Superscript ->
            ValueSome superscript
        | MathAtom.Scripts(_,_,subscript), Subscript ->
            ValueSome subscript
        | MathAtom.Radical(d,_), Subscript ->
            d
        | _ -> ValueNone

/// Describes the position of a subAtom inside an Atom
[<RequireQualifiedAccess>]
type AtomIndex =
    | Self
    | AtomIndexStep of AtomIndexStep * AtomIndex
    static member SubAtom(a:MathAtom, ai:AtomIndex) =
        match ai with
        | Self -> ValueSome a
        | AtomIndexStep(ais, subIndex) ->
            AtomIndexStep.SubAtom(a,ais) |> ValueOption.bind (fun innerAtom ->
                AtomIndex.SubAtom(innerAtom, subIndex)
                )