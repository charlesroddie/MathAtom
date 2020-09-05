namespace MathDisplay.MathAtom
open MathDisplay.DataTypes

[<Struct>] type Accent = Accent of char
[<Struct>] type Operator = Operator of char
type Style = class end
[<Struct>] type Delimiter = Delimiter of string

[<Measure>] type mu

[<RequireQualifiedAccess>]
type Decoration =
    | Underlined
    | Overlined
    | Accented of Accent

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
    | Scripts of baseAtom:MathAtom * superscript:MathAtom voption * subscript:MathAtom voption
    //| Offsetted of x:float * y:float
    | Delimited of left:Delimiter * atom:MathAtom * right:Delimiter
    | Decorated of Decoration * MathAtom
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
        | MathAtom.Radical(_,b), BaseAtom
        | MathAtom.Scripts(b,_,_), BaseAtom
        | MathAtom.Delimited(_,b,_), BaseAtom
        | MathAtom.Decorated(_,b), BaseAtom
            -> ValueSome b
        | MathAtom.Scripts(_,superscript,_), Superscript ->
            superscript
        | MathAtom.Scripts(_,_,subscript), Subscript ->
            subscript
        | MathAtom.LargeOperator(_,lowerLimit,_), Subscript ->
            lowerLimit
        | MathAtom.LargeOperator(_,_,upperLimit), Superscript ->
            upperLimit
        | MathAtom.Radical(d,_), Subscript ->
            d
        | _ -> ValueNone
    /// Immediate subatoms of a MathAtom, together with AtomIndexSteps
    static member internal SubAtomsWithAtomIndexSteps(ma:MathAtom) =
        match ma with
        | MathAtom.Row l ->
            l |> List.mapi (fun i atom -> (atom, RowIndex i))
        | MathAtom.LargeOperator(_,ll,ul) ->
            [   match ll with
                | ValueSome a -> yield (a, Subscript)
                | ValueNone -> ()
                match ul with
                | ValueSome a -> yield (a, Superscript)
                | ValueNone -> ()
            ]
        | MathAtom.Fraction(n,d,_,_,_) ->
            [ (n, Numerator); (d, Denominator) ]
        | MathAtom.Radical(degree, radicand) ->
            [   match degree with
                | ValueSome a -> yield (a, RadicalDegree)
                | ValueNone -> ()
                yield (radicand, BaseAtom)
            ]
        | MathAtom.Scripts(baseAtom, superscript, subscript) ->
            [   yield (baseAtom, BaseAtom)
                match superscript with
                | ValueSome a -> yield (a, Superscript)
                | ValueNone -> ()
                match subscript with
                | ValueSome a -> yield (a, Subscript)
                | ValueNone -> ()
            ]
        | MathAtom.Delimited(_,b,_)
        | MathAtom.Decorated(_,b)
            -> [ (b, BaseAtom) ]
        | MathAtom.Number _
        | MathAtom.Variable _
        | MathAtom.UnaryOperator _
        | MathAtom.Ordinary _
        | MathAtom.BinaryOperator _
        | MathAtom.BinaryRelationalOperator _
        | MathAtom.OpenBracket _
        | MathAtom.CloseBracket _
        | MathAtom.Punctuation _
        | MathAtom.PlaceholderInput
        | MathAtom.Primes _
            -> []

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
    /// Useful for index tests
    static member AllSubAtomsWithAtomIndices(ma:MathAtom) =
        [   yield ma, Self
            for (subAtom, ais) in AtomIndexStep.SubAtomsWithAtomIndexSteps ma do
                yield!
                    AtomIndex.AllSubAtomsWithAtomIndices subAtom
                    |> List.map (fun (a,ai) -> a, AtomIndexStep(ais,ai))
        ]