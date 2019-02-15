namespace MathDisplay.MathAtom

open System.Collections.Generic

type LaTeXArgumentDictionary() =
    let required = new List<MathAtom>()
    let optional = new List<MathAtom>()
    member __.AddOptional atom = optional.Add atom
    member __.AddRequired atom = required.Add atom
    member __.Optional with get id = if id < optional.Count then ValueSome optional.[id] else ValueNone
    member __.Required with get id = if id < required.Count then ValueSome required.[id] else ValueNone