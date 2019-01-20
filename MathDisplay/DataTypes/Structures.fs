namespace MathDisplay.DataTypes

type 'T memory = System.Memory<'T>
type chars = char memory

[<Struct>] type XAlignment = Left | Center | Right
[<Struct>] type YAlignment = Top | Center | Bottom
[<Struct>] type Color = { r:byte; g:byte; b:byte; a:byte }
[<Struct>] type SpaceType = Points | MathUnits
[<Struct>] type Space = { Length:float; SpaceType:SpaceType }
[<Struct>] type LineStyle = Display | Text | Script | ScriptScript

[<Struct>] type UnicodeCharacter = private UnicodeCharacter of chars
type unichar = UnicodeCharacter
module UnicodeCharacter =
    let Create (x:chars) =
        match x.Length with
        | 1 when x.Span.[0] |> System.Char.IsSurrogate |> not ->
            x |> UnicodeCharacter |> ValueSome
        | 2 when x.Span.[0] |> System.Char.IsHighSurrogate && x.Span.[1] |> System.Char.IsLowSurrogate ->
            x |> UnicodeCharacter |> ValueSome
        | _ -> ValueNone
    let ValueOf (UnicodeCharacter x) = x
    let (|UnicodeCharacter|) (UnicodeCharacter x) = x

[<Struct>] type Delimiter = private Delimiter of unichar
module Delimiter =
    let internal CreateUnchecked = Delimiter
    let ValueOf (Delimiter x) = x