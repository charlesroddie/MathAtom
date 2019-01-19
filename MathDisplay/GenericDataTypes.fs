namespace MathDisplay

type internal 'T memory = System.Memory<'T>
type internal chars = char memory

[<Struct>] type XAlignment = Left | Center | Right
[<Struct>] type YAlignment = Top | Center | Bottom
[<Struct>] type Color = { r:byte; g:byte; b:byte; a:byte }

[<Struct>]
type UnicodeCharacter = private UnicodeCharacter of int
module UnicodeCharacter =
    let Create (x:chars) =
        match x.Length with
        | 1 when x.Span.[0] |> System.Char.IsSurrogate |> not ->
            x.Span.[0] |> int |> UnicodeCharacter |> ValueSome
        | 2 when x.Span.[0] |> System.Char.IsHighSurrogate && x.Span.[1] |> System.Char.IsLowSurrogate ->
            System.Char.ConvertToUtf32(x.Span.[0], x.Span.[1]) |> UnicodeCharacter |> ValueSome
        | _ -> ValueNone
    let (|UnicodeCharacter|) (UnicodeCharacter x) = x

type internal unichar = UnicodeCharacter