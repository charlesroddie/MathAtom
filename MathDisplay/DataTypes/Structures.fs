namespace MathDisplay.DataTypes

[<Struct>] type Alignment = Left | Center | Right
[<Struct>] type SpaceType = Points | MathUnits
[<Struct>] type Space = { Length:float; SpaceType:SpaceType }
[<Struct>] type LineStyle = Display | Text | Script | ScriptScript
type Bracket = // could make this an enum with unicode numbers
    | Parenthesis
    | Square
    | Curly
    | Line
    | DoubleLine
    | Angle
    | Floor
    | Ceil
    | UCorner