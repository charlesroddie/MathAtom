namespace MathDisplay.DataTypes

type Alignment = Left = 0 | Center = 1 | Right = 2
type SpaceType = Points = 0 | MathUnits = 1
[<Struct>] type Space = { Length:float; SpaceType:SpaceType }