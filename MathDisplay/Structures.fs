namespace MathDisplay.DataTypes

type Alignment = Left = 0 | Center = 1 | Right = 2
type SpaceType = Points = 0 | MathUnits = 1
[<Struct>] type Space = { Length:float; SpaceType:SpaceType }
type LineStyle = Display = 0 | Text = 1 | Script = 2 | ScriptScript = 3