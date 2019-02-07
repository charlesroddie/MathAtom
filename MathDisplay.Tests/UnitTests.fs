namespace global

open NUnit.Framework
open FsUnit
open FsUnitTyped
open MathDisplay.MathAtom

exception InfoException of string with override this.Message = this.Data0

[<TestClass>]
type TestClass () =

    [<SetUp>]
    member __.Setup () = ()

    [<Test>]
    member __.``Gather output`` () =
        let fff = MathDisplay.DataTypes.List.partitionWhile ((=) 'a') ['a'; 'a'; 'a'; 'a'; 'b'; 'c']
        let x =
            @"123"
            |> LaTeX.toAtom LaTeX.Options.Default
        (match x with Ok r -> r | Error e -> InfoException e |> raise).ToString() |>
        Assert.Pass