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
        let x =
            "^_{\1 23''}^_"
            |> LaTeX.toAtom
        (match x with Ok r -> r | Error e -> InfoException e |> raise).ToString() |>
        Assert.Inconclusive