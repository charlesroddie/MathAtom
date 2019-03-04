namespace MathDisplay.Tests.Benchmarks

open BenchmarkDotNet.Attributes
open MathDisplay.DataTypes

[<CoreJob>]
type Result_ExceptionBenchmark() =
    [<Benchmark>]
    member __.Ok_Result() =
        let rec recurse = function
        | 10 -> Ok 1
        | n -> match recurse (n+1) with Ok i -> Ok (i + 1) | Error e -> Error e
        recurse 10 |> ignore
    [<Benchmark>]
    member __.Ok_Exception() =
        let rec recurse = function
        | 10 -> 1
        | n -> match recurse (n+1) with i -> i + 1
        try recurse 10 |> ignore with _ -> ()
    [<Benchmark>]
    member __.Error_Result() =
        let rec recurse = function
        | 10 -> Error "asd"
        | n -> match recurse (n+1) with Ok i -> Ok (i + 1) | Error e -> Error e
        recurse 10 |> ignore
    [<Benchmark>]
    member __.Error_Exception() =
        let rec recurse = function
        | 10 -> failwith "asd"
        | n -> match recurse (n+1) with i -> i + 1
        try recurse 10 |> ignore with _ -> ()

(*
          Method |          Mean |       Error |      StdDev |
---------------- |--------------:|------------:|------------:|
       Ok_Result |      4.412 ns |   0.0259 ns |   0.0229 ns |
    Ok_Exception |      2.275 ns |   0.0328 ns |   0.0307 ns |
    Error_Result |      3.758 ns |   0.0234 ns |   0.0219 ns |
 Error_Exception | 28,647.859 ns | 118.3354 ns | 110.6910 ns |
*)