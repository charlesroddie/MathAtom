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
        recurse 1 |> ignore
    [<Benchmark>]
    member __.Ok_Exception() =
        let rec recurse = function
        | 10 -> 1
        | n -> match recurse (n+1) with i -> i + 1
        try recurse 1 |> ignore with _ -> ()
    [<Benchmark>]
    member __.Error_Result() =
        let rec recurse = function
        | 10 -> Error "asd"
        | n -> match recurse (n+1) with Ok i -> Ok (i + 1) | Error e -> Error e
        recurse 1 |> ignore
    [<Benchmark>]
    member __.Error_Exception() =
        let rec recurse = function
        | 10 -> failwith "asd"
        | n -> match recurse (n+1) with i -> i + 1
        try recurse 1 |> ignore with _ -> ()

(*
//recurse 10
          Method |          Mean |       Error |      StdDev |
---------------- |--------------:|------------:|------------:|
       Ok_Result |      4.412 ns |   0.0259 ns |   0.0229 ns |
    Ok_Exception |      2.275 ns |   0.0328 ns |   0.0307 ns |
    Error_Result |      3.758 ns |   0.0234 ns |   0.0219 ns |
 Error_Exception | 28,647.859 ns | 118.3354 ns | 110.6910 ns |

//recurse 1
          Method |         Mean |       Error |      StdDev |
---------------- |-------------:|------------:|------------:|
       Ok_Result |     46.72 ns |   0.2317 ns |   0.2167 ns |
    Ok_Exception |     17.36 ns |   0.1735 ns |   0.1623 ns |
    Error_Result |     37.50 ns |   0.1186 ns |   0.1110 ns |
 Error_Exception | 39,582.16 ns | 204.3940 ns | 181.1899 ns |
*)