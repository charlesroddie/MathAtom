namespace MathDisplay.Tests.Benchmarks

open BenchmarkDotNet.Attributes
open MathDisplay.DataTypes

[<CoreJob>]
type AliasMapBenchmark() =
    [<DefaultValue; Params(100, 10000)>]
    val mutable Count : int
    [<DefaultValue; Params(10, 100)>]
    val mutable Length : int
    [<DefaultValue>]
    val mutable Data : (string * string list * string) list

    [<GlobalSetup>]
    member this.SetUp() =
        let rnd = System.Random 42
        let rndValue() =
            seq { for _ in 1..this.Length do yield rnd.Next(int 'a', int 'z') |> char } |> System.String.Concat
        this.Data <- List.init this.Count (fun _ -> rndValue(), [], rndValue())
        
    [<Benchmark>]
    member this.Add_AliasMap() =
        AliasMap.ofList this.Data

    [<Benchmark>]
    member this.Add_BiDictionary() =
        let dict = BiDictionary()
        for key, keys, value in this.Data do
            dict.Add (key::keys, value)
 
(*
// * Summary *

BenchmarkDotNet=v0.11.3, OS=Windows 10.0.17134.472 (1803/April2018Update/Redstone4)
Intel Core i5-4570 CPU 3.20GHz (Haswell), 1 CPU, 4 logical and 4 physical cores
.NET Core SDK=2.1.503
  [Host] : .NET Core 2.1.7 (CoreCLR 4.6.27129.04, CoreFX 4.6.27129.04), 64bit RyuJIT DEBUG
  Core   : .NET Core 2.1.7 (CoreCLR 4.6.27129.04, CoreFX 4.6.27129.04), 64bit RyuJIT

Job=Core  Runtime=Core

       Method | Count | Length |         Mean |       Error |        StdDev |
------------- |------ |------- |-------------:|------------:|--------------:|
     AliasMap |   100 |     10 |     99.87 us |   0.8029 us |     0.7510 us |
 BiDictionary |   100 |     10 |     17.17 us |   0.1828 us |     0.1710 us |
     AliasMap |   100 |    100 |     99.99 us |   0.7460 us |     0.6978 us |
 BiDictionary |   100 |    100 |     41.33 us |   0.4383 us |     0.4100 us |
     AliasMap | 10000 |     10 | 36,264.27 us | 714.2295 us | 1,250.9154 us |
 BiDictionary | 10000 |     10 |  2,936.10 us |  23.2561 us |    21.7538 us |
     AliasMap | 10000 |    100 | 37,032.70 us | 729.3024 us | 1,069.0019 us |
 BiDictionary | 10000 |    100 |  5,387.08 us |  97.3108 us |    91.0246 us |

// * Warnings *
MultimodalDistribution
  AliasMapBenchmark.AliasMap: Core -> It seems that the distribution is bimodal (mValue = 3.54)

// * Legends *
  Count  : Value of the 'Count' parameter
  Length : Value of the 'Length' parameter
  Mean   : Arithmetic mean of all measurements
  Error  : Half of 99.9% confidence interval
  StdDev : Standard deviation of all measurements
  1 us   : 1 Microsecond (0.000001 sec)

// ***** BenchmarkRunner: End *****
Run time: 00:02:40 (160.51 sec), executed benchmarks: 8
*)