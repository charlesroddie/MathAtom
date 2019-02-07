namespace MathDisplay.Tests.Benchmarks

open System.Collections.Generic
open BenchmarkDotNet.Attributes

[<CoreJob>]
type MapBenchmark() =
    [<DefaultValue; Params(100, 10000)>]
    val mutable Count : int
    [<DefaultValue; Params(10, 100)>]
    val mutable Length : int
    [<DefaultValue>]
    val mutable Data : (string * string)[]
    [<DefaultValue>]
    val mutable Dict : Dictionary<string, string>
    [<DefaultValue>]
    val mutable Map : Map<string, string>
    
    [<GlobalSetup>]
    member this.SetUp() =
        let rnd = System.Random 42
        let rndValue() =
            seq { for _ in 1..this.Length do yield rnd.Next(int 'a', int 'z') |> char } |> System.String.Concat
        this.Data <- Array.init this.Count (fun _ -> rndValue(), rndValue())
        this.Dict <- this.Add_Dictionary()
        this.Map <- this.Add_Map()
    
    [<Benchmark>]
    member this.Add_Map() =
        Map.ofArray this.Data

    [<Benchmark>]
    member this.Add_Dictionary() =
        let dict = Dictionary<string, string>()
        for key, value in this.Data do
            dict.Add (key, value)
        dict
    
    [<Benchmark>]
    member this.Lookup_Map() =
        Array.map (fun (key, _) -> Map.tryFind key this.Map) this.Data

    [<Benchmark>]
    member this.Lookup_Dictionary() =
        Array.map (fun (key, _) -> this.Dict.TryGetValue key) this.Data

(*
// * Summary *

BenchmarkDotNet=v0.11.3, OS=Windows 10.0.17134.472 (1803/April2018Update/Redstone4)
Intel Core i5-4570 CPU 3.20GHz (Haswell), 1 CPU, 4 logical and 4 physical cores
.NET Core SDK=2.1.503
  [Host] : .NET Core 2.1.7 (CoreCLR 4.6.27129.04, CoreFX 4.6.27129.04), 64bit RyuJIT DEBUG
  Core   : .NET Core 2.1.7 (CoreCLR 4.6.27129.04, CoreFX 4.6.27129.04), 64bit RyuJIT


            Method| Count | Length |          Mean |       Error |      StdDev |
------------------|------ |------- |--------------:|------------:|------------:|
           Add_Map|   100 |     10 |     43.527 us |   0.5113 us |   0.4782 us |
    Add_Dictionary|   100 |     10 |      5.823 us |   0.0509 us |   0.0476 us |
        Lookup_Map|   100 |     10 |     10.964 us |   0.1041 us |   0.0974 us |
 Lookup_Dictionary|   100 |     10 |      3.960 us |   0.0558 us |   0.0522 us |
           Add_Map|   100 |    100 |     43.808 us |   0.4180 us |   0.3910 us |
    Add_Dictionary|   100 |    100 |     11.997 us |   0.0917 us |   0.0813 us |
        Lookup_Map|   100 |    100 |     10.989 us |   0.1063 us |   0.0994 us |
 Lookup_Dictionary|   100 |    100 |     10.183 us |   0.0940 us |   0.0879 us |
           Add_Map| 10000 |     10 | 15,504.510 us | 300.0044 us | 357.1339 us |
    Add_Dictionary| 10000 |     10 |    858.189 us |  16.8967 us |  27.7618 us |
        Lookup_Map| 10000 |     10 |  3,205.454 us |  63.8344 us |  75.9904 us |
 Lookup_Dictionary| 10000 |     10 |    560.489 us |   5.4141 us |   5.0643 us |
           Add_Map| 10000 |    100 | 16,470.972 us | 325.1749 us | 671.5425 us |
    Add_Dictionary| 10000 |    100 |  1,842.098 us |  35.2396 us |  43.2774 us |
        Lookup_Map| 10000 |    100 |  3,969.983 us |  87.7269 us | 258.6646 us |
 Lookup_Dictionary| 10000 |    100 |  1,529.212 us |  29.9664 us |  57.0141 us |
 
// * Warnings *
MultimodalDistribution
  MapBenchmark.Lookup_Map: Core -> It seems that the distribution can have several modes (mValue = 2.92)

// * Hints *
Outliers
  MapBenchmark.Add_Dictionary: Core -> 1 outlier  was  removed

// * Legends *
  Count  : Value of the 'Count' parameter
  Length : Value of the 'Length' parameter
  Mean   : Arithmetic mean of all measurements
  Error  : Half of 99.9% confidence interval
  StdDev : Standard deviation of all measurements
  1 us   : 1 Microsecond (0.000001 sec)

// ***** BenchmarkRunner: End *****
Run time: 00:07:02 (422 sec), executed benchmarks: 16

*)