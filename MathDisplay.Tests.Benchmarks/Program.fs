module Program =
    let [<EntryPoint>] main _ =
        printfn "Benchmark started."
        printfn "Press Enter to run..."
        System.Console.ReadLine() |> ignore
        BenchmarkDotNet.Running.BenchmarkRunner.Run<MathDisplay.Tests.Benchmarks.MapBenchmark>() |> ignore
        printfn ""
        printfn "Benchmark ended."
        printfn "Press any key to continue..."
        System.Console.ReadLine() |> ignore
        0