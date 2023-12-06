module Program =
    open System

    let solve b' c' sum =
        let (b,c) = float b', float c'
        (sum -b (Math.Sqrt(b*b-4.0*c))/(2.0))

    let find t d = int64 (Math.Ceiling(solve -t d (+)) - (Math.Floor(solve -t d (-)))) - 1L

    let [<EntryPoint>] main _ =
        printfn "Answer should be %A" 36749103 
        printfn "Answer is        %A" (find 48938466L 261119210191063L )
        0