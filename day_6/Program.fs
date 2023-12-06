module Program =
    open System
    type race = { tt: int64;dist: int64}

    let solve b' c' sum =
        let (b,c) = float b', float c'
        (sum -b (Math.Sqrt(b*b-4.0*c))/(2.0))

    let find r = 
        int64 (Math.Ceiling(solve -r.tt r.dist (+)) - (Math.Floor(solve -r.tt r.dist (-)))) - 1L

    let [<EntryPoint>] main _ =
        printfn "Answer should be %A" 36749103 
        printfn "Answer is        %A" (find { tt= 48938466L; dist = 261119210191063L} )
        0