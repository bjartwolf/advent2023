open System


module Input =
    open Xunit 
    type race = { tt: int64;dist: int64}
    let race_large_real = { tt= 48938466L; dist = 261119210191063L}

    let solve (b':int64) (c':int64) (sum: float->float->float)=
        let (b,c) = float b', float c'
        (sum -b (Math.Sqrt(b*b-4.0*c))/(2.0))

    let find_all_wintimes_binary (r: race)  : int64 =
        int64 (Math.Ceiling(solve -r.tt r.dist (+)) - (Math.Floor(solve -r.tt r.dist (-)))) - 1L

    [<Fact>]
    let binary_is_equal() =
        Assert.Equal(36749103L, find_all_wintimes_binary race_large_real )

 module Program = let [<EntryPoint>] main _ = 0