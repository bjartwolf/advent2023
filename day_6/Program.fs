open System


module Input =
    open Xunit 

    type race = { tt: int64;dist: int64}
    type attempt = { tp: int64;dist_raced: int64}
    let races_test = [ {tt=7L;dist=9L};
                       {tt=15L;dist=40L};
                       {tt=30L;dist=200L} ]

    let race_large_test = { tt= 71530L; dist = 940200L}
    let race_large_real = { tt= 48938466L; dist = 261119210191063L}

    let solve_smaller (a':int64) (b':int64) (c':int64) =
        let (a,b,c) = float a', float b', float c'
        (-b-Math.Sqrt(b*b-4.0*a*c))/(2.0*a)

    let solve_larger(a':int64) (b':int64) (c':int64) =
        let (a,b,c) = float a', float b', float c'
        (-b+Math.Sqrt(b*b-4.0*a*c))/(2.0*a)


    let find_all_wintimes_binary (r: race)  : int64 =
        let small = int64 (Math.Floor(solve_smaller 1 -r.tt r.dist)) + 1L
        let large = int64 (Math.Ceiling(solve_larger 1 -r.tt r.dist)) - 1L
        large - small + 1L

    [<Fact>]
    let binary_is_equal() =
        Assert.Equal(4L, find_all_wintimes_binary races_test[0] )
        Assert.Equal(8L, find_all_wintimes_binary races_test[1] )
        Assert.Equal(9L, find_all_wintimes_binary races_test[2] )
        Assert.Equal(71503L, find_all_wintimes_binary race_large_test)
        Assert.Equal(36749103L, find_all_wintimes_binary race_large_real )

 module Program = let [<EntryPoint>] main _ = 0