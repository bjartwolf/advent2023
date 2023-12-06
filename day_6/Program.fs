module Input =
    open System
    open System.IO
    open Xunit 

    type race = { tt: int;dist: int}
    let races_test = [ {tt=7;dist=9};
                       {tt=15;dist=40};
                       {tt=30;dist=200} ]

    let tts (races: race list) = races |> List.map (fun r -> r.tt)
    let ds (races: race list) = races |> List.map (fun r -> r.dist)

    [<Fact>]
    let test2 () =
        Assert.Equal(3,races_test.Length)
        Assert.Equal<int list>([7;15;30], tts races_test)
        Assert.Equal<int list>([9;40;200], ds races_test)

module Program = let [<EntryPoint>] main _ = 0