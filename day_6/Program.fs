module Input =
    open Xunit 

    type race = { tt: int;dist: int}
    type attempt = { tp: int;dist_raced: int}
    let races_test = [ {tt=7;dist=9};
                       {tt=15;dist=40};
                       {tt=30;dist=200} ]

    let tts (races: race list) = races |> List.map (fun r -> r.tt)
    let ds (races: race list) = races |> List.map (fun r -> r.dist)

    let tp_possible race_total_time = [0 .. race_total_time]

    let dist time_total time_pressed = { tp = time_pressed;
                                         dist_raced = time_pressed * (time_total - time_pressed)  }

    let distances time_total times_pressed =
        times_pressed |> List.map (fun time -> dist time_total time)

    let filter_win_times (attempts: attempt list) (record:int) : attempt list =
        attempts |> List.filter (fun a -> a.dist_raced > record )

    let find_all_wintimes (r: race)  : int list =
        let times = tp_possible r.tt 
        let attempts = distances r.tt times
        let filtered_distances = filter_win_times attempts  r.dist 
        filtered_distances |> List.map (fun a -> a.tp)
        
    [<Fact>]
    let test_time_pressed_list() =
        Assert.Equal<int list>([2;3;4;5;], find_all_wintimes races_test[0] )
        Assert.Equal<int>(8, find_all_wintimes races_test[1] |> List.length) 
        Assert.Equal<int>(9, find_all_wintimes races_test[2] |> List.length) 
        ()

    [<Fact>]
    let test_time_pressed() =
        let race_dist = dist 7
        Assert.Equal(0, (race_dist 0).dist_raced)
        Assert.Equal(6,( race_dist 1).dist_raced)
        Assert.Equal(10,( race_dist 2).dist_raced)
        Assert.Equal(12,( race_dist 3).dist_raced)
        Assert.Equal(0,( race_dist 7).dist_raced)
 
    [<Fact>]
    let test2 () =
        Assert.Equal(3,races_test.Length)
        Assert.Equal<int list>([7;15;30], tts races_test)
        Assert.Equal<int list>([9;40;200], ds races_test)
        Assert.Equal<int list>([0;1;2;3;4;5;6;7], tp_possible 7)

module Program = let [<EntryPoint>] main _ = 0