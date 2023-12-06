module Input =
    open Xunit 

    type race = { tt: int;dist: int}
    type attempt = { tp: int;dist_raced: int}
    let races_test = [ {tt=7;dist=9};
                       {tt=15;dist=40};
                       {tt=30;dist=200} ]

    let race_large_test = { tt= 71530; dist = 940200}
    //let race_large_real = { tt= 48938466; dist = 261119210191063}

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

    // must be binary search to find the elements 
    // find binary search, make work for long, make equal for testdata
    // should calculate the same results

    // search min is 0, search max is max, searching for smallest number that will win
    let findMin (r: race): int =
       let min_guess = 0
       let max_guess = r.dist
       
       let rec innerGuess (guess:int): int = 
           let guess_dist = (dist r.tt guess).dist_raced 
           let prev_attempt_dist = (dist r.tt (guess - 1)).dist_raced 
           if (guess_dist > r.dist && not (prev_attempt_dist> r.dist)) then
               guess
           else if (guess_dist> r.dist) then
               let newGuess = guess - 1
               innerGuess newGuess
           else 
               let newGuess = guess + 1
               innerGuess newGuess
       innerGuess (max_guess / 2 - 1)

    [<Fact>]
    let find_smallest_binary() =
        Assert.Equal(2, findMin races_test[0])
        Assert.Equal(4, findMin races_test[1])

        (*
    let find_all_wintimes_binary (r: race)  : int =
        let wins = find_all_wintimes r 
        let find_max_wintime = wins |> List.max 
        let find_max_wintime_binary = find_max_wintime 
        if find_max_wintime <> find_max_wintime_binary then failwith (sprintf "max is wrong real %A guess %A" find_max_wintime find_max_wintime_binary)
        let find_min_wintime = wins |> List.min

        let find_min_wintime_binary = findMin r 
        if find_min_wintime <> find_min_wintime_binary then failwith (sprintf "min is wrong real %A guess %A" find_min_wintime find_max_wintime_binary)
        find_max_wintime - find_min_wintime + 1
*)
    let find_all_wintimes_count (r:race): int = 
        find_all_wintimes r |> List.length 

    [<Fact>]
    let binary_is_equal() =
        Assert.Equal(4, find_all_wintimes_binary races_test[0] )
        Assert.Equal(8, find_all_wintimes_binary races_test[1] )
        Assert.Equal(9, find_all_wintimes_binary races_test[2] )

    [<Fact>]
    let test_time_pressed_list() =
        Assert.Equal(4, find_all_wintimes_count races_test[0] )
        Assert.Equal(8, find_all_wintimes_count races_test[1] )
        Assert.Equal(9, find_all_wintimes_count races_test[2] )


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