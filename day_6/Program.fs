module Input =
    open Xunit 

    type race = { tt: int64;dist: int64}
    type attempt = { tp: int64;dist_raced: int64}
    let races_test = [ {tt=7L;dist=9L};
                       {tt=15L;dist=40L};
                       {tt=30L;dist=200L} ]

    let race_large_test = { tt= 71530L; dist = 940200L}
    let race_large_real = { tt= 48938466L; dist = 261119210191063L}

    let tts (races: race list) = races |> List.map (fun r -> r.tt)
    let ds (races: race list) = races |> List.map (fun r -> r.dist)

//    let tp_possible (race_total_time: int64) = [0 .. race_total_time]

    let dist time_total time_pressed = { tp = time_pressed;
                                         dist_raced = time_pressed * (time_total - time_pressed)  }

    let distances time_total times_pressed =
        times_pressed |> List.map (fun time -> dist time_total time)

    let filter_win_times (attempts: attempt list) (record:int64) : attempt list =
        attempts |> List.filter (fun a -> a.dist_raced > record )

        (*
    let find_all_wintimes (r: race)  : int64 list =
        let times = tp_possible r.tt 
        let attempts = distances r.tt times
        let filtered_distances = filter_win_times attempts  r.dist 
        filtered_distances |> List.map (fun a -> a.tp)
*)
    // must be binary search to find the elements 
    // find binary search, make work for long, make equal for testdata
    // should calculate the same results

    // search min is 0, search max is max, searching for smallest number that will win
    let findMin (r: race): int64 =
       let max_guess = r.tt
       
       let rec innerGuess (guess:int64): int64 = 
           let guess_dist = (dist r.tt guess).dist_raced 
           let prev_attempt_dist = (dist r.tt (guess - 1L)).dist_raced 
           if (guess_dist > r.dist && not (prev_attempt_dist> r.dist)) then
               guess
           else if (guess_dist> r.dist) then
               let newGuess = guess - 1L
               innerGuess newGuess
           else 
               let newGuess = guess + 1L
               innerGuess newGuess
       innerGuess (max_guess / 2L - 1L)

    let findMax (r: race): int64 =
       let max_guess = r.tt
       
       let rec innerGuess (guess:int64): int64 = 
           let guess_dist = (dist r.tt guess).dist_raced 
           let next_attempt_dist = (dist r.tt (guess + 1L)).dist_raced 
           if (guess_dist > r.dist && not (next_attempt_dist > r.dist)) then
               guess
           else if (guess_dist > r.dist) then
               let newGuess = guess + 1L
               innerGuess newGuess
           else 
               let newGuess = guess - 1L
               innerGuess newGuess
       innerGuess (max_guess / 2L - 1L)

    [<Fact>]
    let find_largest_binary() =
         Assert.Equal(11L, findMax races_test[1])
         Assert.Equal(5L, findMax races_test[0])
         Assert.Equal(19L, findMax races_test[2])


    [<Fact>]
    let find_smallest_binary() =
        Assert.Equal(4L, findMin races_test[1])
        Assert.Equal(2L, findMin races_test[0])
        Assert.Equal(11L, findMin races_test[2])

    let find_all_wintimes_binary (r: race)  : int64 =
 //       let wins = find_all_wintimes r 
   //     let find_max_wintime = wins |> List.max 
        let find_max_wintime_binary = findMax r 
     //   if find_max_wintime <> find_max_wintime_binary then failwith (sprintf "max is wrong real %A guess %A" find_max_wintime find_max_wintime_binary)
    //    let find_min_wintime = wins |> List.min

        let find_min_wintime_binary = findMin r 
  //      if find_min_wintime <> find_min_wintime_binary then failwith (sprintf "min is wrong real %A guess %A" find_min_wintime find_max_wintime_binary)
        find_max_wintime_binary - find_min_wintime_binary + 1L

    [<Fact>]
    let binary_is_equal() =
        Assert.Equal(4L, find_all_wintimes_binary races_test[0] )
        Assert.Equal(8L, find_all_wintimes_binary races_test[1] )
        Assert.Equal(9L, find_all_wintimes_binary races_test[2] )
        Assert.Equal(36749103L, find_all_wintimes_binary race_large_real )

//    let find_all_wintimes_count (r:race): int = 
//        find_all_wintimes r |> List.length 

(*
    [<Fact>]
    let test_time_pressed_list() =
        Assert.Equal(4L, find_all_wintimes_count races_test[0] )
        Assert.Equal(8L, find_all_wintimes_count races_test[1] )
        Assert.Equal(9L, find_all_wintimes_count races_test[2] )
*)

    //[<Fact>]
    //let test_time_pressed() =
    //    let race_dist = dist 7
    //    Assert.Equal(0, (race_dist 0).dist_raced)
    //    Assert.Equal(6,( race_dist 1).dist_raced)
    //    Assert.Equal(10,( race_dist 2).dist_raced)
    //    Assert.Equal(12,( race_dist 3).dist_raced)
    //    Assert.Equal(0,( race_dist 7).dist_raced)
 
    //[<Fact>]
    //let test2 () =
    //    Assert.Equal(3,races_test.Length)
    //    Assert.Equal<int list>([7;15;30], tts races_test)
    //    Assert.Equal<int list>([9;40;200], ds races_test)
    //    Assert.Equal<int list>([0;1;2;3;4;5;6;7], tp_possible 7)

 module Program = let [<EntryPoint>] main _ = 0