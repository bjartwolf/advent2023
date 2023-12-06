module Input =
    open Xunit 

    type race = { tt: int64;dist: int64}
    type attempt = { tp: int64;dist_raced: int64}
    let races_test = [ {tt=7L;dist=9L};
                       {tt=15L;dist=40L};
                       {tt=30L;dist=200L} ]

    let race_large_test = { tt= 71530L; dist = 940200L}
    let race_large_real = { tt= 48938466L; dist = 261119210191063L}

    let dist time_total time_pressed = { tp = time_pressed;
                                         dist_raced = time_pressed * (time_total - time_pressed)  }

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

    let find_all_wintimes_binary (r: race)  : int64 =
        let find_max_wintime_binary = findMax r 
        let find_min_wintime_binary = findMin r 
        find_max_wintime_binary - find_min_wintime_binary + 1L

    [<Fact>]
    let binary_is_equal() =
        Assert.Equal(4L, find_all_wintimes_binary races_test[0] )
        Assert.Equal(8L, find_all_wintimes_binary races_test[1] )
        Assert.Equal(9L, find_all_wintimes_binary races_test[2] )
        Assert.Equal(71503L, find_all_wintimes_binary race_large_test)
        Assert.Equal(36749103L, find_all_wintimes_binary race_large_real )

 module Program = let [<EntryPoint>] main _ = 0