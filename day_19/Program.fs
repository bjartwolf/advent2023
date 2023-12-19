module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): int list = 
        [1]

    type Cat = X | M | A | S 
    type Operation = Gt | Lt
    type Rule = Cat * Operation * int
    type NodeTerminator = NextNode of string | Accepted | Rejected
    type Node = Rule list * NodeTerminator 

    let tfN: Node =  [ (A, Lt, 2006); M, Gt, 2090], NextNode "rfg"


    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(1, input.Length) 

module Program = let [<EntryPoint>] main _ = 0