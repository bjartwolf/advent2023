module Input =
    open System
    open System.IO
    open Xunit 

    type Cat = X | M | A | S 
    type Operation = Gt | Lt // use A > 0 
    type NodeTerminator = N of string | Acc| Rej
    type Rule = Cat * Operation * int * NodeTerminator
    type Node = Rule list 
    type Map = Map<string, Node>

    let tn1: Node =  [ A, Lt, 2006, N "qkq"; M, Gt, 2090, Acc; A, Gt, 0, N "rfg"]

    let parseLine (line: string): (string*Node) = 
        let foo = line.Split("{") 
        let id = foo[0]
        let rawRules = foo[1].Replace("}","").Split(",")
        let rules = [
            for rawRule in rawRules do
                let gt = rawRule.Contains(">")
                if gt then
                    let parts = rawRule.Split(">")
                    if (parts[1].Contains(":")) then
                        let bar = match parts[0] with 
                                                    | "x" -> X
                                                    | "m" -> M 
                                                    | "a" -> A
                                                    | "s" -> S
                        let baz = parts[1].Split(":")
                        let num = int baz[0] 
                        let stop = match baz[1] with
                            | "A" -> Acc
                            | "R" -> Rej
                            | str -> N str
                        yield bar,Gt,num,stop
        ]
        id, rules 
    
    [<Fact>]
    let testParseLine() = 
        Assert.Equal(("ax", [A,Gt,2006, N "qkq"] ), parseLine "ax{a>2006:qkq}") 
        Assert.Equal(("px", tn1), parseLine "px{a<2006:qkq,m>2090:A,rfg}") 


    let readInit (filePath: string): int list = 
        [1]



    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(1, input.Length) 

module Program = let [<EntryPoint>] main _ = 0