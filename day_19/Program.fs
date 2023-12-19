module Input =
    open System
    open System.IO
    open Xunit 

    type Cat = X | M | A | S 
    type Operation = Gt | Lt // use A > 0 
    type NodeTerminator = N of string | Acc| Rej
    type Rule = Cat * Operation * int * NodeTerminator
    type Node = Rule list 
    type NodeMap = Map<string, Node>

    let tn1: Node =  [ A, Lt, 2006, N "qkq"; M, Gt, 2090, Acc; X, Gt, 0, N "rfg"]

    type Part = { x: int; m: int; a: int; s: int}
    let parsePart (partLine:string) = 
        let rawLine = partLine.Replace("{","").Replace("}","")
        let rawParts = rawLine.Split(",") 
        let x = int (rawParts[0].Split("=")[1])
        let m = int (rawParts[1].Split("=")[1])
        let a = int (rawParts[2].Split("=")[1])
        let s = int (rawParts[3].Split("=")[1])
        { x = int x; m = int m; a = int a; s = int s}

    [<Fact>]
    let testParsePart() = 
        Assert.Equal<Part>({ x = 89; m = 520; a = 174; s = 541}, parsePart "{x=89,m=520,a=174,s=541}")
 
    let parseFirstPartLine (line: string): (string*Node) = 
        let foo = line.Split("{") 
        let id = foo[0]
        let rawRules = foo[1].Replace("}","").Split(",")
        let rules = [
            for rawRule in rawRules do
                let gt = rawRule.Contains(">")
                let lt = rawRule.Contains("<")
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
                else if lt then
                    let parts = rawRule.Split("<")
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
                        yield bar,Lt,num,stop
                else
                    let stop = match rawRule with
                        | "A" -> Acc
                        | "R" -> Rej
                        | str -> N str
                    yield X,Gt,0,stop

        ]
        id, rules 
    
    [<Fact>]
    let testParseLine() = 
        Assert.Equal(("ax", [A,Gt,2006, N "qkq"; X, Gt, 0, Acc] ), parseFirstPartLine "ax{a>2006:qkq,A}") 
        Assert.Equal(("px", tn1), parseFirstPartLine "px{a<2006:qkq,m>2090:A,rfg}") 

    let parseNodes(fileName: string): NodeMap =
        let allTxt = File.ReadAllText fileName
        let firstPart = allTxt.Split(Environment.NewLine+Environment.NewLine).[0]
        let m: (string*Node) list = firstPart.Split(Environment.NewLine) 
                                          |> Array.toList
                                          |> List.map parseFirstPartLine
        Map.ofList m


    [<Fact>]
    let parseTest() = 
        let map = parseNodes "testinput.txt" 
        Assert.Equal(11, map.Count) 

    [<Fact>]
    let parseOther () = 
        let map = parseNodes "input.txt" 
        Assert.Equal(572, map.Count) 


module Program = let [<EntryPoint>] main _ = 0