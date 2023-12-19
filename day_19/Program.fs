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
    type Parts = Part list

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

    let parseNodes(fileName: string): NodeMap*Parts =
        let allTxt = File.ReadAllText fileName
        let foo = allTxt.Split(Environment.NewLine+Environment.NewLine)
        let firstPart = foo[0]
        let m: (string*Node) list = firstPart.Split(Environment.NewLine) 
                                          |> Array.toList
                                          |> List.map parseFirstPartLine
        let secondPart = foo[1].Split(Environment.NewLine) |> Array.toList |> List.map parsePart
        Map.ofList m, secondPart


    [<Fact>]
    let parseTest() = 
        let (map,parts) = parseNodes "testinput.txt" 
        Assert.Equal(11, map.Count) 
        Assert.Equal(5, parts.Length) 

    [<Fact>]
    let parseOther () = 
        let (map,parts) = parseNodes "input.txt" 
        Assert.Equal(572, map.Count) 
        Assert.Equal(773-573, parts.Length) 

    let rec evalNode (p: Part) (n: Node): NodeTerminator = 
        match n with 
            | [] -> failwith "cant happen, should have a happy rule" 
            | (cat, op, num, term ):: t -> 
                  match op with
                    | Gt -> match cat with 
                                    | X -> if p.x > num then term else evalNode p t 
                                    | M -> if p.m > num then term else evalNode p t 
                                    | A -> if p.a > num then term else evalNode p t 
                                    | S -> if p.s > num then term else evalNode p t 
                    | Lt -> match cat with
                                    | X -> if p.x < num then term else evalNode p t 
                                    | M -> if p.m < num then term else evalNode p t 
                                    | A -> if p.a < num then term else evalNode p t 
                                    | S -> if p.s < num then term else evalNode p t 

    [<Fact>]
    let testNodeEval() = 
        let (map,parts) = parseNodes "testinput.txt" 
        Assert.Equal<NodeTerminator>(N "qqz", evalNode parts[0] (Map.find "in" map))
        Assert.Equal<NodeTerminator>(N "px", evalNode parts[1] (Map.find "in" map))
        Assert.Equal<NodeTerminator>(N "qqz", evalNode parts[2] (Map.find "in" map))
        Assert.Equal<NodeTerminator>(N "px", evalNode parts[3] (Map.find "in" map))
        Assert.Equal<NodeTerminator>(N "px", evalNode parts[4] (Map.find "in" map))
        Assert.Equal<NodeTerminator>(N "qs", evalNode parts[0] (Map.find "qqz" map))
        Assert.Equal<NodeTerminator>(Acc, evalNode parts[0] (Map.find "lnx" map))
        Assert.Equal<NodeTerminator>(Rej, evalNode parts[1] (Map.find "gd" map))

 
    //let isAccepted (m:NodeMap) (p:Part): bool =
        
        

    //    // starter med å slå opp in
    //    let firstNode = Map.find "in" m
    //    true
         

    let partSum (p:Part) : int =
        p.x + p.a + p.m + p.s

module Program = let [<EntryPoint>] main _ = 0