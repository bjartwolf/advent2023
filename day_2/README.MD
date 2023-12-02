Perhaps I can use this later to speed up regex stuff
```fsharp
module AdventParser =  
	let colors = [|"red"; "green"; "blue"|] 
	let pattern = (colors |> Array.map (fun color -> sprintf "(?<%s>\d+)\s* %s" color color))
	let combinedPattern = String.Join('|', pattern)
	let regex = new Regex(combinedPattern)
	let parseDraw (draw: string): Draw = 
		let matches = regex.Matches(draw)

		(*
		Used for debugging
		let groupNames = regex.GetGroupNames() |> Array.where (fun x -> x <> "" && x <> "0")
		/*)

		let res = [for regexMatch in matches do
					//for groupName in groupNames do - can be used dynamically
					for groupName in colors do
						let group = regexMatch.Groups.[groupName]
						if group.Success && group.Value <> "" then
							yield groupName, group.Value]

		let redMatch = res |> List.tryFind(fun (name,value) -> name = "red")
		let red = match redMatch with
						| Some (name,value) -> int value 
						| None -> 0

		let greenMatch = res |> List.tryFind(fun (name,value) -> name = "green")
		let green = match greenMatch with
						| Some (name,value) -> int value 
						| None -> 0

		let blueMatch= res |> List.tryFind(fun (name,value) -> name = "blue")
		let blue = match blueMatch with
						| Some (name,value) -> int value 
						| None -> 0

		{ Red = red; Green = green; Blue = blue}

```