namespace VersionsOfDotNet

open System

type Version =
    { Numbers: int list
      Preview: string option }

    override v.ToString() =
        let s = v.Numbers |> List.map string |> List.reduce (sprintf "%s.%s")
        match v.Preview with
        | Some preview -> sprintf "%s-%s" s preview
        | None -> s

module Version =
    module private Int =
        let parse i = 
            match Int32.TryParse i with
            | (true, r) -> Some r
            | _ -> None

    module private String =
        let split (sep: char) (s: string) = s.Split(sep) |> Array.toList
        let trim (s: string) = s.Trim()
        let toLowerInvariant (s: string) = s.ToLowerInvariant()

    module private Option =
        let mapList optionList = 
            if optionList |> List.forall Option.isSome
            then optionList |> List.map Option.get |> Some
            else None

    let parse (s: string) =
        match s |> String.trim |> String.toLowerInvariant |> String.split '-' with
        | [ ] | [ "" ] -> None
        | version::preview ->
            version 
            |> String.split '.'
            |> List.filter (not << String.IsNullOrEmpty)
            |> List.map Int.parse 
            |> Option.mapList
            |> Option.map (fun numbers -> 
                                { Numbers = numbers
                                  Preview = if preview.IsEmpty then None 
                                            else Some (preview |> String.concat "-") })

    let displayedAs display (version: Version) =
        string version = display

    let mightMatchInChannel channelVersion version =
        List.tryHead channelVersion.Numbers = List.tryHead version.Numbers

    let matches versionStart version =
        versionStart.Numbers.Length <= version.Numbers.Length &&
        versionStart.Numbers = List.take versionStart.Numbers.Length version.Numbers &&
        match versionStart.Preview with 
        | Some p1 -> 
            match version.Preview with
            | Some p2 -> p2.StartsWith(p1)
            | None -> false
        | None -> true

    let (|Version|_|) input = parse input
