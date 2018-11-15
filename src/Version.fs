namespace VersionsOfDotNet

open System

[<CustomComparison; StructuralEquality>]
type Version =
    { Numbers: int list
      Preview: string option }

    override v.ToString() =
        let s = v.Numbers |> List.map string |> List.reduce (sprintf "%s.%s")
        match v.Preview with
        | Some preview -> sprintf "%s-%s" s preview
        | None -> s

    member this.CompareTo(other) =
        match compare this.Numbers other.Numbers with
        | 0 -> 
            match this.Preview, other.Preview with
            | None, None -> 0
            | Some p1, Some p2 -> compare p1 p2
            | Some _, None -> -1
            | None, Some _ -> 1
        | c -> c

    interface IComparable with
        member this.CompareTo(other: obj) =
            match other with
            | :? Version as v -> this.CompareTo(v)
            | _ -> invalidArg "other" "cannot compare values of different types"

module Version =
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
