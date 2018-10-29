namespace VersionsOfDotNet

open System

type Version =
    { Major: int
      Minor: int
      Patch: int
      Preview: string option }

    override v.ToString() =
        let s = sprintf "%i.%i.%i" v.Major v.Minor v.Patch
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
        let split (sep: char) (s: string) = s.Split(sep)
        let trim (s: string) = s.Trim()
        let toLowerInvariant (s: string) = s.ToLowerInvariant()

    let parse (s: string) =
        let parts = s |> String.trim |> String.toLowerInvariant |> String.split '.'
        if parts.Length >= 3 then
            let major = Int.parse parts.[0]
            let minor = Int.parse parts.[1]
            let patch, preview =
                match parts.[2..] |> String.concat "." |> String.split '-' |> Array.toList with 
                | patch::[] -> Int.parse patch, None
                | patch::preview -> Int.parse patch, Some (preview |> String.concat "-")
                | _ -> None, None
            match major, minor, patch with
            | Some maj, Some min, Some p -> 
                Some { Major = maj; Minor = min; Patch = p; Preview = preview }
            | _ -> None
        else None

    let (|Version|_|) input = parse input
