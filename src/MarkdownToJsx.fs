module Import.MarkdownToJsx

open Fable.Core
open Fable.Import.React

type IExports =
    abstract compiler : string -> obj -> ReactElement
    abstract Markdown : obj -> ReactElement

[<Import("*", "markdown-to-jsx")>]
let MarkdownToJsx : IExports = jsNative
