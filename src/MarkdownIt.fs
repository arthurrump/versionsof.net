// ts2fable 0.6.1
module rec MarkdownIt
open System
open Fable.Core
open Fable.Import.JS

let [<Import("*","markdown-it")>] markdownIt: MarkdownIt.IExports = jsNative
let [<Import("*","markdown-it")>] MarkdownIt: MarkdownItStatic = jsNative

type [<AllowNullLiteral>] IExports =
    abstract MarkdownIt: MarkdownItStaticStatic

type [<AllowNullLiteral>] MarkdownItStatic =
    [<Emit "$0($1...)">] abstract Invoke: unit -> MarkdownIt.MarkdownIt
    [<Emit "$0($1...)">] abstract Invoke: presetName: U3<string, string, string> * ?options: MarkdownIt.Options -> MarkdownIt.MarkdownIt
    [<Emit "$0($1...)">] abstract Invoke: options: MarkdownIt.Options -> MarkdownIt.MarkdownIt

type [<AllowNullLiteral>] MarkdownItStaticStatic =
    [<Emit "new $0($1...)">] abstract Create: unit -> MarkdownItStatic
    [<Emit "new $0($1...)">] abstract Create: presetName: U3<string, string, string> * ?options: MarkdownIt.Options -> MarkdownItStatic
    [<Emit "new $0($1...)">] abstract Create: options: MarkdownIt.Options -> MarkdownItStatic

module MarkdownIt =

    type [<AllowNullLiteral>] IExports =
        abstract Token: TokenStatic

    type [<AllowNullLiteral>] MarkdownIt =
        abstract render: md: string * ?env: obj option -> string
        abstract renderInline: md: string * ?env: obj option -> string
        abstract parse: src: string * env: obj option -> ResizeArray<Token>
        abstract parseInline: src: string * env: obj option -> ResizeArray<Token>
        abstract ``use``: plugin: (MarkdownIt -> ResizeArray<obj option> -> unit) * [<ParamArray>] ``params``: ResizeArray<obj option> -> MarkdownIt
        abstract utils: obj with get, set
        abstract disable: rules: U2<ResizeArray<string>, string> * ?ignoreInvalid: bool -> MarkdownIt
        abstract enable: rules: U2<ResizeArray<string>, string> * ?ignoreInvalid: bool -> MarkdownIt
        abstract set: options: Options -> MarkdownIt
        abstract normalizeLink: url: string -> string
        abstract normalizeLinkText: url: string -> string
        abstract validateLink: url: string -> bool
        abstract block: ParserBlock with get, set
        abstract core: Core with get, set
        abstract helpers: obj option with get, set
        abstract ``inline``: ParserInline with get, set
        abstract linkify: LinkifyIt with get, set
        abstract renderer: Renderer with get, set

    type [<AllowNullLiteral>] Options =
        abstract html: bool option with get, set
        abstract xhtmlOut: bool option with get, set
        abstract breaks: bool option with get, set
        abstract langPrefix: string option with get, set
        abstract linkify: bool option with get, set
        abstract typographer: bool option with get, set
        abstract quotes: string option with get, set
        abstract highlight: (string -> string -> unit) option with get, set

    type [<AllowNullLiteral>] LinkifyIt =
        abstract tlds: lang: string * linkified: bool -> unit

    type [<AllowNullLiteral>] Renderer =
        abstract rules: obj with get, set
        abstract render: tokens: ResizeArray<Token> * options: obj option * env: obj option -> string
        abstract renderAttrs: token: Token -> string
        abstract renderInline: tokens: ResizeArray<Token> * options: obj option * env: obj option -> string
        abstract renderToken: tokens: ResizeArray<Token> * idx: float * options: obj option -> string

    type [<AllowNullLiteral>] Token =
        abstract attrGet: (string -> string option) with get, set
        abstract attrIndex: (string -> float) with get, set
        abstract attrJoin: (string -> string -> unit) with get, set
        abstract attrPush: (ResizeArray<string> -> unit) with get, set
        abstract attrSet: (string -> string -> unit) with get, set
        abstract attrs: ResizeArray<ResizeArray<string>> with get, set
        abstract block: bool with get, set
        abstract children: ResizeArray<Token> with get, set
        abstract content: string with get, set
        abstract hidden: bool with get, set
        abstract info: string with get, set
        abstract level: float with get, set
        abstract map: ResizeArray<float> with get, set
        abstract markup: string with get, set
        abstract meta: obj option with get, set
        abstract nesting: float with get, set
        abstract tag: string with get, set
        abstract ``type``: string with get, set

    type [<AllowNullLiteral>] TokenStatic =
        [<Emit "new $0($1...)">] abstract Create: ``type``: string * tag: string * nesting: float -> Token

    type [<AllowNullLiteral>] TokenRender =
        [<Emit "$0($1...)">] abstract Invoke: tokens: ResizeArray<Token> * index: float * options: obj option * env: obj option * self: Renderer -> unit

    type Rule =
        Rule<obj>

    type [<AllowNullLiteral>] Rule<'S> =
        [<Emit "$0($1...)">] abstract Invoke: state: 'S * ?silent: bool -> U2<bool, unit>

    type [<AllowNullLiteral>] RuleInline =
        inherit Rule<StateInline>

    type [<AllowNullLiteral>] RuleBlock =
        inherit Rule<StateBlock>

    type Ruler =
        Ruler<obj>

    type [<AllowNullLiteral>] Ruler<'S> =
        abstract after: afterName: string * ruleName: string * rule: Rule<'S> * ?options: obj option -> unit
        abstract at: name: string * rule: Rule<'S> * ?options: obj option -> unit
        abstract before: beforeName: string * ruleName: string * rule: Rule<'S> * ?options: obj option -> unit
        abstract disable: rules: U2<string, ResizeArray<string>> * ?ignoreInvalid: bool -> ResizeArray<string>
        abstract enable: rules: U2<string, ResizeArray<string>> * ?ignoreInvalid: bool -> ResizeArray<string>
        abstract enableOnly: rule: string * ?ignoreInvalid: bool -> unit
        abstract getRules: chain: string -> ResizeArray<Rule<'S>>
        abstract push: ruleName: string * rule: Rule<'S> * ?options: obj option -> unit

    type [<AllowNullLiteral>] RulerInline =
        inherit Ruler<StateInline>

    type [<AllowNullLiteral>] RulerBlock =
        inherit Ruler<StateBlock>

    type [<AllowNullLiteral>] ParserBlock =
        abstract parse: src: string * md: MarkdownIt * env: obj option * outTokens: ResizeArray<Token> -> unit
        abstract ruler: RulerBlock with get, set

    type [<AllowNullLiteral>] Core =
        abstract ``process``: state: obj option -> unit
        abstract ruler: Ruler with get, set

    type [<AllowNullLiteral>] ParserInline =
        abstract parse: src: string * md: MarkdownIt * env: obj option * outTokens: ResizeArray<Token> -> unit
        abstract tokenize: state: State -> unit
        abstract skipToken: state: State -> unit
        abstract ruler: RulerInline with get, set
        abstract ruler2: RulerInline with get, set

    type [<AllowNullLiteral>] Delimiter =
        abstract close: bool with get, set
        abstract ``end``: float with get, set
        abstract jump: float with get, set
        abstract length: float with get, set
        abstract level: float with get, set
        abstract marker: float with get, set
        abstract ``open``: bool with get, set
        abstract token: float with get, set

    type [<AllowNullLiteral>] State =
        abstract env: obj option with get, set
        abstract level: float with get, set
        /// Link to parser instance 
        abstract md: MarkdownIt with get, set
        /// The markdown source code that is being parsed. 
        abstract src: string with get, set
        abstract tokens: ResizeArray<Token> with get, set
        /// Return any for a yet untyped property 
        [<Emit "$0[$1]{{=$2}}">] abstract Item: undocumented: string -> obj option with get, set

    type [<AllowNullLiteral>] StateInline =
        inherit State
        /// Stores `{ start: end }` pairs. Useful for backtrack
        /// optimization of pairs parse (emphasis, strikes).
        abstract cache: obj with get, set
        /// Emphasis-like delimiters 
        abstract delimiters: ResizeArray<Delimiter> with get, set
        abstract pending: string with get, set
        abstract pendingLevel: float with get, set
        /// Index of the first character of this token. 
        abstract pos: float with get, set
        /// Index of the last character that can be used (for example the one before the end of this line). 
        abstract posMax: float with get, set
        /// Push new token to "stream".
        /// If pending text exists, flush it as text token.
        abstract push: ``type``: string * tag: string * nesting: float -> Token
        /// Flush pending text 
        abstract pushPending: unit -> Token
        /// <summary>Scan a sequence of emphasis-like markers and determine whether
        /// it can start an emphasis sequence or end an emphasis sequence.</summary>
        /// <param name="start">- position to scan from (it should point to a valid marker)</param>
        /// <param name="canSplitWord">- determine if these markers can be found inside a word</param>
        abstract scanDelims: start: float * canSplitWord: bool -> obj

    type [<AllowNullLiteral>] StateBlock =
        inherit State
        /// Used in lists to determine if they interrupt a paragraph 
        abstract parentType: U5<string, string, string, string, string> with get, set
        abstract eMarks: ResizeArray<float> with get, set
        abstract bMarks: ResizeArray<float> with get, set
        abstract bsCount: ResizeArray<float> with get, set
        abstract sCount: ResizeArray<float> with get, set
        abstract tShift: ResizeArray<float> with get, set
        abstract blkIndent: float with get, set
        abstract ddIndent: float with get, set
        abstract line: float with get, set
        abstract lineMax: float with get, set
        abstract tight: bool with get, set
