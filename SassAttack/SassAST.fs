module SassAttack.SassAST

open System
open System.Collections.Generic
open System.Text

// AST
type Node =
    | Root of List<Node>
    | Comment of string
    | SassVar of string * string
    | CssProp of string * string
    | StyleGroup of string * List<Node>
    | IncludeMixin of string
    | ImportSassFile of string
    // support one-line functions
    | SassFunction of string * List<Node>
    | SassFunctionReturn of string

// Parsing result
type Command =
    | AddChild of Node
    | ReturnCursor

// Parser state (would have hold more stuff if parsing functions, mixins etc..., but they are omitted)
type State =
    | Undefined of string
    | InComment of string

// Parsing process state
type ThreadState = {
    LastSymbol: char
    CommandsReversed: Command list
    Current: State
}

exception UnhandledParsingSituation of char*ThreadState
exception UnhandledSplitSituation of string

let private optionallyAppend value list =
    if Option.isNone value then list else List.concat [value.Value; list]

let private isEmpty input = input.ToString().Trim().Length = 0
let private (|Empty|Symbol|) (input:char) = if isEmpty input then Empty else Symbol

let private (|Include|SassDeclaration|Prop|Import|Return|) (input:string) =
    if input.StartsWith("@include") then
        Include
    else
        if input.StartsWith("$") then
            SassDeclaration
        else
            if input.StartsWith("@import") then
                Import
            else
                if input.StartsWith("@return") then
                    Return
                else
                    Prop

let private (|Selector|Function|) (input:string) =
    if input.StartsWith("@function") then
        Function
    else
        Selector

let private (++) a b = a + b.ToString()
let private _split (str:string) =
    let elements = str.Split(':')
    if elements.Length >= 2 then
        elements.[0].Trim(), elements.[1].Trim()
    else
        raise (UnhandledSplitSituation str)

// Parses single symbol into the state machine - adding commands, which are equivalent to a flatten AST
let private threader state symbol =

    let update =
        match state.Current, symbol, state.LastSymbol with
        | Undefined str, '/', '/' when str.Length = 1 -> InComment "", None
        | Undefined str, '{', '#' -> Undefined (str ++ symbol), None // deal with sass for expressions like .weekday-#{$i} { }
        | Undefined str, '{', _ ->
            match str with
            | Selector -> Undefined "", Some [StyleGroup (str.Trim(), List<Node>()) |> AddChild]
            | Function -> Undefined "", Some [SassFunction (str.Trim().Substring(9), List<Node>()) |> AddChild]
        | Undefined str, '\n', _ -> Undefined str, None
        | Undefined str, ';', _ when str.Contains("url('data:image/") -> Undefined (str ++ symbol), None
        | Undefined str, ';', _ ->
            match str with
                | Include -> Undefined "", Some [ReturnCursor; str.Substring(8) |> IncludeMixin |> AddChild]
                | Import -> Undefined "", Some [ReturnCursor; str.Substring(7) |> ImportSassFile |> AddChild]
                | Return -> Undefined "", Some [ReturnCursor; str.Substring(7) |> SassFunctionReturn |> AddChild]
                | Prop -> Undefined "", Some [ReturnCursor; str |> _split |> CssProp |> AddChild]
                | SassDeclaration -> Undefined "", Some [ReturnCursor; str.Substring(1) |> _split |> SassVar |> AddChild]
        | Undefined str, '}', _ when str.Length = 0 -> Undefined "", Some [ReturnCursor]
        | Undefined str, Empty, _ when str.Length = 0 -> Undefined str, None
        | Undefined str, Empty, _ when str.Length <> 0 -> Undefined (str ++ symbol), None
        | Undefined str, Symbol, _ -> Undefined (str ++ symbol), None

        | InComment text, '\n', _ -> Undefined "", Some [ReturnCursor; text |> Comment |> AddChild]
        | InComment text, _, _ -> InComment (text ++ symbol), None

        | _, _, _ ->
            printfn "%A %c" state symbol
            raise (UnhandledParsingSituation (symbol,state))


    { state with
        LastSymbol = symbol
        Current = fst update
        CommandsReversed = state.CommandsReversed |> optionallyAppend (snd update) }

let private reverse list =
    let rec loop acc = function
        | []           -> acc
        | head :: tail -> loop (head :: acc) tail
    loop [] list

let ParseToCommands (text: string) =
    let res = text.ToCharArray() |> Seq.fold threader { LastSymbol = ' '; CommandsReversed = []; Current = Undefined "" }
    res.CommandsReversed |> reverse

exception NotSuitableParent of Command * Node
exception WrongSyntax of string

// Takes the flatten AST from the ParseToCommands result and creates the actual AST from it
let CreateTreeFromCommands (commands: Command list) =

    commands
    |> List.fold
           (fun path command ->
                match command with
                | ReturnCursor ->
                    List.tail path
                | AddChild node ->
                    match List.head path, node with
                    | Root children, _ -> children.Add(node)
                    | StyleGroup (_,children), _ -> children.Add(node)
                    | SassFunction (_, children), SassFunctionReturn _ -> children.Add(node)
                    | _, _ -> raise (NotSuitableParent (command, (List.head path)))
                    node::path)
           [Root (List<Node>())]
    // if the syntax of the file is correct there would be only one item left in the list - the root
    // However if it's not then the root is the last element in the list
    |> (fun l -> if List.length l > 1 then raise (WrongSyntax "Look out for missing ';' or wrong '{' and '}'!") else l )
    |> List.head

let rec renderTabs (builder: StringBuilder) count =
    if count > 0 then
        builder.Append('\t') |> ignore
        renderTabs builder (count - 1)

let rec private tabsString count =
    if count > 0 then
        "\t" + (tabsString (count - 1))
    else
        ""

type Renderer = {
    CommentRender: StringBuilder -> int -> string -> unit
    IncludeRender: StringBuilder -> int -> string -> unit
    ImportSassRender: StringBuilder -> int -> string -> unit
    PropRender: StringBuilder -> int -> string -> string -> unit
    SassVarRender: StringBuilder -> int -> string -> string -> unit
}

// Default renderer, which would render the Sass code it has already parsed without logical modifications
module Renderer =
    let Default = {
        CommentRender = (fun builder tabs text ->
                            renderTabs builder tabs
                            builder.Append("// ") |> ignore
                            builder.AppendLine(text) |> ignore)
        IncludeRender = (fun builder tabs text ->
                            renderTabs builder tabs
                            builder.Append("@include") |> ignore
                            builder.Append(text) |> ignore
                            builder.AppendLine(";") |> ignore)
        ImportSassRender = (fun builder tabs text ->
                            renderTabs builder tabs
                            builder.Append("@import") |> ignore
                            builder.Append(text) |> ignore
                            builder.AppendLine(";") |> ignore)
        PropRender = (fun builder tabs name value ->
                            renderTabs builder tabs
                            builder.Append(name) |> ignore
                            builder.Append(": ") |> ignore
                            builder.Append(value) |> ignore
                            builder.AppendLine(";") |> ignore)
        SassVarRender = (fun builder tabs name value ->
                            renderTabs builder tabs
                            builder.Append("$") |> ignore
                            builder.Append(name) |> ignore
                            builder.Append(": ") |> ignore
                            builder.Append(value) |> ignore
                            builder.AppendLine(";") |> ignore)
    }

// Invoke the specified renderer on to the AST generated from CreateTreeFromCommands
let rec RenderAST (renderer:Renderer) (builder: StringBuilder) (tabs: int) node =

    match node with
    | Root children -> children |> Seq.iter (RenderAST renderer builder (tabs + 1))
    | Comment text -> renderer.CommentRender builder tabs text
    | IncludeMixin text -> renderer.IncludeRender builder tabs text
    | ImportSassFile text -> renderer.ImportSassRender builder tabs text
    | CssProp (name, value) -> renderer.PropRender builder tabs name value
    | SassVar (name, value) -> renderer.SassVarRender builder tabs name value
    | StyleGroup (selectors, children) ->
        builder.AppendLine() |> ignore
        renderTabs builder tabs
        builder.AppendJoin(",\n" + (tabsString tabs), selectors.Split(',') |> Seq.map (fun sel -> sel.Trim()) |> Seq.toArray) |> ignore
        builder.AppendLine(" {") |> ignore

        children |> Seq.iter (RenderAST renderer builder (tabs + 1))

        renderTabs builder tabs
        builder.AppendLine("}") |> ignore
    | SassFunction (def, children) ->
        renderTabs builder tabs
        builder.Append("@function") |> ignore
        builder.Append(def) |> ignore
        builder.AppendLine("{") |> ignore

        children |> Seq.iter (RenderAST renderer builder (tabs + 1))

        renderTabs builder tabs
        builder.AppendLine("}") |> ignore
    | SassFunctionReturn text ->
        renderTabs builder tabs
        builder.Append("@return") |> ignore
        builder.Append(text) |> ignore
        builder.AppendLine(";") |> ignore