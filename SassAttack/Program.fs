// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text
open SassAttack.SassAST

// In property values or var declarations replace $tmp -> var(--tmp)
// Ignore $i, which is used for sass loops
// If a math operation is detected in the value then surround the whole value with calc: $atom * 5 -> calc(var(--atom) * 5)
let replaceValue (value:string) =

    let replaced =
        value.Split('*', '(', ')', ' ', '\t', ',', '+', '/')
        |> Seq.filter (fun it -> (it.StartsWith("$") || it.StartsWith("-$")) && it <> "$i")
        |> Seq.fold (fun (newVal:string) it -> newVal.Replace(it.Substring(if it.StartsWith("-$") then 1 else 0), " var(--" + it.Substring(if it.StartsWith("-$") then 2 else 1) + ")" )) value

    let calced =
        if(replaced.Split('*', '/', '+')).Length > 1 then
            let hasImportant = replaced.EndsWith("!important")
            let withoutImportant = if hasImportant then replaced.Replace("!important", "") else replaced
            "calc( " + withoutImportant + " )" + if hasImportant then "!important" else ""
        else
            replaced

    calced

// Take care of some hardcoded composite values like padding: 2px 4px 0 0
// Treat every part of the composite as different value, so that
// 2*$atom 0 5px 0 -> calc(var(--atom)*2) 0 5px 0 instead of calc(2*var(--atom) 0 5px 0), which is meaningless
let higherReplaceValue (builder:StringBuilder) (name:string) (value:string) =
    let replacedValues =
        if ["padding"; "margin"; "border"; "border-top"; "border-bottom"; "border-left"; "border-right"; "box-shadow"] |> Seq.contains (name.Trim())
        then value.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        else [|value|]
        |> Seq.map replaceValue
    builder.AppendJoin(' ', replacedValues) |> ignore

// Renderer for sass definitions $tmp: 5px; -> --tmp: 5px;
// withRootAndSass set to true is a way to preserve sass vars for itermediate work:
// $tmp: 5px; -> --tmp: 5px; $tmp: 5px;
let sassVarRender withRootAndSass builder tabs (name:string) (value:string) =
    renderTabs builder tabs
    if withRootAndSass then builder.Append(":root{--") else builder.Append("--")
    |> ignore
    builder.Append(name) |> ignore
    builder.Append(": ") |> ignore

    // treat every "sub-value" individually
    higherReplaceValue builder name value

    if withRootAndSass then builder.AppendLine(";}") else builder.AppendLine(";")
    |> ignore

    if withRootAndSass then
        renderTabs builder tabs
        builder.Append("$") |> ignore
        builder.Append(name) |> ignore
        builder.Append(": ") |> ignore
        builder.Append(value) |> ignore
        builder.AppendLine(";") |> ignore

// Renderer for css props color: value; -> color: value2; where value2 is the result of higherReplaceValue
let propRender builder tabs (name:string) (value:string) =
    renderTabs builder tabs
    builder.Append(name) |> ignore
    builder.Append(": ") |> ignore
    // treat every "sub-value" individually
    higherReplaceValue builder name value
    builder.AppendLine(";") |> ignore

let processFile i file =

    printfn "Starting %i %s" i file

    let builder = StringBuilder()
    let renderer = { Renderer.Default with
                         SassVarRender = (sassVarRender false)
                         PropRender = propRender }

    file
    |> File.ReadAllText
    |> ParseToCommands
    |> CreateTreeFromCommands
    |> RenderAST renderer builder -1

    File.WriteAllText (file, builder.ToString())

let allFiles () =
    try
        Directory.GetFiles("/home/jedinja/dev/phoenix-frontend/src/sass/", "_wo*.scss", SearchOption.AllDirectories)
        |> Seq.filter (fun file -> ["_mixins"] |> Seq.forall (fun pattern -> not (file.Contains(pattern))))
        |> Seq.iteri processFile
    with
    | UnhandledSplitSituation str -> printfn "UnhandledSplitSituation %s" str

let singleFile () =
    try
        "/home/jedinja/dev/phoenix-frontend/src/sass/_variables.scss"
        |> processFile 0
    with
    | UnhandledSplitSituation str -> printfn "UnhandledSplitSituation %s" str

[<EntryPoint>]
let main argv =

    //singleFile()
    //allFiles()

    0
