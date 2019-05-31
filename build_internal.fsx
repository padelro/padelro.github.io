(**
Do whatever here!
*)

(*** hide ***)
// Run the script

#I __SOURCE_DIRECTORY__
#I "./packages/FSharp.Formatting"
#load "FSharp.Formatting.fsx"

open System.IO
open FSharp.Literate
let source = __SOURCE_DIRECTORY__
let template = Path.Combine(source, "./template.html")
let script = Path.Combine(source, "./build.fsx")

let output = Literate.ProcessScriptFile(script, template, OutputKind.Html, generateAnchors = false, includeSource  = false)
//let outFile = "./build/build.html"
// TODO: breaking change