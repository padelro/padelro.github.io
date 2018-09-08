#r "paket:
nuget FSharp.Core
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Cli
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.IO //enables !! and globbing
open Fake.IO.Globbing.Operators

// Properties
let buildDir = "./build/"

// Helpers
let buildDebug =
    fun (options : DotNet.BuildOptions) ->
        { options with Configuration = DotNet.BuildConfiguration.Debug }

// Targets
Target.create "Clean" (fun _ ->
    Shell.cleanDir buildDir // nothing there

    !!"./**/*.fsproj"
    |> Seq.map (DotNet.exec id "clean")
    |> Seq.iter (printfn "%A")
)

Target.create "BuildApp" (fun _ ->
    !!"./**/*.fsproj"
    |> Seq.iter (DotNet.build buildDebug)
)

Target.create "All" (fun _ ->
    Trace.trace "No target was specified..."
)

// Dependencies
open Fake.Core.TargetOperators

"Clean" ==> "BuildApp" ==> "All"

// Start build
Target.runOrDefault "All"
