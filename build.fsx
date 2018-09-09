(**
# Test build script

    dotnet tool install --global fake-cli
    fake run build.fsx

Setting this whole thing up:

- .NET Core and Fake CLI installed -> FAKE runs on .NET Core
- A build task that uses a .NET 4.6 library -> Fsharp.Formating runs on .NET 4.6
- Fake runs `build.fsx` which uses `fsi` with NetStandard to load and execute `build_internal.fsx` and uses Fsharp.Formating

*)

(*** include: build_internal ***)

(**
If one of the "other" dependencies does not exist, everything fails...

![Build dependencies](../adata/build_stuff.png "build_0")
*)

(*** hide ***)

#r "paket: groupref Build"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.IO
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


(*** define: build_internal***)
Target.create "Literate" (fun _ ->
    let script = "build_internal.fsx"
    let (exitcode, msgs) =
        Fsi.exec (fun p ->
            { p with TargetProfile = Fsi.Profile.NetStandard }
            |> Process.setEnvironmentVariable "__X_TEMP__" "zero"
        ) script [ "_x_arg_" ]

    Trace.trace (sprintf "%A %A\n" exitcode msgs)
)

(*** hide ***)

Target.create "All" (fun _ ->
    Trace.trace "No target was specified..."
)

// Dependencies
open Fake.Core.TargetOperators

"Clean" ==> "BuildApp" ==> "All"
"Clean" ==> "Literate"

// Start build
Target.runOrDefault "All"

(**
All done!
*)
