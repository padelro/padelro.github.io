open System
open System.Threading
open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Text
open System.Diagnostics
open System.Reflection
open System.Runtime.Versioning
open System.Runtime.InteropServices

open Argu

// --

open FsMoonZ
open GA
open Lorentz

type Program =
    | None = 0
    | GA = 1
    | FsMoonz = 2
    | AData = 3
    | Mandelbrot = 4
    | Gen0 = 5
    | Lorentz = 6

[<NoAppSettings>]
type CliArguments =
    | [<EqualsAssignment; AltCommandLine("-p")>] Program of Program
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Program _ -> "Specify the program to run."

[<EntryPoint>]
let main argv =
    do printfn "Hello World from F#! [%A]" DateTime.Now

    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<CliArguments>(programName = "Playground.exe", errorHandler = errorHandler)
    let args = parser.ParseCommandLine argv
    let program = args.GetResult(Program, defaultValue = Program.None)

    do printfn "Running program: [%A]..." program
    
    match program with
    | Program.None -> printfn "No program specified..."
    | Program.GA -> GA.m()
    | Program.FsMoonz -> View.m()
    | Program.AData -> AData.m() |> ignore
    | Program.Mandelbrot -> Mandelbrot.m()
    | Program.Gen0 -> Gen0.m() |> ignore
    | Program.Lorentz -> Lorentz._main [||] |> Lorentz.UI.CE.runLoop |> ignore
    | _ -> failwith "Program does not exist! See usage for help."

    let frameworkName = Assembly.GetEntryAssembly().GetCustomAttribute<TargetFrameworkAttribute>().FrameworkName
    do printfn "OS: %A, framework: %A" RuntimeInformation.OSDescription frameworkName

    0
