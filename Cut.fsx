#if INTERACTIVE

// -- [MARK] : Check `global.json`
#I @"C:\Program Files\dotnet\shared\Microsoft.WindowsDesktop.App\9.0.0\"
#r "System.Private.Windows.Core.dll"
#r "System.Windows.Forms.dll"
#r "System.Windows.Forms.Primitives.dll"
#r "Microsoft.Win32.SystemEvents.dll"
#r "System.Drawing.dll"
#r "System.Drawing.Common.dll"

#endif

open System.IO
open System.Drawing
open System.Drawing.Imaging

let dot = Directory.GetCurrentDirectory()

let _in = Path.Combine( dot, "./adata")
let _out = Path.Combine( dot, "./adata/out")

let createOut out =
    if not ( Directory.Exists out ) then
        Directory.CreateDirectory( out ) |> printfn "Created: %A" |> ignore

let crop (file: FileInfo) =
    let source = new Bitmap( file.FullName )
    let dest = new Bitmap( 1000, 1000 )
    dest.SetResolution( source.HorizontalResolution, source.VerticalResolution )
    let graphics = Graphics.FromImage ( dest )
    graphics.DrawImage( source, Rectangle(0, 0, 1500, 1500), 350, 350, 1500, 1500, GraphicsUnit.Pixel )
    let ms = new MemoryStream()
    dest.Save( ms, ImageFormat.Png )
    file.Name, ms.GetBuffer()

let combine path1 (path2, data) =
    Path.Combine( path1, path2 ), data

[
    for file in Directory.GetFiles( _in, "*.png" ) ->
        file
        |> FileInfo
        |> crop
        |> combine _out
        //|> File.WriteAllBytes
]
