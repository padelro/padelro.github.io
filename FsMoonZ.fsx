#if INTERACTIVE
#load ".paket/load/main.group.fsx"
#endif

open System.IO
open System.Drawing
open System.Drawing.Imaging
open System.Drawing.Drawing2D
open System.Numerics
open System.Runtime.InteropServices

open FSharp.Collections.ParallelSeq

open FSharpPlus
open FSharpPlus.Data

module RayMarching =
    let ``MAX_STEPS`` = 300
    let e = 1.0e-10f
    let ``MAX_DIST`` = 50.f

    let rayOrigin = Vector3(0.f, 3.f, -2.f)
    let lightPosition = Vector3(0.f, 7.f, 4.0f)

    let inline dist p =
        let s1 = Vector3 (2.f, 5.f, 6.f)
        let s2 = Vector3 (-1.f, 3.f, 6.f)
        let s3 = Vector3 (-1.8f, 5.f, 4.f)

        let dS1 = (p - s1).Length() - 1.0f
        let dS2 = (p - s2).Length() - 0.5f
        let dS3 = (p - s3).Length() - 1.7f
        let dP = p.Y

        [ dP; dS1; dS2; dS3 ] |> List.min

    let inline normal p =
        let d = dist p
        Vector3(
            d - dist (p - (Vector3.UnitX * 0.1f)),
            d - dist (p - (Vector3.UnitY * 0.1f)),
            d - dist (p - (Vector3.UnitZ * 0.1f))
        )
        |> Vector3.Normalize

    let inline light (r: System.Random) (marcher: Vector3 -> Vector3-> float32) p x =
        let modifier = float32 >> ((*) (sin x)) >> (*) 1.2f
        let (rx, ry, rz) = r.NextDouble() |> modifier, r.NextDouble() |> modifier, r.NextDouble() |> modifier
        let rnd = Vector3(rx, ry, rz) * 0.12f
        let lightDirection = (lightPosition - p) |> Vector3.Normalize
        let normal = normal p
        let r = Vector3.Dot(normal + (normal * 0.13f), lightDirection + rnd) |> Vector3

        let dist = marcher (p + rnd) lightDirection
        if dist < (lightPosition - p).Length()
        then r * 0.3f
        else r + r / (0.337f * dist)


    let inline march (rayOrigin: Vector3) (rayDirection: Vector3) =
        seq { for step in 1 .. ``MAX_STEPS`` -> step }
        |> Seq.fold ( fun marchDistance _step ->
            let p = rayOrigin + rayDirection * marchDistance
            let d = dist p

            match d with
            | d when d <= e -> 0.f
            | d when d >= ``MAX_DIST`` -> 255.0f
            | d -> marchDistance + d
        ) 0.1f

    let inline marchPx rnd x y w h =
        let u = x / (float32 w) - 0.5f
        let v = y / (float32 h) - 0.5f
        let aspectRatio = (w |> float32) / (h |> float32)

        let rayDirection = Vector3(u, -v / aspectRatio, 0.1f)
        let d = march rayOrigin rayDirection

        let p = rayOrigin + rayDirection * d
        (light rnd (march) p y)

module Img =
    let inline bmpAsArray (b: Bitmap) (buffer: byte array) =
        let bmpData = b.LockBits(Rectangle(0, 0, b.Width, b.Height), ImageLockMode.ReadOnly, b.PixelFormat)
        let ptr, bytes = bmpData.Scan0, bmpData.Stride * bmpData.Height

        do
            Marshal.Copy(ptr, buffer, 0, bytes)
            b.UnlockBits(bmpData)

    let inline arrayAsBmp (buffer: byte[]) (b: Bitmap) =
        let bmpData = b.LockBits(Rectangle(0, 0, b.Width, b.Height), ImageLockMode.WriteOnly, b.PixelFormat)
        let ptr, _ = bmpData.Scan0, bmpData.Stride * bmpData.Height

        do
            Marshal.Copy(buffer, 0, ptr, buffer.Length)
            b.UnlockBits(bmpData)

    let inline colorGetT (buffer: byte array) width x y i = buffer.[ (width * 4) * y + x * 4 + i ] |> byte
    let inline colorSetT (buffer: byte array) width x y i v = buffer.[ (width * 4) * y + x * 4 + i ] <- v

    type Img =
        | Img of Bitmap
        | Internal of Bitmap * byte[]
        with
            static member Bind func (img: Img) =
                match img with
                | Img b ->
                    let buffer = Array.create (b.Width * b.Height * 4) 0uy
                    let rnd = System.Random()

                    do begin
                        (bmpAsArray b buffer)

                        let w, h = b.Width, b.Height
                        let colorGet = colorGetT buffer w
                        let colorSet = colorSetT buffer w

                        seq { for x in 0 .. w-1 -> seq { for y in 0 .. h-1 -> (x, y) } }
                        |> PSeq.iter (fun scan ->
                            scan |> Seq.iter (fun (x, y) ->
                                let (r, g, b) = func (x, y, (colorGet x y 2, colorGet x y 1, colorGet x y 0), rnd)

                                colorSet x y 3 255uy
                                colorSet x y 0 b
                                colorSet x y 1 g
                                colorSet x y 2 r
                            )
                        )
                    end
                    Internal (b, buffer)

                | Internal (b, buffer) ->
                    let rnd = System.Random()

                    do begin
                        let w, h = b.Width, b.Height
                        let colorGet = colorGetT buffer w
                        let colorSet = colorSetT buffer h

                        seq { for x in 0 .. w-1 -> seq { for y in 0 .. h-1 -> (x, y) } }
                        |> PSeq.iter (fun scan ->
                            scan |> Seq.iter (fun (x, y) ->
                                let (r, g, b) = func (x, y, (colorGet x y 2, colorGet x y 1, colorGet x y 0), rnd)

                                colorSet x y 3 255uy
                                colorSet x y 0 b
                                colorSet x y 1 g
                                colorSet x y 2 r
                            )
                        )
                    end
                    Internal (b, buffer)

            static member ReturnFrom (img: Img) =
                match img with
                | Img b -> b
                | Internal (b, buffer) ->
                    do arrayAsBmp buffer b
                    b

    type ImgBuilder() =
        member _.Bind(value, func) = Img.Bind func value
        member _.ReturnFrom(value: Img) = Img.ReturnFrom value
        member _.Return(rgb: (byte * byte * byte)) = rgb

    let img = ImgBuilder()

module View =
    open RayMarching
    open Img

    let w, h = (3840, 1600)
    let b = new Bitmap(w, h)
    let g = Graphics.FromImage b
    g.SmoothingMode <- SmoothingMode.HighQuality
    g.Clear(Color.Red)

    let m() =
        printfn "Exec [shader >> renderer]..."

        let toImg (b: Bitmap) = (Img b)
        let renderer = fun bI -> img { return! bI }
        let shader = fun (bI) -> img {
            let! (x, y, (_, _, _), rnd) = bI

            let (r, g, b) =
                marchPx rnd (float32 x) (float32 y) w h
                |> fun c ->
                    min 255.f ((max 0.f c.X) * 163.f),
                    min 255.f ((max 0.f c.Y) * 144.f),
                    min 255.f ((max 0.f c.Z) * 146.f)

            let gray = 0.2126f * r + 0.7152f * g + 0.0722f * b |> byte

            return
                if x > 7 && x < 140 && y > 10 && y < 1337
                    then (128uy, gray, gray)
                    else (gray, gray, gray)
        }

        let bmpOut = b |> toImg |> (shader >> renderer)
        do g.DrawString("-> FsMoonZ 2.0c", new Font("PragmataPro Mono Liga", 36.f, FontStyle.Italic), Brushes.YellowGreen, 150.f, 10.f)

        do bmpOut.Save(Path.Combine(__SOURCE_DIRECTORY__ + "./adata/smoons.png"), ImageFormat.Png)

        printfn "Done!"

#if INTERACTIVE

#time "on"
do View.m()
#time "off"

#endif

[<RequireQualifiedAccess>]
module Implicit =
    let inline Invoke (x: ^t) = ((^R or ^t) : (static member op_Implicit : ^t -> ^R) x) : ^R

module ImplicitUsage =
    type T1 = { v : int }
        with
            static member op_Implicit { v = v } = v + 10
            static member op_Implicit v = { v = v - 5 }
            static member FromInt(v: int): T1 = Implicit.Invoke(v)
            static member ToInt(v: T1): int = Implicit.Invoke(v)

#if INTERACTIVE

ImplicitUsage.T1.FromInt 4 |> T1.ToInt |> printfn "int <-> T1 <-> int: %A"

#endif