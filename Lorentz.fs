namespace Lorentz

open System
open System.Windows.Forms
open System.Drawing
open System.Collections.Generic
open System.Diagnostics
open Aether

open DiffSharp

module Lorentz =
    let c = 1f
    let gamma v = 1f / sqrt(1f - (pown v 2 / pown c 2))
    let x' v x t = gamma v * (x + (v * t))
    let t' v t x = gamma v * (t + (v * x / pown c 2))

    let x'' v x' t' = gamma v * (x' - (v * t'))
    let t'' v t' x' = gamma v * (t' - (v * x' / pown c 2))

    type Model = {
        v: float32
        t: float32
    }
    with
        static member _v = (fun a -> a.v), (fun b (a: Model) -> { a with v = b })
        static member _t = (fun a -> a.t), (fun b (a: Model) -> { a with t = b })

    module Temp =
        // Meh?
        type Buffer<'T>(capacity: int) as __ =
            inherit Queue<'T>()

            member __.Capacity() = capacity
            member __.Add(element: 'T): unit =
                if __.Count = capacity then
                    do __.Dequeue() |> ignore
                __.Enqueue(element)

    module Utils =
        [<Measure>]
        type fps

        type FpsCounter() =
            let watch = Stopwatch()
            let mutable fps = 0<fps>
            let mutable n1 = 0<fps>
            let mutable snapshotMs = 0L
            let mutable date = DateTime.Now

            // --
            let startTime = Stopwatch.GetTimestamp()
            let elapsed = Stopwatch.GetElapsedTime(startTime)
            // --

            member __.Fps = fps

            member __.Reset() =
                watch.Reset()
                n1 <- 0<fps>; snapshotMs <- watch.ElapsedMilliseconds; fps <- 0<fps>; date <- DateTime.Now
                watch.Start()

            member __.Update() =
                n1 <- 1<fps> + n1
                if (watch.ElapsedMilliseconds - snapshotMs >= 1000L) then
                    fps <- n1; n1 <- 0<fps>; snapshotMs <- watch.ElapsedMilliseconds; date <- DateTime.Now

            member __.Date = date

    module UI =
        open Utils

        type ImageView =
            | Graph of labels: string array * toolbox: DrawingToolbox
            | Custom of labels: string array * toolbox: DrawingToolbox
        and Palette = {
            bgColor: Color

            color1: Color
            color2: Color
            colorSecondary: Color
            color4: Color

            color5: Color
            colorPrimary: Color
            color7: Color
            color8: Color

            fgColor: Color
        }
        with
            static member create(palette: Color array) = {
                bgColor = palette[0]

                color1 = palette[1]
                color2 = palette[2]
                colorSecondary = palette[3]
                color4 = palette[4]

                color5 = palette[5]
                colorPrimary = palette[6]
                color7 = palette[7]
                color8 = palette[8]

                fgColor = palette[9]
            }

        and DrawingToolbox = {
            bgColor: Color

            textBrush: Brush
            eventsBrush: Brush

            axesPen: Pen
            axesSecondaryPen: Pen

            linesPen: Pen
            timelinesPen: Pen

            fontImg: Font
        }
        with
            interface IDisposable with
                member __.Dispose() =
                    (__.textBrush :> IDisposable).Dispose()
                    (__.eventsBrush :> IDisposable).Dispose()

                    (__.axesPen :> IDisposable).Dispose()
                    (__.axesSecondaryPen :> IDisposable).Dispose()

                    (__.linesPen :> IDisposable).Dispose()
                    (__.timelinesPen :> IDisposable).Dispose()

                    (__.fontImg :> IDisposable).Dispose()

            static member createToolbox(baseFontSize, scale, palette: Palette) = {
                bgColor = palette.bgColor

                textBrush = new SolidBrush(palette.fgColor)
                eventsBrush = new SolidBrush(palette.colorPrimary)

                axesPen = new Pen(palette.color1)
                axesSecondaryPen = new Pen(palette.color2)

                linesPen = new Pen(palette.color5, float32 scale / 2f)
                timelinesPen = new Pen(palette.colorSecondary)

                fontImg = new Font("Berkeley Mono", baseFontSize * float32 scale)
            }

        // https://coolors.co/palette/03071e-370617-6a040f-9d0208-d00000-dc2f02-e85d04-f48c06-faa307-ffba08
        let palette1 =
            [| "#03071E"; "#370617"; "#6A040F"; "#9D0208"; "#D00000"; "#DC2F02"; "#E85D04"; "#F48C06"; "#FAA307"; "#FFBA08" |]
            |> Array.map ColorTranslator.FromHtml
            |> Palette.create
        // https://coolors.co/palette/03045e-023e8a-0077b6-0096c7-00b4d8-48cae4-90e0ef-ade8f4-caf0f8
        let palette2 =
            [| "#03045E"; "#023E8A"; "#0077B6"; "#0096C7"; "#00B4D8"; "#48CAE4"; "#90E0EF"; "#ADE8F4"; "#CAF0F8"; "#FDFDFD" |]
            |> Array.map ColorTranslator.FromHtml
            |> Palette.create

        type GfxForm() as __ =
            inherit Form()

            let fps = 60<fps>

            let clock interval =
                let out = new Event<_>()
                let timer = new Timer(Interval = interval, Enabled = true)
                timer.Tick.Add (fun _ -> out.Trigger DateTime.Now)
                timer.Start()
                out.Publish

            let keyDown () =
                let out = new Event<_>()
                __.KeyDown.Add(fun key -> out.Trigger key)
                out.Publish

            do
                __.SetStyle(
                    ControlStyles.OptimizedDoubleBuffer
                    ||| ControlStyles.AllPaintingInWmPaint
                    ||| ControlStyles.UserPaint, true)


            let ctx = BufferedGraphicsManager.Current
            let rawGraphics = Graphics.FromHwnd(__.Handle)
            let mutable buffer = Unchecked.defaultof<BufferedGraphics>

            override __.OnVisibleChanged(_) =
                buffer <- ctx.Allocate(rawGraphics, __.DisplayRectangle)

            member __.Tick = clock (1000 / fps * 1<fps>)
            member __.Graphics = buffer.Graphics
            member __.Render() = buffer.Render(rawGraphics)

        let form(text, (w: int, h: int)) =
            new GfxForm(
                StartPosition = FormStartPosition.Manual,
                WindowState = FormWindowState.Normal,
                Size = Size(w, h),
                Text = text,
                Left = 1000,
                Top = 800
                // TopMost = true
            )

        module CE =
            type RenderLoop<'T> =
                Delayed of (unit -> 'T)

            let inline withValues<^GfxBuilder when ^GfxBuilder : (member WithValues: PointF[] seq -> unit) > (v: PointF[] seq) (x: ^GfxBuilder) : unit =
                (^GfxBuilder : (member WithValues : PointF[] seq -> unit) (x, v))

            let inline withSecondaryAxes<^GfxBuilder when ^GfxBuilder : (member WithSecondaryAxes: PointF[] seq -> unit) > (v: PointF[] seq) (x: ^GfxBuilder) : unit =
                (^GfxBuilder : (member WithSecondaryAxes : PointF[] seq -> unit) (x, v))

            let inline paint<^GfxBuilder when ^GfxBuilder : (member Paint: int -> (Graphics -> (int * int) -> DrawingToolbox -> unit) -> unit) > (idx: int) (draw: Graphics -> (int * int) -> DrawingToolbox -> unit) (x: ^GfxBuilder) : unit =
                (^GfxBuilder : (member Paint : int -> (Graphics -> (int * int) -> DrawingToolbox -> unit) -> unit) (x, idx, draw))

            let inline model<^GfxBuilder when ^GfxBuilder : (member Model: Model) > (x: ^GfxBuilder) : Model =
                (^GfxBuilder : (member Model : Model) (x))

            type GfxBuilder(scale: int, images: ImageView array) as __ =
                let w = 1200
                let h = 400
                let baseFontSize = 9.0f
                let textBrush = new SolidBrush(Color.WhiteSmoke)

                let imagesCount = images |> Array.length

                let form = form("GFX", (w + 10 * imagesCount + (imagesCount - 1) * 10, h + 40))
                do form.Show()

                let fpsCounter = FpsCounter()
                do fpsCounter.Reset()
                let font = new Font("Berkeley Mono", baseFontSize)

                let img = Array.create imagesCount (new Bitmap(w * scale / 2, h * scale))
                let gImg = img |> Array.map ( Graphics.FromImage )

                let wImg = w / imagesCount

                [<DefaultValue>] val mutable values: PointF[] seq
                [<DefaultValue>] val mutable secondaryAxes: PointF[] seq

                member val Model: Model = { v = 0f; t = 0f } with get, set

                do
                    __.values <- [||]
                    __.secondaryAxes <- [||]

                    form.KeyDown.Add(fun key ->
                        match key.KeyCode, Form.ModifierKeys with
                        | Keys.Right, _ -> __.Model <- Optic.set (Model._v) (min ((Optic.get Model._v __.Model) + 0.025f) 1f-0.9e-5f) __.Model
                        | Keys.Left, _ -> __.Model <- Optic.set (Model._v) (max ((Optic.get Model._v __.Model) - 0.025f) -1f+0.9e-5f) __.Model

                        | Keys.Up, _ -> __.Model <- Optic.set (Model._t) (min ((Optic.get Model._t __.Model) + 0.1f) 9f) __.Model
                        | Keys.Down, _ -> __.Model <- Optic.set (Model._t) (max ((Optic.get Model._t __.Model) - 0.1f) 0f) __.Model

                        | _ -> ()
                    )

                member __.WithValues(v) =
                    __.values <- v

                member __.WithSecondaryAxes(v) =
                    __.secondaryAxes <- v

                let paintGraph (toolbox: DrawingToolbox) idx wLocal hLocal =
                    __.secondaryAxes
                    |> Seq.iter (fun v ->
                        if (v |> Array.length > 0) then
                            gImg[idx].DrawLines(toolbox.axesSecondaryPen, v |> Array.map(fun p -> PointF(p.X + (float32 wLocal)/2f, -p.Y + (float32 hLocal)/2f)))
                    )

                    __.values
                    |> Seq.iter (fun v ->
                        if (v |> Array.length > 0) then
                            gImg[idx].DrawLines(toolbox.linesPen, v |> Array.map(fun p -> PointF(p.X + (float32 wLocal)/2f, -p.Y + (float32 hLocal)/2f)))
                    )

                let paintCustom (toolbox: DrawingToolbox) idx wLocal hLocal =
                    __.values
                    |> Seq.iter (fun v ->
                        if (v |> Array.length > 0) then
                            v |> Array.iter (fun p ->
                                gImg[idx].FillEllipse(toolbox.eventsBrush, p.X + (float32 wLocal)/2f - 3f, -p.Y + (float32 hLocal)/2f - 3f, 7f, 7f)
                            )
                    )

                let drawAxesAndLabels(labels: string array) (gImg: Graphics) wLocal hLocal (toolbox: DrawingToolbox) =
                    // axes
                    gImg.DrawLine(toolbox.axesPen, 100f, float32 hLocal/2f, float32 wLocal - 100f, float32 hLocal/2f)
                    gImg.DrawLine(toolbox.axesPen, float32 wLocal/2f, 100f, float32 wLocal/2f, float32 hLocal - 100f)

                    // labels
                    let labelX = labels[0]
                    let labelY = labels[1]
                    let sx = gImg.MeasureString(labelX, toolbox.fontImg)
                    let sy = gImg.MeasureString(labelY, toolbox.fontImg)

                    gImg.DrawString(labelX, toolbox.fontImg, toolbox.textBrush, float32 wLocal - sx.Width / 2f - 20f, float32 hLocal/2f - sx.Height / 2f)
                    gImg.DrawString(labelY, toolbox.fontImg, toolbox.textBrush, float32 wLocal/2f - sy.Width / 2f, 20f)

                let drawLightAxes (gImg: Graphics) wLocal hLocal (toolbox: DrawingToolbox) =
                    // light axes 45°
                    gImg.DrawLine(toolbox.axesSecondaryPen, 300f, 100f, float32 wLocal - 300f, float32 hLocal - 100f)
                    gImg.DrawLine(toolbox.axesSecondaryPen, 300f, float32 hLocal - 100f, float32 wLocal - 300f, 100f)

                member __.Paint(idx, f) =
                    do
                        let wLocal, hLocal = (w * scale / imagesCount, h * scale)

                        match images[idx] with
                        | Graph (labels, toolbox) ->
                            gImg[idx].Clear(toolbox.bgColor)
                            drawAxesAndLabels labels gImg[idx] wLocal hLocal toolbox
                            drawLightAxes gImg[idx] wLocal hLocal toolbox
                            (paintGraph toolbox, toolbox)
                        | Custom (labels, toolbox) ->
                            gImg[idx].Clear(toolbox.bgColor)
                            drawAxesAndLabels labels gImg[idx] wLocal hLocal toolbox
                            (paintCustom toolbox, toolbox)
                        |> fun (painter, toolbox) ->
                            painter idx wLocal hLocal

                            f gImg[idx] (w * scale / imagesCount, h * scale) toolbox
                            form.Graphics.DrawImage(img[idx], 10 + idx * wImg, 10, w / 2 - 10, h - 20)

                member __.Delay(f) = f
                member __.Run(f) = Delayed f
                member __.Bind(x, f) = f (x __)
                member __.Return(x) = x

                [<TailCall>] // ??
                member __.ReturnFrom(x) =
                    match x with Delayed f -> 
                        if form.IsDisposed |> not then
                            do
                                form.Graphics.DrawString((sprintf "Fps: %A" fpsCounter.Fps), font, textBrush, 20f, 20f)
                                form.Graphics.DrawString((sprintf "Model:\n%A" __.Model), font, textBrush, 20f, 60f)
                                form.Graphics.DrawString((sprintf "→/← -> +/- model.v\n↑/↓ -> +/- model.t"), font, textBrush, 20f, 130f)
                                form.Render()

                                form.Text <- sprintf "Fps: %A Time: %A" fpsCounter.Fps fpsCounter.Date
                        else
                            failwith "Application terminated!"

                        do
                            Application.DoEvents()
                            fpsCounter.Update()

                        f ()

                with
                    interface IDisposable with
                        member __.Dispose() =
                            (textBrush :> IDisposable).Dispose()
                            (font :> IDisposable).Dispose()
                            img |> Array.iter (fun i -> (i :> IDisposable).Dispose())
                            (form :> IDisposable).Dispose()

            let gfx =
                new GfxBuilder(
                    2,
                    [|
                        ImageView.Graph (
                            [| "x"; "t" |],
                            DrawingToolbox.createToolbox (9f, 2, palette1)
                        )
                        ImageView.Custom (
                            [| "x"; "y" |],
                            DrawingToolbox.createToolbox(9f, 2, palette2)
                        )
                    |]
                )

            let runLoop = function Delayed f -> f ()

    open UI
    open UI.CE
    open FSharpPlus

    let _main (args: string array) =
        let gridSize = 50f
        let dEvents = 4f

        let rec renderLoopRec frame =
            gfx {
                let! m = model


                let values =
                    [| -9f .. 9f |]
                    |> Array.map (fun t ->
                        (gridSize * m.v * t, gridSize * t)
                        |> PointF
                    )

                let valuesAxesX' =
                    [| -9f .. 9f |]
                    |> Array.map ( fun x ->
                        [| -9f .. 9f |]
                        |> Array.map ( fun t ->
                            ((x' m.v (gridSize * x) (gridSize * t)), (t' m.v (gridSize * t) (gridSize * x)))
                            |> PointF
                        )
                    )

                let valuesAxesY' =
                    [| -9f .. 9f |]
                    |> Array.map ( fun t ->
                        [| -9f .. 9f |]
                        |> Array.map ( fun x ->
                            ((x' m.v (gridSize * x) (gridSize * t)), (t' m.v (gridSize * t) (gridSize * x)))
                            |> PointF
                        )
                    )

                do! [| values |]
                    |> withValues

                do! [| valuesAxesX'; valuesAxesY' |]
                    |> Array.concat
                    |> withSecondaryAxes

                let timelines =
                    [|
                        [|
                            (-9f * gridSize, m.t * gridSize)
                            (9f * gridSize, m.t * gridSize)
                        |] |> Array.map PointF

                        [|
                            ((x' m.v (-9f * gridSize) (m.t * gridSize)), (t' m.v (m.t * gridSize) (-9f * gridSize)))
                            ((x' m.v (9f * gridSize) (m.t * gridSize)), (t' m.v (m.t * gridSize) (9f * gridSize)))
                        |] |> Array.map PointF
                    |]

                let deltaX = gridSize * m.v * m.t

                let e1x, e1t = (x' m.v (gridSize * -dEvents) (dEvents * gridSize)), (t' m.v (dEvents * gridSize) (gridSize * -dEvents))
                let e2x, e2t = (x' m.v (gridSize * dEvents) (dEvents * gridSize)), (t' m.v (dEvents * gridSize) (gridSize * dEvents))

                let events =
                    [|
                        (-dEvents * gridSize, dEvents * gridSize)
                        (dEvents * gridSize, dEvents * gridSize)

                        (e1x, e1t)
                        (e2x, e2t)
                    |] |> Array.map PointF

                do! paint 0 (fun gImg (w, h) toolBox ->
                    timelines
                    |> Seq.iter (fun tl ->
                        if (tl |> Array.length > 0) then
                            gImg.DrawLines(toolBox.timelinesPen, tl |> Array.map(fun p -> PointF(p.X + (float32 w)/2f, -p.Y + (float32 h)/2f)))
                    )

                    if (events.Length > 0) then
                        events
                        |> Array.iter (fun e ->
                            gImg.FillEllipse(toolBox.eventsBrush, e.X + (float32 w)/2f - 3f, -e.Y + (float32 h)/2f - 3f, 7f, 7f)
                        )
                )

                // --

                let values =
                    [|
                        [|
                            (0f, 0f)
                            (-dEvents * gridSize, 0f)
                            (dEvents * gridSize, 0f)
                        |] |> Array.map PointF
                        [|
                            (deltaX, 0f)
                            (e1x, 0f)
                            (e2x, 0f)
                        |] |> Array.map PointF
                    |]

                let lightAreas =
                    [|
                        (PointF(0f, 0f), m.t * 2f * gridSize, m.t * 2f * gridSize)
                    |]

                let lightAreasCustom =
                    [| -MathF.PI .. 0.001f .. MathF.PI |]
                    |> Array.map ( fun alpha ->
                        let e1xa = x' m.v (gridSize * -m.t) (m.t * gridSize)
                        let e2xa = x' m.v (gridSize * m.t) (m.t * gridSize)

                        if (cos alpha > 0f) then
                            (e1xa * cos alpha, m.t * sin alpha * gridSize)
                        else
                            (-e2xa * cos alpha, m.t * sin alpha * gridSize)
                    )
                    |> Array.map PointF

                do! values
                    |> withValues

                do! [||]
                    |> withSecondaryAxes

                do! paint 1 (fun gImg (w, h) toolBox ->
                    if (lightAreasCustom.Length > 0) then
                        lightAreasCustom
                        |> fun la ->
                            gImg.DrawClosedCurve(toolBox.axesSecondaryPen, la |> Array.map(fun p -> PointF(p.X + (float32 w)/2f, -p.Y + (float32 h)/2f)))

                    lightAreas
                    |> Seq.iter (fun (p, rx, ry) ->
                        gImg.DrawEllipse(toolBox.axesPen, p.X + (float32 w)/2f - rx/2f, -p.Y + (float32 h)/2f - ry/2f, rx, ry)
                    )
                )

                return! (renderLoopRec (frame + 1))
                
            }

        renderLoopRec 0

#if INTERACTIVE

let s1 = "()()"
let s2 = "())("
let s1Rev = s1.ToCharArray() |> Array.rev
let s2Rev = s2.ToCharArray() |> Array.rev
let palindromeS1 = String(s1Rev) = s1
let palindromeS2 = String(s2Rev) = s2

do
    _main [||]
    |> runLoop { v = 0f; t = 0f }
    |> printfn "Result: %i"

#endif