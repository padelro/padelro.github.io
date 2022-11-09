#if INTERACTIVE

// -- [MARK] : Check `global.json`
#I @"C:\Program Files\dotnet\shared\Microsoft.WindowsDesktop.App\7.0.0\"
#r "System.Windows.Forms.dll"
#r "System.Windows.Forms.Primitives.dll"
#r "Microsoft.Win32.SystemEvents.dll"
#r "System.Drawing.dll"
#r "System.Drawing.Common.dll"

#endif

open System
open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open System.Collections.Generic


#if INTERACTIVE

#r "nuget: DiffSharp-cpu"
#r "nuget: FSharp.Collections.ParallelSeq"

#endif

open DiffSharp
open FSharp.Collections.ParallelSeq

#if INTERACTIVE

do dsharp.config(dtype=Dtype.Float32, device=Device.CPU, backend=Backend.Torch)

#endif

type SObj =
    {   id: int
    ;  pos: Tensor
    ;    v: Tensor
    ;  acc: Tensor
    ;    f: Tensor
    ; mass: Tensor }

let δxy (pos1: Tensor) (pos2: Tensor) =
    let x1, y1, x2, y2 =
        pos1.[0], pos1.[1],
        pos2.[0], pos2.[1]

    let δx = x2 - x1
    let δy = y2 - y1
    δx, δy

let δ δx δy =
    sqrt ( pown δx 2 + pown δy 2 )

let scaleBy = dsharp.tensor Math.PI * 0.1e11
let G = dsharp.tensor 6.674e-11
let zero = dsharp.tensor [| 0.; 0. |]

let f (m1: Tensor) (m2: Tensor) (x: Tensor) (y: Tensor) =
    let δx, δy = δxy x y
    let δ = δ δx δy

    let fMod =
        G
        * scaleBy
        * ( m1 * m2 )
        / ( pown δ 2 )

    let α = abs (δy / δx) |> atan
    let sign = dsharp.diff abs

    if δ < (m1 + m2) / 2.0
    then zero
    else dsharp.tensor [| fMod * (cos α) * (sign δx); fMod * (sin α) * (sign δy) |]

let fObj object1 object2 =
    let m1, m2, pos1, pos2 =
        object1.mass, object2.mass,
        object1.pos, object2.pos

    f m1 m2 pos1 pos2

let updateWorld world _ =
    world
    |> List.map (
        fun object ->
            world
            |> List.filter ( fun sObj1 -> sObj1.id <> object.id )
            |> List.fold (
                fun Σf otherObject ->
                    Σf + fObj object otherObject
                ) ( zero )
        )

let body (pos: Tensor) mass (graphics: Graphics) =
    let x, y, m = float pos.[0], float pos.[1], float mass
    let p = new Pen( Color.FromArgb( 220, 120, 140, 190 ) )

    graphics.DrawEllipse(
        p,
        float32 (x - m/2.0),
        float32 (y - m/2.0),
        float32 ( m ),
        float32 ( m )
        )
    graphics // ...

let force (pos: Tensor) (v: Tensor) (graphics: Graphics) =
    let x, y = float pos.[0], float pos.[1]
    let scaleBy = 1.0e2
    let vx, vy = float v.[0], float v.[1]
    let p = new Pen( Color.FromArgb( 120, 220, 10, 50 ) )

    graphics.DrawLine(
        p,
        float32 ( x ),
        float32 ( y ),
        float32 ( x + vx * scaleBy ),
        float32 ( y + vy * scaleBy )
        )
    graphics

let text (pos: Tensor) line text (graphics: Graphics) =
    let x, y = float pos.[0], float pos.[1]
    use fontS = new Font( new FontFamily("IBM Plex Mono"), 8.0f ) // (!)
    let offsetX, offsetY = 30.f, 5.0f

    graphics.DrawString(
        sprintf "%s" text,
        fontS,
        Brushes.Black,
        float32 x + offsetX, float32 y - offsetY + ( float32 line * 10.0f )
        )
    graphics

let drawEon t (graphics: Graphics) =
    let x, y = 10.0f, 0.0f
    use fontS = new Font( FontFamily.GenericMonospace, 8.0f )
    let offsetX, offsetY = 10.f, 10.0f

    graphics.DrawString(
        sprintf "t: %i" t,
        fontS,
        Brushes.Black,
        float32 x + offsetX, float32 y + offsetY
        )
    graphics

let drawObj sObj (graphics: Graphics) =
    graphics
    |> body sObj.pos sObj.mass
    |> force sObj.pos ( sObj.v * 10 )
    |> force sObj.pos ( sObj.f * 10 )
    |> text sObj.pos 1 ( sprintf "(%3.6f, %3.6f)" ( float sObj.pos.[0] ) ( float sObj.pos.[1] ) )
    |> ignore // meh

let mAffine = dsharp.tensor 0.01

let drawFieldCache =
    seq { 0 .. 20 .. 1200 }
    |> Seq.map (
        fun i ->
            seq { 0 .. 20 .. 1200 }
            |> Seq.map (
                fun j ->
                    dsharp.tensor [| float i; float j |]
            )
    )
    |> Seq.collect id


let drawField world graphics =
    drawFieldCache
    |> PSeq.map (
        fun pos ->
            let f =
                world
                |> List.fold (
                    fun Σf sObj ->
                        Σf + f mAffine sObj.mass pos sObj.pos * sObj.mass
                    ) ( zero )

            ( pos, f )
    )
    |> Seq.iter(
        fun (pos, f) ->
            graphics |> force pos f |> ignore
    )


let drawWorld world time (graphics: Graphics) =
    graphics |> drawEon time |> drawField world
    world |> List.iter ( drawObj >> (fun drawWith -> drawWith graphics) )

let sObjs =
    [
        {   id = 1
        ;  pos = dsharp.tensor [| 100.; 100.|]
        ;    v = dsharp.tensor [| 0.; 0. |]
        ;  acc = dsharp.tensor [| 0.; 0. |]
        ; mass = dsharp.tensor 40.
        ;    f = dsharp.tensor [| 0.; 0. |] }
        // --
        {   id = 2
        ;  pos = dsharp.tensor [| 200.; 400.|]
        ;    v = dsharp.tensor [| 0.; 0. |]
        ;  acc = dsharp.tensor [| 0.; 0. |]
        ; mass = dsharp.tensor 100.
        ;    f = dsharp.tensor [| 0.; 0. |] }
        // --
        {   id = 3
        ;  pos = dsharp.tensor [| 900.; 600.|]
        ;    v = dsharp.tensor [| 0.; 0. |]
        ;  acc = dsharp.tensor [| 0.; 0. |]
        ; mass = dsharp.tensor 10.
        ;    f = dsharp.tensor [| 0.; 0. |] }
        // --
        {   id = 4
        ;  pos = dsharp.tensor [| 150.; 750.|]
        ;    v = dsharp.tensor [| 0.; 0. |]
        ;  acc = dsharp.tensor [| 0.; 0. |]
        ; mass = dsharp.tensor 5.
        ;    f = dsharp.tensor [| 0.; 0. |] }
        // --
        {   id = 5
        ;  pos = dsharp.tensor [| 850.; 250.|]
        ;    v = dsharp.tensor [| 0.; 0. |]
        ;  acc = dsharp.tensor [| 0.; 0. |]
        ; mass = dsharp.tensor 420.
        ;    f = dsharp.tensor [| 0.; 0. |] }
        // --
        {   id = 6
        ;  pos = dsharp.tensor [| 550.; 550.|]
        ;    v = dsharp.tensor [| 0.; 0. |]
        ;  acc = dsharp.tensor [| 0.; 0. |]
        ; mass = dsharp.tensor 30.
        ;    f = dsharp.tensor [| 0.; 0. |] }
        // --
        {   id = 7
        ;  pos = dsharp.tensor [| 250.; 1000.|]
        ;    v = dsharp.tensor [| 0.; 0. |]
        ;  acc = dsharp.tensor [| 0.; 0. |]
        ; mass = dsharp.tensor 180.
        ;    f = dsharp.tensor [| 0.; 0. |] }
    ]

let memoize f =
    let cache = ref (Dictionary<_,_>())
    fun x ->
        match (!cache).TryGetValue(x) with
        | (true, res) ->
            res
        | _ ->
            let res = f x
            (!cache).Add(x, res)
            res

let universe =
    Seq.unfold (
        memoize (
            fun (_, objects, t) ->
                let forces = updateWorld objects t

                let μ = 0.9
                let newWorld =
                    forces
                    |> List.zip objects
                    |> List.map (
                        fun (object, force) ->
                            let acc' = ( 1.0e1 * force ) / object.mass
                            let v' = ( μ * object.v ) + acc'
                            let pos' = object.pos + v'

                            { object with
                                pos = pos'
                                ; v = v'
                                ; acc = acc'
                                ;   f = force }
                    )

                Some ( (newWorld, t), (forces, newWorld, t + 1L) )
        )
    )

type DrawForm() as x =
    inherit Form()
    do
        x.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)

let m() =
    do dsharp.config(dtype=Dtype.Float32, device=Device.CPU, backend=Backend.Reference)

    let knob =
        new TrackBar(
            TickFrequency = 50
            , Minimum = 0
            , Maximum = 1000
            , Dock = DockStyle.Bottom
        )

    let form =
        new DrawForm(
            StartPosition = FormStartPosition.CenterScreen
            , WindowState = FormWindowState.Normal
            , Size = Size(800, 1200)
            , Text = "Space"
        )

    form.Controls.Add knob

    let view =
        use graphics = Graphics.FromHwnd(form.Handle)
        let ctx = BufferedGraphicsManager.Current
        use buffer = ctx.Allocate( graphics, form.DisplayRectangle )
        let graphics = buffer.Graphics
        graphics.SmoothingMode <- SmoothingMode.HighSpeed

        knob.ValueChanged.Add (
            fun _ ->
                graphics.Clear(Color.WhiteSmoke)

                let time = knob.Value

                let world, tt =
                    ( sObjs |> List.map ( fun _ -> Array.zeroCreate<float32> 2 |> dsharp.tensor ), sObjs , 0L )
                    |> universe |> Seq.skip time |> Seq.take 1 |> Seq.exactlyOne // just to make sure

                graphics |> drawWorld world tt |> fun _ -> buffer.Render()
            )

        form.ShowDialog()

    view