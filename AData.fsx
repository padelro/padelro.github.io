open System
open System.IO
open System.Windows.Forms
open System.Drawing
open System.Threading.Tasks
open System.Drawing.Drawing2D

Environment.SetEnvironmentVariable(
    "Path", Environment.GetEnvironmentVariable("Path") + ";" +
        Path.Combine( __SOURCE_DIRECTORY__, @"./packages/DiffSharp/build/" )
    )

#I "./packages/DiffSharp/lib/net46/"
#I "./packages/DiffSharp/build/"

#r "DiffSharp.dll"

open DiffSharp.AD.Float64

type SObj =
    {   id: int
    ;  pos: DV
    ;    v: DV
    ;  acc: DV
    ;    f: DV
    ; mass: D }

let δxy (pos1: DV) (pos2: DV) =
    let x1, y1, x2, y2 =
        pos1.[0], pos1.[1],
        pos2.[0], pos2.[1]

    let δx = x2 - x1
    let δy = y2 - y1
    δx, δy

let δ δx δy=
    sqrt ( pown δx 2 + pown δy 2 )

let f (m1: D) (m2: D) (x: DV) (y: DV) =
    let δx, δy = δxy x y
    let δ = δ δx δy
    let scaleBy = D Math.PI * 0.1e11

    let fMod =
        D 6.674e-11
        * scaleBy
        * ( m1 * m2 )
        / ( pown δ 2 )

    let α = abs (δy / δx) |> atan
    let sign = diff abs

    if δ < (m1 + m2) / 2.0
    then DV  [| 0.; 0. |]
    else DV.ofArray [| fMod * (cos α) * (sign δx); fMod * (sin α) * (sign δy) |]

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
                ) ( DV [| 0. ; 0. |] )
        )

let body (pos: DV) mass (graphics: Graphics) =
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

let force (pos: DV) (v: DV) (graphics: Graphics) =
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

let text (pos: DV) line text (graphics: Graphics)  =
    let x, y = float pos.[0], float pos.[1]
    use fontS = new Font( new FontFamily("Operator Mono Medium"), 8.0f ) // (!)
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

let drawField world graphics =
    [
        [0 .. 20 .. 1200]
        |> List.map (
            ( fun i ->
                [ 0 .. 20 .. 1200 ]
                |> List.map (
                    fun j -> async {
                        let pos = DV [| float i; float j |]
                        let m = D 0.01

                        let f =
                            world
                            |> List.fold (
                                fun Σf sObj ->
                                    Σf + f m sObj.mass pos sObj.pos * sObj.mass
                                ) (DV [| 0.; 0. |])

                        return ( pos, f )
                    }
                )
                |> Async.Parallel
                |> Async.RunSynchronously
            )
            >>
            ( fun pl ->
                pl |> Array.iter (
                    fun (pos, f) ->
                        graphics |> force pos f |> ignore
                    )
            )
        )
    ] |> ignore
let drawWorld world time (graphics: Graphics) =
    graphics |> drawEon time |> drawField world
    world |> List.iter ( drawObj >> (fun drawWith -> drawWith graphics) )

let sObjs =
    [
        {   id = 1
        ;  pos = DV [| 100.; 100.|]
        ;    v = DV [| 0.; 0. |]
        ;  acc = DV [| 0.; 0. |]
        ; mass = D 40.
        ;    f = DV [| 0.; 0. |] }
        // --
        {   id = 2
        ;  pos = DV [| 200.; 400.|]
        ;    v = DV [| 0.; 0. |]
        ;  acc = DV [| 0.; 0. |]
        ; mass = D 100.
        ;    f = DV [| 0.; 0. |] }
        // --
        {   id = 3
        ;  pos = DV [| 900.; 600.|]
        ;    v = DV [| 0.; 0. |]
        ;  acc = DV [| 0.; 0. |]
        ; mass = D 10.
        ;    f = DV [| 0.; 0. |] }
        // --
        {   id = 4
        ;  pos = DV [| 150.; 750.|]
        ;    v = DV [| 0.; 0. |]
        ;  acc = DV [| 0.; 0. |]
        ; mass = D 5.
        ;    f = DV [| 0.; 0. |] }
        // --
        {   id = 5
        ;  pos = DV [| 850.; 250.|]
        ;    v = DV [| 0.; 0. |]
        ;  acc = DV [| 0.; 0. |]
        ; mass = D 420.
        ;    f = DV [| 0.; 0. |] }
        // --
        {   id = 6
        ;  pos = DV [| 550.; 550.|]
        ;    v = DV [| 0.; 0. |]
        ;  acc = DV [| 0.; 0. |]
        ; mass = D 30.
        ;    f = DV [| 0.; 0. |] }
        // --
        {   id = 7
        ;  pos = DV [| 250.; 1000.|]
        ;    v = DV [| 0.; 0. |]
        ;  acc = DV [| 0.; 0. |]
        ; mass = D 180.
        ;    f = DV [| 0.; 0. |] }
    ]

let memoize f =
    let cache = ref Map.empty
    fun x ->
        match (!cache).TryFind(x) with
        | Some res ->
            res
        | None ->
            let res = f x
            cache := (!cache).Add(x,res)
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
        ) ( sObjs |> List.map ( fun _ -> DV.zeroCreate 2 ), sObjs , 0L )


type DrawForm() as x =
    inherit Form()
    do
        x.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)

let _program =

    let ctx = BufferedGraphicsManager.Current
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

    let view = async {
        let dResult = form.ShowDialog()
        let! endDialog = (Task.FromResult dResult) |> Async.AwaitTask
        return endDialog
    }

    view |> Async.StartAsTask |> Async.AwaitTask |> ignore

    knob.ValueChanged.Add (
        fun _ ->
            use graphics = Graphics.FromHwnd(form.Handle)
            use buffer = ctx.Allocate( graphics, form.DisplayRectangle )
            let graphics = buffer.Graphics
            graphics.SmoothingMode <- SmoothingMode.HighSpeed
            graphics.Clear(Color.WhiteSmoke)

            let time = knob.Value

            let world, tt =
                universe |> Seq.skip time |> Seq.take 1 |> Seq.exactlyOne // just to make sure

            graphics |> drawWorld world tt |> fun _ -> buffer.Render()
        )
    ()