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

type DxForm(size: int * int) as __ =
    inherit Form(Text = "Main")
    let (w, h) = size

    do
        __.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.UserPaint ||| ControlStyles.ResizeRedraw, true)
        __.ResizeRedraw <- true
        __.StartPosition <- FormStartPosition.Manual
        __.Location <- Point(50, 100)
        __.ClientSize <- Size(w, h)

    override __.OnLoad(evt) =
        do base.OnLoad(evt)

        __.KeyDown.Add(fun key ->
            match key.KeyCode with
            | Keys.Escape -> __.Close()
            | _  -> ()
        )

open Aether
open Aether.Operators
open G2.V2

type V with
    static member _x = (fun a -> a.x), (fun b (a: V) -> { a with x = b })
    static member _y = (fun a -> a.y), (fun b (a: V) -> { a with y = b })

type Model =
    {
        mutable case: int

        // case 1
        u: V
        v: V

        // case 2
        a: V
        b: V
        c: V
    } with
    static member _u = (fun a -> a.u), (fun b (a: Model) -> { a with u = b })
    static member _v = (fun a -> a.v), (fun b (a: Model) -> { a with v = b })

    static member _a = (fun a -> a.a), (fun b (a: Model) -> { a with a = b })
    static member _b = (fun a -> a.b), (fun b (a: Model) -> { a with b = b })
    static member _c = (fun a -> a.c), (fun b (a: Model) -> { a with c = b })

    static member _ux = Model._u >-> V._x
    static member _uy = Model._u >-> V._y
    static member _vx = Model._v >-> V._x
    static member _vy = Model._v >-> V._y

    static member _ax = Model._a >-> V._x
    static member _ay = Model._a >-> V._y
    static member _bx = Model._b >-> V._x
    static member _by = Model._b >-> V._y
    static member _cx = Model._c >-> V._x
    static member _cy = Model._c >-> V._y

[<EntryPoint>]
let main argv =
    use w = new DxForm((1200, 800), FormBorderStyle = FormBorderStyle.FixedSingle)
    use t = new Timer(Interval = int (1000./60.), Enabled = true)
    use gw = Graphics.FromHwnd(w.Handle)

    let ctx = BufferedGraphicsManager.Current
    use bg = ctx.Allocate(gw, w.DisplayRectangle)
    use g = bg.Graphics
    do g.SmoothingMode <- SmoothingMode.HighQuality

    use font = new Font("Elementa", 12.f)
    use fontS = new Font("Elementa", 10.f, FontStyle.Italic)
    use brush = new SolidBrush(Color.FromArgb(255, 25, 25, 25))

    let ttt = 8
    let bs = Array.init ttt (fun i ->
        let x = Math.PI * float i / float ttt
        let f = sin x
        new SolidBrush(Color.FromArgb(int (255. * f), 100 + i * 7, 40 + i * 3, 50 + i * 5))
    )

    let mutable model =
        {
            case = 1
            u = 3. * V.e1 + 4. * V.e2
            v = 3. * V.e1
            a = V.e1 + V.e2
            b = 2. * V.e1
            c = -V.e1 + 3. * V.e2
        }

    let (ox, oy) = ( float32 w.ClientSize.Width / 2.f, 400.f )
    let gridSize = 100.0f

    let drawGrid (ox, oy) =
        let s = 1.f
        for _x in -300.f .. gridSize .. 300.f do
            for _y in -300.f .. gridSize .. 300.f do
                g.DrawEllipse(Pens.DarkRed, ox + _x - s, oy - _y - s, 2.f * s, 2.f * s)

    let drawV (v: V) (c : Color) s name =
        using (new Pen(c, s)) (fun p ->
            g.DrawLine(
                p,
                ox, oy,
                ox + gridSize * float32 (v.x),
                oy - gridSize * float32 (v.y)
            )
            g.DrawString(name, fontS, brush, ox + gridSize * float32 (v.x) / 2.f, oy - gridSize * float32 (v.y) / 2.f)
        )

    let drawBiV (v: BiV) (c : Color) s =
        using (new Pen(c, s)) (fun p ->
            g.DrawPolygon(p,
                [|
                    PointF(ox, oy)
                    PointF(ox + gridSize * float32 (v.u.x), oy - gridSize * float32 (v.u.y))
                    PointF(ox + gridSize * float32 (v.u.x + v.v.x), oy - gridSize * float32 (v.u.y + v.v.y))
                    PointF(ox + gridSize * float32 (v.v.x), oy - gridSize * float32 (v.v.y))
                |]
            )
        )


    t.Tick.Add(fun _ ->
        g.Clear(Color.LightGray)
        let now = DateTime.Now.ToString()
        let txtSize = g.MeasureString(now, font)

        [ 1 .. ttt ]
        |> List.iter (fun i ->
            g.DrawString(now, font, bs.[i - 1],
                (float32 w.ClientSize.Width - txtSize.Width + float32(i % 2)) / 2.f,
                (float32 w.ClientSize.Height - txtSize.Height - float32 i - 55.f)
            )
        )

        do drawGrid (ox, oy)

        model.case |> function
        | 1 ->
            let uv = (model.u |* model.v)

            do drawV model.u Color.Red 2.f (nameof model.u)
            do drawV model.v Color.LightBlue 2.f (nameof model.v)
            do drawBiV (uv |> snd) Color.DarkOliveGreen 1.f

        | 2 ->
            let ``a||`` = project model.a model.b
            let ``a|_`` = reject model.a model.b
            let a' = model.a - ``a||``

            let ``a reflectd b`` = reflect model.a model.b
            let ``c rotated a b`` = rotate model.c model.a model.b

            do drawV model.a Color.LightSeaGreen 2.f (nameof model.a)
            do drawV model.b Color.Blue 2.f (nameof model.b)
            do drawV ``a||`` Color.Yellow 6.f (nameof ``a||``)
            do drawV ~~model.b Color.DeepPink 4.f ("-" + nameof model.b)
            do drawV a' Color.DarkTurquoise 6.f String.Empty
            do drawV ``a|_`` Color.Green 2.f (nameof ``a|_``)
            do drawBiV (model.a |^ model.b) Color.HotPink 1.f

            do drawV model.c Color.Black 4.f (nameof model.c)
            do drawV ``a reflectd b`` Color.AliceBlue 2.f (nameof ``a reflectd b``)
            do drawV ``c rotated a b`` Color.DeepPink 2.f (nameof ``c rotated a b``)
        | _ -> ()

        /// --

        do bg.Render()
        w.Text <- sprintf "Time: %A" (DateTime.Now)
    )

    w.KeyDown.Add(fun key ->
        match key.KeyCode, Form.ModifierKeys with
        | Keys.D1, _ -> model.case <- 1
        | Keys.D2, _ -> model.case <- 2

        | Keys.D, Keys.Shift ->
            match model.case with
            | 1 -> model <- Optic.set (Model._ux) (model.u.x + 0.5) model
            | 2 -> model <- Optic.set (Model._ax) (model.a.x + 0.5) model
            | _ -> ()
        | Keys.A, Keys.Shift ->
            match model.case with
            | 1 -> model <- Optic.set (Model._ux) (model.u.x - 0.5) model
            | 2 -> model <- Optic.set (Model._ax) (model.a.x - 0.5) model
            | _ -> ()
        | Keys.W, Keys.Shift ->
            match model.case with
            | 1 -> model <- Optic.set (Model._uy) (model.u.y + 0.5) model
            | 2 -> model <- Optic.set (Model._ay) (model.a.y + 0.5) model
            | _ -> ()
        | Keys.S, Keys.Shift ->
            match model.case with
            | 1 -> model <- Optic.set (Model._uy) (model.u.y - 0.5) model
            | 2 -> model <- Optic.set (Model._ay) (model.a.y - 0.5) model
            | _ -> ()

        | Keys.D, Keys.Control ->
            match model.case with
            | 2 -> model <- Optic.set (Model._cx) ((Optic.get (Model._cx) model) + 0.5) model
            | _ -> ()
        | Keys.A, Keys.Control ->
            match model.case with
            | 2 -> model <- Optic.set (Model._cx) ((Optic.get (Model._cx) model) - 0.5) model
            | _ -> ()
        | Keys.W, Keys.Control ->
            match model.case with
            | 2 -> model <- Optic.set (Model._cy) ((Optic.get (Model._cy) model) + 0.5) model
            | _ -> ()
        | Keys.S, Keys.Control ->
            match model.case with
            | 2 -> model <- Optic.set (Model._cy) ((Optic.get (Model._cy) model) - 0.5) model
            | _ -> ()

        | Keys.D, _ ->
            match model.case with
            | 1 -> model <- Optic.set (Model._vx) (model.v.x + 0.5) model
            | 2 -> model <- Optic.set (Model._bx) (model.b.x + 0.5) model
            | _ -> ()
        | Keys.A, _ ->
            match model.case with
            | 1 -> model <- Optic.set (Model._vx) (model.v.x - 0.5) model
            | 2 -> model <- Optic.set (Model._bx) (model.b.x - 0.5) model
            | _ -> ()
        | Keys.W, _ ->
            match model.case with
            | 1 -> model <- Optic.set (Model._vy) (model.v.y + 0.5) model
            | 2 -> model <- Optic.set (Model._by) (model.b.y + 0.5) model
            | _ -> ()
        | Keys.S, _ ->
            match model.case with
            | 1 -> model <- Optic.set (Model._vy) (model.v.y - 0.5) model
            | 2 -> model <- Optic.set (Model._by) (model.b.y - 0.5) model
            | _ -> ()
        | _, k when k <> Keys.Shift && k <> Keys.Control-> model.case <- 0
        | _ -> ()
    )

    do w.ShowDialog() |> ignore
    bs |> Array.iter(fun o -> (o :> IDisposable).Dispose())
    printfn "Hello World from F#! [%A]" DateTime.Now

    let framework = Assembly.GetEntryAssembly().GetCustomAttribute<TargetFrameworkAttribute>().FrameworkName
    printfn "%A" (RuntimeInformation.OSDescription, framework)

    0 // return an integer exit code
