open System
open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open System.Threading.Tasks

type DrawForm() as x =
    inherit Form()
    do x.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)

type World =
    { width : int
    ; height : int
    ; seed : int
    ; size: int
    }

type GridParams =
    { margin: int
    ; offsetX: int
    ; offsetY: int
    ; cellSize: int
    ; cellSpacing: int
    ; columns: int
    ; rows: int
    ; maxNoise: float
    }

let gridParams world =
    let w, h = world.width, world.height
    let rnd0 = Random(world.seed)
    let gen = fun () -> rnd0.NextDouble()

    let cellSize, cellSpacing = 66, 22
    let margin = 150

    let gridSizeX = (w - margin*2) / ( cellSize + cellSpacing )
    let gridSizeY = (h - margin*2) / ( cellSize + cellSpacing )

    let offsetX = (w - margin*2) % ( cellSize + cellSpacing )
    let offsetY = (h - margin*2) % ( cellSize + cellSpacing )

    { margin = margin
    ; offsetX = offsetX
    ; offsetY = offsetY
    ; cellSize = cellSize
    ; cellSpacing = cellSpacing
    ; columns = gridSizeX
    ; rows = gridSizeY
    ; maxNoise = 6.2
    }

let eggshell, darkGunmetal, teaGreen, vividTangerine, englishVermillion, naColor =
    let hexColor = Drawing.ColorTranslator.FromHtml
    hexColor "#EEF4D4",
    hexColor "#1C2826",
    hexColor "#DAEFB3",
    hexColor "#EA9E8D",
    hexColor "#d64550",
    hexColor "#000000"

let showDrwaingGrid gridParams (graphics: Graphics) =
    let d = gridParams
    use p = new Pen(Color.FromArgb(100, darkGunmetal))

    p.DashStyle <- DashStyle.Dash

    graphics.DrawRectangle(
        p,
        Rectangle(
            d.margin,
            d.margin,
            d.offsetX + d.columns * (d.cellSize + d.cellSpacing),
            d.offsetY + d.rows * (d.cellSize + d.cellSpacing)
        )
    )

    let x = 50
    let y = 50

    let breakPointsX =
        [ d.margin
        ; d.offsetX/2
        ; d.cellSpacing/2
        ; d.columns * (d.cellSize + d.cellSpacing) - d.cellSpacing
        ; d.cellSpacing/2
        ; d.offsetX/2
        ; d.margin
        ]
        |> List.scan (
            ( + )
        ) 0

    let _ =
        breakPointsX
        |> List.map (fun p ->
            ( PointF( float32 p, float32 y )
            , PointF( float32 p, float32 (y + 50) )
            )
        )
        |> List.iter ( fun (p1, p2) -> graphics.DrawLine( p, p1, p2 ) )

    let breakPointsY =
        [ d.margin
        ; d.offsetY/2
        ; d.cellSpacing/2
        ; d.rows * (d.cellSize + d.cellSpacing) - d.cellSpacing
        ; d.cellSpacing/2
        ; d.offsetY/2
        ; d.margin
        ]
        |> List.scan (
            ( + )
        ) 0

    let _ =
        breakPointsY
        |> List.map (fun p ->
            ( PointF( float32 x, float32 p )
            , PointF( float32 (x + 50), float32 p )
            )
        )
        |> List.iter ( fun (p1, p2) -> graphics.DrawLine( p, p1, p2 ) )

    [0 .. d.columns - 1 ]
        |> List.iter (
            fun i ->
                [ 0 .. d.rows - 1 ]
                |> List.iter (
                    fun j ->
                        graphics.DrawRectangle(p,
                            Rectangle(
                                d.margin + d.offsetX/2 + i * (d.cellSize + d.cellSpacing) + d.cellSpacing /2,
                                d.margin + d.offsetY/2 + j * (d.cellSize + d.cellSpacing) + d.cellSpacing /2,
                                d.cellSize, d.cellSize)
                        )
                )
        )

    graphics

let genOrigins size gridParams gen =
    [1 .. size]
    |> List.map (
        fun _ ->
            Point(
                gen() * float gridParams.columns |> int,
                gen() * float gridParams.rows |> int
            )
    )
    |> List.distinct

type Operation =
    | Sketch
    | Fill
    | Ex
    | NA

type Shape =
    { origin: Point
    ; points: Point list
    ; color: Color
    ; op: Operation
    }

let defaultPen = new Pen(darkGunmetal, 2.5f)

let colorsDist =
    [ teaGreen, 0.25
    ; vividTangerine, 0.25
    ; englishVermillion, 0.25
    ; darkGunmetal, 0.25
    ]

let opsDist =
    [ Sketch, 0.5
    ; Fill, 0.3
    ; Ex, 0.2
    ]

let pickOneOf valuesWithDistribution gen acc =
    let rndOpValue = gen()
    let _, valuesWithDist =
        valuesWithDistribution
        |> List.fold (
            fun acc (value, distribution) ->
                if (fst acc) < rndOpValue
                then (fst acc + distribution, value)
                else acc
         ) acc
    valuesWithDist

let genShape (point: Point) gridParams gen =
    let rotation = ( Math.PI * 2./5. * gen() )
    let theta n = Math.PI * 2. * float n/5. + rotation
    let x n = cos (theta n) * float gridParams.cellSize/2. |> int
    let y n = sin (theta n) * float gridParams.cellSize/2. |> int

    let color = pickOneOf colorsDist gen (0.0, naColor)
    let op = pickOneOf opsDist gen (0.0, NA)

    { origin = point
    ; points =
        [ point + Size(x 1, y 1)
        ; point + Size(x 2, y 2)
        ; point + Size(x 3, y 3)
        ; point + Size(x 4, y 4)
        ; point + Size(x 5, y 5)
        ]
    ; color = color
    ; op = op
    }

let genShapeFromOrigin (origin: Point) =
    fun gridParams gen ->
        let d = gridParams

        let originPoint =
            Point(
                d.margin + d.offsetX/2 + origin.X * ( d.cellSize + d.cellSpacing ) + 2 * d.cellSpacing,
                d.margin + d.offsetY/2 + origin.Y * ( d.cellSize + d.cellSpacing ) + 2 * d.cellSpacing
            )

        genShape originPoint gridParams gen

let randomNoise (point: Point) =
    fun gridParams gen ->
        let offset = gen() * gridParams.maxNoise - gridParams.maxNoise/2.0 |> int
        point + Size(offset, offset)

let addShapeNoise shape =
    fun gridParams gen ->
        let s:Shape = shape gridParams gen
        { s with points = s.points |> List.map (fun p -> randomNoise p gridParams gen) }

let shapesFrom world =
    let rnd0 = Random(world.seed)
    let gen = fun () -> rnd0.NextDouble()
    let gridParams = gridParams world

    genOrigins world.size gridParams gen
    |> List.map (
        genShapeFromOrigin
        >> addShapeNoise
    ), gridParams, gen

let fillScreen world (color: Color) (graphics: Graphics) =
    use brush = new SolidBrush(color)
    let w, h = world.width, world.height
    graphics.FillRectangle(brush, 0, 0, w, h)
    graphics

let drawSeed seed (graphics: Graphics) =
    let x, y = 10.0f, 0.0f
    use fontS = new Font( new FontFamily("Operator Mono Medium"), 8.0f )
    let offsetX, offsetY = 2.f, 2.0f
    graphics.DrawString(
        sprintf "Seed: %i" seed,
        fontS,
        Brushes.Black,
        float32 x + offsetX, float32 y + offsetY
    )
    graphics

let drawShapesOnGrid shapes (graphics: Graphics) =
    fun gridParams gen ->
        shapes
        |> List.iter (
            fun shape ->
                let shape  = shape gridParams gen
                match shape.op with
                | Sketch ->
                    graphics.DrawPolygon(
                        new Pen(shape.color),
                        shape.points |> List.toArray
                    )
                | Fill ->
                    graphics.FillPolygon(
                        new SolidBrush(shape.color),
                        shape.points|> List.toArray |> Array.map ( fun p -> PointF(float32 p.X, float32 p.Y) )
                    )
                | Ex ->
                    graphics.FillPolygon(
                        new LinearGradientBrush( PointF(0.0f, 0.0f), PointF (2.0f, 2.0f), shape.color, defaultPen.Color ),
                        shape.points |> List.toArray |> Array.map ( fun p -> PointF(float32 p.X, float32 p.Y) )
                    )
                | _ ->
                    ()
        )

        graphics

let tick world (graphics: Graphics) =
    let shapes, gridParams, gen = shapesFrom world

    let shapesOnGrid = drawShapesOnGrid shapes

    graphics
    |> fillScreen world eggshell
    |> shapesOnGrid
    |> fun drawableGrid ->
        drawableGrid gridParams gen
    |> drawSeed world.seed
    |> showDrwaingGrid gridParams

let program =
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
            , Text = "http://www.kovach.me/posts/2018-03-07-generating-art.html"
        )

    form.Controls.Add knob

    let refresh =
        fun _ ->
            use graphics = Graphics.FromHwnd(form.Handle)
            use buffer = ctx.Allocate( graphics, form.DisplayRectangle )
            let graphics = buffer.Graphics
            graphics.SmoothingMode <- SmoothingMode.AntiAlias
            graphics.Clear(Color.WhiteSmoke)

            let seed = knob.Value
            let size = 44 * 22

            let world =
                { width = form.ClientSize.Width
                ; height = form.ClientSize.Height
                ; seed = seed
                ; size = size
                }

            graphics |> tick world |> fun _ -> buffer.Render()

    form.ResizeEnd.Add(
        refresh
    )

    knob.ValueChanged.Add (
        refresh
    )

    let view = async {
        let dResult = form.ShowDialog()
        let! endDialog = (Task.FromResult dResult) |> Async.AwaitTask
        return endDialog
    }

    view |> Async.StartAsTask |> Async.AwaitTask |> ignore