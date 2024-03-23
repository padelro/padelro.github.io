open System
open System.Numerics
// https://youtu.be/vNoFdtcPFdk

let rk4SingleStep (f: float32 -> Vector3 -> Vector3) dt tk (xk: Vector3) =
    let f1 = f tk xk
    let f2 = f (tk + dt/2.f) (xk + dt/2.f * f1)
    let f3 = f (tk + dt/2.f) (xk + dt/2.f * f2)
    let f4 = f (tk + dt) (xk + dt * f3)

    xk + dt/6.f * (f1 + 2.f * f2 + 2.f * f3 + f4)

let lorenz _t (x: Vector3) sigma beta rho =
    Vector3(
        sigma * (x[1] - x[0]),
        x[0] * (rho - x[2]) - x[1],
        x[0] * x[1] - beta * x[2]
    )

let sigma = 10.f
let beta = 8.f/3.f
let rho = 28.f

let x0 = Vector3(-8.f, 8.f, 7.f)
let dt = 0.01f

let out =
    [| -30.f .. dt .. 30.f |]
    |> Array.scan (fun xin time ->
        let f = fun t x -> lorenz t x sigma beta rho
        rk4SingleStep f dt time xin
    ) x0

let m() =
    out
    |> Array.map( fun p -> sprintf "%f,%f,%f\n" p[0] p[1] p[2])
    |> Array.fold (+) "X,Y,Z\n"
    |> fun txt -> System.IO.File.WriteAllText("./out.txt", txt)

#if INTERACTIVE
do m()
#endif

// https://chart-studio.plotly.com/create/