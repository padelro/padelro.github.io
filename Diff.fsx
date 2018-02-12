#if INTERACTIVE
#time
#endif

#if INTERACTIVE
#I "./packages/DiffSharp/lib/net46/"
#I "./packages/DiffSharp/build/"
#r "DiffSharp.dll"
#endif

open System
open System.IO

Environment.SetEnvironmentVariable("Path",
    Environment.GetEnvironmentVariable("Path") + ";" + Path.Combine(__SOURCE_DIRECTORY__,@"./packages/DiffSharp/build/"))

module Playground =

    open DiffSharp.AD.Float64
    open DiffSharp.Util

    let ε = D 1.0e-5 // meh...

    let convPolarCartesian (rθ:DV) =
        let cosθ = cos (rθ.[1])
        let sinθ = sin (rθ.[1])
        let r = rθ.[0]

        DV.ofArray [| r * cosθ; r * sinθ |]

    let convCartesianPolar (xy: DV) =
        let x = xy.[0]
        let y = xy.[1]

        DV.ofArray [| sqrt (x * x + y * y) ; atan (y / x) |]

    let jac1, jac2 = jacobian convPolarCartesian, jacobian convCartesianPolar

    let checkδ p1 p2 =
        abs (p1 - p2) |> DV.toArray |> Array.fold (+) (D 0.) |>
        function
            | d when d < ε -> true
            //| _ -> false
            | d -> failwithf "DELTA over! %A" d

    let rnd = Random ( DateTime.Now.Millisecond )

    let makeItpRETTY =
        function
            | true -> printfn "%s" "\u00AF\\_(\u30C4)_/\u00AF"
            | false -> printfn "%s" "\u0028\u256F\u00B0\u25A1\u00B0\u0029\u256F\uFE35\u0020\u253B\u2501\u253B"

    let testThisOneBillionTimes (x0,y0) =
        let inXY =  DV [| x0; y0 |] // v1, v2

        let ``conversionAlmostWorks?`` = (inXY |> convCartesianPolar |> convPolarCartesian) |> checkδ inXY

        let inrθ = inXY |> convCartesianPolar

        let ``RJ1'`` = inrθ |> jac1
        let ``RJ2'`` = inXY |> jac2

        let jacobianProduct = ``RJ2'`` * ``RJ1'``

        let identity n = DM.init n n ( fun i j -> if i = j then 1. else 0. )

        let ``jacobianProductIsAlmostIdentity?`` =
            jacobianProduct - identity 2 |> DM.det < ε

        let reverseEverything =
            function inrθ -> ( (jac1 inrθ) |> DM.transpose |> DM.Inverse ) * inrθ

        let ``backinxy?`` = reverseEverything inrθ |> checkδ inXY

        [ ``conversionAlmostWorks?``; ``jacobianProductIsAlmostIdentity?``; ``backinxy?`` ]
        |> List.forall id

    let max = 100_000 // 1_000_000

    [1..max] |> List.fold ( fun s run ->
        ( if run % 50000 = 0 then printfn "%A%%" ( float run * (100. / float max) ) )

        let x0 = float (rnd.Next())
        let y0 = float (rnd.Next())
        testThisOneBillionTimes(x0, y0) && s) true |> makeItpRETTY

(* eof *)
