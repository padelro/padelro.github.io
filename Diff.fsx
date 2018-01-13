#if INTERACTIVE
#time
open DiffSharp.AD.Float32
#endif

#I "../packages/DiffSharp/lib/net46/"
#I "../packages/DiffSharp/build/"
#r "DiffSharp.dll"

open System
open System.IO

Environment.SetEnvironmentVariable("Path",
    Environment.GetEnvironmentVariable("Path") + ";" + Path.Combine(__SOURCE_DIRECTORY__,@"../packages/DiffSharp/build/"))

module Playground =

    open DiffSharp.AD.Float64
    open DiffSharp.Util

    let rnd = Random ( DateTime.Now.Millisecond )

    let ε = D 1.0e-6
    let checkδ p1 p2 =
        abs (p1 - p2) |> DV.toArray |> Array.fold (+) (D 0.) |>
        function
            | d when d < ε -> true
            | d -> failwithf "DELTA over! %A" d


    let inXY =  DV [| float (rnd.Next()) ; float (rnd.Next()); |] // v1, v2

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

    let ``conversionAlmostWorks?`` = (inXY |> convCartesianPolar |> convPolarCartesian) |> checkδ inXY

    let inrθ = inXY |> convCartesianPolar

    let RJ1', RJ2' = inrθ |> jac1, inXY |> jac2

    let jacobianProduct = RJ2' * RJ1'

    let identity n = DM.init n n ( fun i j -> if i = j then 1. else 0. )

    let ``jacobianProductIsAlmostIdentity?`` =
        jacobianProduct - identity 2 |> DM.det < ε

    let reverseEverything =
        function inrθ -> ( (jac1 inrθ) |> DM.transpose |> DM.Inverse ) * inrθ

    let ``backinxy?`` = reverseEverything inrθ |> checkδ inXY

    let makeItpRETTY =
        function
            | true -> printfn "--> IS THIS OK?"
            | false -> printfn "Welp..."

    [ ``conversionAlmostWorks?``; ``jacobianProductIsAlmostIdentity?``; ``backinxy?`` ]
    |> List.forall id
    |> makeItpRETTY