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

[<Measure>]
type deg
[<Measure>]
type rad

let toDeg (angle:float):float<deg> = angle * 1.<deg>
let radToDeg (rad: float<rad>) = rad * 180.<deg> / (Math.PI * 1.<rad>) // oh, come on...
let degToRad (deg: float<deg>) = deg * (Math.PI * 1.<rad>) / 180.<deg>

// #load "./packages/FSharp.Charting/lib/net45/FSharp.Charting.fsx" // TODO: breaking change
#I "./packages/FSharp.Charting/lib/net45/"
#r "FSharp.Charting.dll"

module Playground =

    open DiffSharp.AD.Float64
    open DiffSharp.Util

    let ε = D 1.0e-1 // meh...

    let convPolarCartesian (rθ: DV) =
        let cosθ = cos (rθ.[1])
        let sinθ = sin (rθ.[1])
        let r = rθ.[0]

        DV.ofArray [| r * cosθ; r * sinθ |]

    let convCartesianPolar (xy: DV) =
        let x = xy.[0]
        let y = xy.[1]

        DV.ofArray [| sqrt ( (pown x 2) + (pown y 2) ) ; atan (y / x) |]

    let jac1, jac2 = jacobian convPolarCartesian, jacobian convCartesianPolar

    let checkδxy (p1: DV) (p2: DV) =
        [| pown (p2.[0] - p1.[0]) 2
           pown (p2.[1] - p1.[1]) 2 |]
        |> Array.fold (+) (D 0.) |> sqrt |>
        function
            | d when d < ε -> true
            | _ -> false
            //| d -> failwithf "DELTA over! %A (p1: %A p2: %A)" d p1 p2

    let toDeg rad = rad * 180. / Math.PI
    let toRad deg = deg * Math.PI / 180.

    let checkδrθ (p1: DV) (p2: DV) =
        [| pown ( p1.[0] - p2.[0] ) 2
           D ( abs ( ( (float p1.[0]) |> toDeg ) - ( (float p2.[0]) |> toDeg ) ) ) |] // ?
        |> Array.fold (+) (D 0.) |>
        function
            | d when d < ε -> true
            | _ -> false
            //| d -> failwithf "DELTA over! %A (p1: %A p2: %A)" d p1 p2

    let rnd = Random ( DateTime.Now.Millisecond )

    let makeItpRETTY =
        function
            | true -> printfn "%s" "\u00AF\\_(\u30C4)_/\u00AF"
            | false -> printfn "%s" "\u0028\u256F\u00B0\u25A1\u00B0\u0029\u256F\uFE35\u0020\u253B\u2501\u253B"

    let testThisOneBillionTimes x0 y0 =
        let inXY =  DV [| x0; y0 |]

        let ``conversionAlmostWorks?`` = ( inXY |> convCartesianPolar |> convPolarCartesian |> convCartesianPolar |> convPolarCartesian ) |> checkδxy inXY

        let inrθ = inXY |> convCartesianPolar

        let ``canWeConvert?`` =  D ( float inrθ.[1] |> toDeg |> toRad |> toDeg |> toRad ) - inrθ.[1] < ε

        let ``canWeConvertWithMeasures?`` =  D ( (float inrθ.[1] * 1.<rad> |> radToDeg |> degToRad |> radToDeg |> degToRad) / 1.<rad> ) - inrθ.[1] < ε

        let ``RJ1'`` = inrθ |> jac1
        let ``RJ2'`` = inXY |> jac2

        let jacobianProduct = ``RJ2'`` * ``RJ1'``

        let identity n = DM.init n n ( fun i j -> if i = j then 1. else 0. )

        let ``jacobianProductIsAlmostIdentity?`` =
            ( jacobianProduct |> DM.det ) - 1. < ε

        let reverseEverythingToXy =
            function inrθ -> inrθ * ( inrθ |> jac1 |> DM.Inverse )

        // printfn "Check back in xy..."
        let revXY = reverseEverythingToXy inrθ
        let ``backinxy?`` =  revXY |> checkδxy inXY

        let reverseEverythingTorθ =
            function inxy -> inxy * ( inxy |> jac2 |> DM.Inverse )

        // printfn "Check back in rθ..."
        let revrθ = reverseEverythingTorθ inXY
        let ``backinrθ?`` =  revrθ |> checkδrθ inrθ

        let props =
            [ ``canWeConvert?``; ``canWeConvertWithMeasures?``; ``conversionAlmostWorks?``; ``jacobianProductIsAlmostIdentity?``; ``backinxy?``; ``backinrθ?`` ]
            |> List.forall id

        props, ( (inXY, revXY) , (inrθ , revrθ) )

    let max = 5_000 // 1_000_000_000

    open FSharp.Charting

    let run =
        [1..max] |> List.fold ( fun s run ->
            ( if run % 5000 = 0 then printfn "%A%%" ( float run * (100. / float max) ) )

            let whatever = 8999
            let x0 = float ( rnd.Next(whatever) )
            let y0 = float ( rnd.Next(whatever) )

            ( testThisOneBillionTimes x0 y0 ) :: s ) []
            |> List.unzip
            |> fun (props, lists) ->
                props |> List.fold (&&) true |> makeItpRETTY
                lists
                |> List.unzip
                |> fun ( linXY, linrθ ) ->
                    let xyPoints =
                        linXY |> List.unzip |> fun (l1, l2) ->
                        [
                            l1 |> List.map ( fun e -> (float e.[0], float e.[1]) ) |> Chart.Point
                            l2 |> List.map ( fun e -> (float e.[1], float e.[0]) ) |> Chart.Point
                        ]
                    let rθPoints =
                        linrθ |> List.unzip |> fun (l1, l2) ->
                        [
                            l1 |> List.map ( fun e -> ( float e.[0], (float e.[1] |> toDeg) * 1.e+2 ) ) |> Chart.Point
                            l2 |> List.map ( fun e -> ( float e.[1] * 4.e+11, (float e.[0] |> toDeg) * 1.e-2 ) ) |> Chart.Point
                        ]
                    Chart.Combine ( (xyPoints @ rθPoints) ) // |> List.map ( fun chart -> chart :> ChartTypes.GenericChart ) )

    do run |> Chart.Show

    printfn "What next?"

(* eof *)
