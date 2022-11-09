open System

let z (a:float) (b: float) = (a, b)
let (!+) z1 z2 = ( (fst z1) + (fst z2), (snd z1) + (snd z2) )
let (!*) z1 z2 = ( (fst z1) * (fst z2) - (snd z1) * (snd z2), (snd z1) * (fst z2) + (fst z1) * (snd z2) )
let (!|) z = sqrt ( (fst z) * (fst z) + (snd z) * (snd z) )
let f' z c = (!+) ((!*) z z) c

let limit' max c =
    let f z = f' z c
    let rec limitRec fv n =
        if (n < max) then
            limitRec (f fv) (n + 1)
        else
            fv

    let f0 = z 0.0 0.0
    limitRec f0 0

let limit c = limit' 20 c |> (!|)

let scale' x upperLimit =
    if Double.IsNaN x then
        15.0
    elif Double.IsInfinity x then
        0.0
    else
        let m = min x 500.0
        (m * upperLimit) / 500.0

let scale x =
    scale' x 5.0

let y1, stepX, y2 = -1.3, 0.00725, 1.3
let x1, stepY, x2 = -2.0, 0.00725, 1.0

let limits =
    [ for b in y1 .. stepY .. y2 ->
        [ for a in x1 .. stepX .. x2 ->
            log ( limit (z a b) )
        ]
    ]

let maxLimit =
    limits
    |> List.collect id
    |> List.max

let run() =
    Console.ForegroundColor
    |> fun old ->
        limits |> List.iter (fun row ->
            row |> List.iter( fun v ->
                let color = enum<ConsoleColor>(v |> scale |> int)
                Console.ForegroundColor <- color
                printf "0"
            )
            printfn ""
        )
        do Console.ForegroundColor <- old