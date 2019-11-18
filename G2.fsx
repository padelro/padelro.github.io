// https://bivector.net/doc.html
let e = 1.0e-6
#nowarn "62"

// https://www.shapeoperator.com/2016/12/12/sunset-geometry/
// https://www.youtube.com/watch?v=ERpcSJzX448

module V2 =
    open System;

    type V = { x: float; y: float } with
        static member e1 = { x = 1.; y = 0. }
        static member e2 = { x = 0.; y = 1. }

    let addV u v = { x = u.x + v.x; y = u.y + v.y }
    let subV u v = { x = u.x - v.x; y = u.y - v.y }
    let dotV u v = u.x * v.x + u.y * v.y
    let mulS u s = { x = u.x * s; y = u.y * s }
    let divS u s = { x = u.x / s; y = u.y / s }

    type V with
        static member (+) (u, v) = addV u v
        static member (-) (u, v) = subV u v
        static member (*) (u, s) = mulS u s
        static member (*) (s, u) = mulS u s
        static member (/) (u, s) = divS u s

        static member (|.) (u, v) = dotV u v
        static member Size(v) = v |. v
        static member (~~) (u) = u / V.Size(u)
        static member (~-) (u) = mulS u -1.

    type BiV = { u: V; v: V }

    let addBiV v1 v2 = { u = v1.u + v2.u; v = v1.v + v2.v }
    let subBiV v1 v2 = { u = v1.u - v2.u; v = v1.v - v2.v }
    let mulBivS u s  = { u = u.u * s; v = u.v * s }
    let divBivS u s  = { u = u.u / s; v = u.v / s }

    type BiV with
        static member (+) (u, v) = addBiV u v
        static member (-) (u, v) = subBiV u v
        static member (*) (u, s) = mulBivS u s
        static member (/) (u, s) = divBivS u s

        static member (*) (s, u) = mulBivS u s
        static member (*) (A: BiV, v: V) = { x = v.y; y = -v.x } * (A |> BiV.Size)
        static member Size(A) = A.u.x * A.v.y - A.u.y * A.v.x

        static member __Tests() =
            let u = 3. * V.e1
            printfn "==> (u . u):\t %A" <| (abs((u |. u) - (u.x * u.x + u.y * u.y)) < e)
            let bv = { u = 3. * V.e1 ; v = 2. * V.e2 }
            printfn "==> L(bv):\t %A" <| (BiV.Size(bv) - 6. < e)

    type V with
        static member (|^) (u, v) = { u = u; v = v }
        static member (|*) (u: V, v: V) = (u |. v, u |^ v) // MV

    let project (u: V) (v: V) =
        (u |. v) * (~~v)

    let reject (u: V) (v: V) =
        (u |^ v) * (~~v)

    let reflect (a: V) (b: V) =
        (~~b |* a) |> fun (dot, wedge) -> dot * b + wedge * b

    let rotate c a b =
        let v1 = reflect c a
        reflect v1 b

    type V with
        static member __Tests() =
            let u = 3. * V.e1 + 4. * V.e2

            printfn "==> L(v):\t %A" <| (V.Size(u) - Math.Pow(5., 2.) < e)
            printfn "==> (~~u |* u):\t %A" <| ((~~u |* u) |> fun (dot, wedge) -> dot - 1. < e && wedge |> BiV.Size < e)

            let v = 3. * V.e1
            let uv = (u |* v)
            let vu = (v |* u)

            let s = ((fst uv + fst vu) / 2., (snd uv + snd vu) / 2.)
            let d = ((fst uv - fst vu) / 2., (snd uv - snd vu) / 2.)
            let uDotv = u |. v
            let uWedgev = u |^ v

            printfn "==> (u |. v):\t %A" <| (uDotv - fst s < e && snd s |> BiV.Size < e)
            printfn "==> (u |^ v):\t %A" <| (fst d < e && (uWedgev - snd d) |> BiV.Size < e)

            let a = V.e1 + V.e2
            let b = 2. * V.e1

            printfn "==> (~~b):\t %A" <| (~~b - 0.5 * V.e1 |> V.Size < e)

            printfn "==> (u proj v):\t %A" <| (project a b - V.e1 |> V.Size < e)
            printfn "==> (u reject v):\t %A" <| ((reject a b + (project a b) - a) |> V.Size < e)

            let c = -V.e1 + 3. * V.e2

            let aref = reflect a b
            let crot = rotate c a b

            printfn "==> (a reflect b):\t %A" <| ((aref + 2. * (reject a b) - a) |> V.Size < e)
            printfn "==> (c rotate a b):\t %A" <| ((crot |. a) - 2. * (a |. b) < e)

    do V.__Tests()
    do BiV.__Tests()