open System
open System.IO
open System.Drawing
open System.Drawing.Imaging

open System.Runtime.InteropServices
open System.Runtime.CompilerServices


[<Measure>]
type px
[<Measure>]
type rad


type ColorChannel = Red | Green | Blue | Alpha


[<IsByRefLike; Struct>]
type PositionDescription =
    { x: int; y: int; stride: int; pixelSizeBytes: int }


let inline getColorForChannel channel (data: inref<byte array>) position : outref<byte> =
    let channelIdx = match channel with Blue -> 0 | Green -> 1 |  Red -> 2 | Alpha -> 3 // BGRA wtf?!?
    &data.[ position.stride * position.y + position.x * position.pixelSizeBytes + channelIdx ]


let testDataBmp(destImageFile: string) =
    let (w, h) = (6000, 3000)
    use b = new Bitmap(w, h, PixelFormat.Format32bppArgb)

    let rect = Rectangle(0, 0, w, h)
    let bmpData = b.LockBits(rect, ImageLockMode.WriteOnly, b.PixelFormat)

    let (ptr, bytes) = bmpData.Scan0, bmpData.Stride * bmpData.Height
    let pixelSizeBytes = bmpData.Stride / bmpData.Width

    let data = Array.create (w * h * pixelSizeBytes) 0uy

    let r = Random(500)

    for x in [ 0 .. w-1 ] do
        for y in [ 0 .. h-1 ] do
            let position = { x = x; y = y; stride = bmpData.Stride; pixelSizeBytes = pixelSizeBytes }
            let red   = &getColorForChannel Red   &data position
            let green = &getColorForChannel Green &data position
            let blue  = &getColorForChannel Blue  &data position
            let alpha = &getColorForChannel Alpha &data position

            do
                let v = r.NextDouble()

                if x % 100  = 0 || y % 100 = 0
                then
                    red   <- 0x7duy
                    green <- 0x7euy
                    blue  <- 0x7fuy
                else
                    red   <- byte(float 0xffuy * (if v < 0.12 then 0.8 else 1.0))
                    green <- byte(float 0xffuy * (if v < 0.14 then 0.8 else 1.0))
                    blue  <- byte(float 0xffuy * (if v < 0.11 then 0.8 else 1.0))

                alpha <- 0xffuy

    do
        Marshal.Copy(data, 0, ptr, bytes)
        b.UnlockBits(bmpData)

    use memStream = new MemoryStream()
    b.Save(memStream, ImageFormat.Png)
    let dataStream = memStream.ToArray()

    File.WriteAllBytes(destImageFile, dataStream)

    (dataStream, w, h)


let testBmp(sourceImageFile: string) =
    use b = new Bitmap(sourceImageFile)
    let w, h = b.Width, b.Height

    use memStream = new MemoryStream()
    b.Save(memStream, ImageFormat.Png)
    let dataStream = memStream.ToArray()

    (dataStream, w, h)


let ``Transform LatLong to Hammer-Aitoff`` (long, lat) =
    let z = sqrt (1.0 + cos lat * cos (long / 2.0))
    let u = cos lat * sin (long / 2.0) / z
    let v = sin lat / z

    (u, v)


let ``Transform Hammer-Aitoff to LatLong`` (u, v) =
    let z2 = 1.0 - pown (1.0/4.0 * u) 2 - pown (1.0/2.0 * v) 2
    let z = sqrt (abs z2)
    let long = 2.0 * atan(z * u / (2.0 * (2.0 * pown z 2 - 1.0)))
    let lat = asin(z * v)

    (long, lat)


let remap x (x1, x2) (x3, x4) =
    if x1 >= x2 || x3 >= x4 then failwith "Invalid range!"

    let i1 = x2 - x1
    let i2 = x4 - x3
    (x - x1) / i1 * i2 + x3


let ``Test projection: Cylindrical => Hammer`` (sourceImageFile, destImageFile: string) =
    let (data, w, h) = sourceImageFile |> testDataBmp

    use testBmp = new Bitmap(Image.FromStream(new MemoryStream(data)))
    use b = new Bitmap(w, h)
    let aspectRatio = float w / float h

    for x in 0 .. w-1 do
        for y in 0 .. h-1 do
            let (u, v) =
                remap (float x) (0.0, float (w - 1)) (-sqrt 2.0 * aspectRatio, sqrt 2.0 * aspectRatio),
                remap (float y) (0.0, float (h - 1)) (              -sqrt 2.0,               sqrt 2.0)

            let (long, lat) = ``Transform Hammer-Aitoff to LatLong`` (u, v)

            let (xSrc1, ySrc1) =
                remap long (      -Math.PI,       Math.PI) (0.0, float (w - 1)),
                remap lat  (-Math.PI / 2.0, Math.PI / 2.0) (0.0, float (h - 1))

            b.SetPixel(x, y, testBmp.GetPixel(int xSrc1, int ySrc1 % h))

    do b.Save(destImageFile, ImageFormat.Png)


let ``Test projection: Hammer ==> Cylindrical`` (sourceImageFile, destImageFile: string) =
    let (data, w, h) = testBmp(sourceImageFile)

    use testBmp = new Bitmap(Image.FromStream(new MemoryStream(data)))
    use b = new Bitmap(w, h)

    for x in 0 .. w-1 do
        for y in 0 .. h-1 do
            let (long, lat) =
                remap (float x) (0.0, float (w - 1)) (      -Math.PI,       Math.PI),
                remap (float y) (0.0, float (h - 1)) (-Math.PI / 2.0, Math.PI / 2.0)

            let (u, v) = ``Transform LatLong to Hammer-Aitoff`` (long, lat)

            let (xSrc1, ySrc1) =
                remap u (-1.0, 1.0) (0.0, float (w - 1)),
                remap v (-1.0, 1.0) (0.0, float (h - 1))

            b.SetPixel(x, y, testBmp.GetPixel(int xSrc1, int ySrc1))

    
    do b.Save(destImageFile, ImageFormat.Png)


// https://en.wikipedia.org/wiki/Cube_mapping

let convert_cube_uv_to_xyz (index, u, v) =
    match index with
    | 0 -> ( 1.0,   -v,   -u) // POSITIVE X
    | 1 -> (-1.0,   -v,    u) // NEGATIVE X
    | 2 -> (   u,  1.0,    v) // POSITIVE Y
    | 3 -> (   u, -1.0,   -v) // NEGATIVE Y
    | 4 -> (   u,   -v,  1.0) // POSITIVE Z
    | 5 -> (  -u,   -v, -1.0) // NEGATIVE Z
    | _ -> failwith "Invalid cube face"


// http://paulbourke.net/miscellaneous/cubemaps/
// https://svs.gsfc.nasa.gov/3895

let ``Test projection: Cylindrical ==> CubeMap`` (sourceImageFile: string) =
    use testBmp = new Bitmap(sourceImageFile)
    let (w, h) = testBmp.Width, testBmp.Height
    
    let rect = Rectangle(0, 0, w, h)
    let bmpData = testBmp.LockBits(rect, ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb)

    let (ptr, bytes) = bmpData.Scan0, bmpData.Stride * bmpData.Height
    let pixelSizeBytes = bmpData.Stride / bmpData.Width

    let data = Array.create (w * h * pixelSizeBytes) 0uy

    do
        Marshal.Copy(ptr, data, 0, bytes)
        testBmp.UnlockBits(bmpData)

    [
        for index in 0 .. 5 ->
            async {
                use b = new Bitmap(w / 4, h / 2, PixelFormat.Format32bppArgb)

                let l = w / 4 * h / 2

                for x in 0 .. w / 4 - 1 do
                    for y in 0 .. h / 2 - 1 do
                        let uc = remap (float x) (0.0, float (w / 4 - 1)) (-1.0, 1.0)
                        let vc = remap (float y) (0.0, float (h / 2 - 1)) (-1.0, 1.0)

                        let (vx, vy, vz) = convert_cube_uv_to_xyz (index, float uc, float vc)
                        let r = sqrt(pown vx 2 + pown vy 2 + pown vz 2)

                        let (xLong, yLat) =
                            atan2 vy vx,
                            acos(vz / r)

                        let (xSrc1, ySrc1) =
                            remap (float xLong) ( -Math.PI, Math.PI) (0.0, float (w - 1)),
                            remap (float  yLat) (      0.0, Math.PI) (0.0, float (h - 1))

                        let position = { x = int xSrc1; y = int ySrc1; stride = bmpData.Stride; pixelSizeBytes = pixelSizeBytes }
                        let red   = getColorForChannel Red   &data position
                        let green = getColorForChannel Green &data position
                        let blue  = getColorForChannel Blue  &data position
                        let alpha = getColorForChannel Alpha &data position

                        let c = Color.FromArgb(int alpha, int red, int green, int blue)

                        b.SetPixel(x, y, c)

                        let p = float (x * w / 4 + y) / float l * 100.0 // 💥
                        if abs (p - round p) <= 1.0e-6 && int p % 10 = 0 then
                            printfn "%2i %10.2f" index p

                return
                    Path.Combine(__SOURCE_DIRECTORY__ , sprintf "./cube_out_%i.png" index)
                    |> fun file ->
                        do b.Save(file, Imaging.ImageFormat.Png)
            }
    ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

    do printfn "DONE!"

let _ =
    do 
        (Path.Combine(__SOURCE_DIRECTORY__ , "./data_out.png"), Path.Combine(__SOURCE_DIRECTORY__ , "./data_T_out.png"))
        |> ``Test projection: Cylindrical => Hammer``

    do 
        (Path.Combine(__SOURCE_DIRECTORY__ , "./data_T_out.png"), Path.Combine(__SOURCE_DIRECTORY__ , "./data_T_check_out.png"))
        |> ``Test projection: Hammer ==> Cylindrical``

    do
        //Path.Combine(__SOURCE_DIRECTORY__ , "data_T_check_out.png")
        Path.Combine(__SOURCE_DIRECTORY__ , "starmap_g16k.jpg")
        |> ``Test projection: Cylindrical ==> CubeMap``


