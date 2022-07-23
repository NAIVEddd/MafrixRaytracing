module IFS_6_837

open Engine.Core.Color
open Engine.Core.Light
open Engine.Core.Point
open Engine.Core.Camera
open Engine.Core.RenderTarget
open Engine.Core.Pipeline
open Engine.Core.Transformation
open Engine.Core.Texture
open Engine.Model.Obj

type IFS_Info =
    struct
        val filename : string
        val points : int
        val iters : int
        val size : int
        val output : string
        new(args : string[]) =
            assert(args[0] = "-input")
            assert(args[2] = "-points")
            assert(args[4] = "-iters")
            assert(args[6] = "-size")
            assert(args[8] = "-output")
            {
                filename = args[1];
                points = int(args[3]);
                iters = int(args[5]);
                size = int(args[7]);
                output = args[9]
            }
    end

type Probablity(ifsDef_filename:string) as this =
    let mutable mats: Matrix4x4 array = [||]
    let mutable possiblities: float array = [||]
    let mutable n : int = 0
    do
        let mutable lines = System.IO.File.ReadAllLines(ifsDef_filename)
        this.init(lines)
    member this.init(l:string[]) =
        let mutable lines = l
        n <- int(lines[0])
        lines <- Array.skip 1 lines
        while lines.Length > 0 do
            lines <- this.parse_ifs_define(lines)
        let mutable count = 0.0
        for i in 0..possiblities.Length-1 do
            count <- count + possiblities[i]
            possiblities[i] <- count
    member this.parse_ifs_define(lines:string[]) =
        possiblities <- Array.append possiblities [|double(lines[0])|]
        let nums1 = lines[1].Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map double
        let nums2 = lines[2].Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map double
        let nums3 = lines[3].Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map double
        let l =
            [|nums1[0];nums1[1];0;nums1[2];
              nums2[0];nums2[1];0;nums2[2];
              0;0;0;0;
              nums3[0];nums3[1];0;nums3[2]|]
        mats <- Array.append mats [|Matrix4x4(l)|]
        Array.skip 4 lines
    member this.sample() =
        let rand = System.Random.Shared.NextDouble()
        let mutable n = 0
        while possiblities[n] < rand do
            n <- n+1
        mats[n]
    member this.transform(vec:Vector) =
        //vec * this.sample()
        let mat = this.sample()
        let arr = Array.zeroCreate<float> 4
        let data = [|vec.x; vec.y; vec.z; 1.0|]
        for y in 0..1 do
            for x in 0..3 do
                arr[y] <- arr[y] + mat.GetRow(y)[x] * data[x]
        //arr[0] <- mat.GetRow(0)[0] * data[0] + mat.GetRow(0)[1] * data[1] + mat.GetRow(0)[3] * data[3]
        //arr[1] <- mat.GetRow(1)[0] * data[0] + mat.GetRow(1)[1] * data[1] + mat.GetRow(1)[3] * data[3]
        Vector(arr[0], arr[1], 1.)
        //vec * this.sample()


let IFS() =
    let info = IFS_Info([|"-input"; "sierpinski_triangle.txt"; "-points"; "10000"; "-iters"; "20"; "-size"; "200"; "-output"; "sierpinski_triangle_0.tga"|])
    let prob = Probablity(info.filename)
    let screen = Screen(info.size, info.size)
    let rand = System.Random.Shared

    let points = Array.zeroCreate<Vector> (info.points+1)

    for i in 1..info.points do
        let x = rand.NextDouble()// * double info.size
        let y = rand.NextDouble()// * double info.size
        let vec = Vector(x,y,1)
        points[i] <- vec
    for i in 1..info.points do
        let mutable k = 0
        while k < info.iters do
            k <- k + 1
            points[i] <- prob.transform(points[i])
    for i in 1..info.points do
        let mutable vec = points[i] * double info.size
        vec <- Vector(vec.x, double info.size - vec.y, vec.z)
        if vec.x < info.size && vec.y < info.size && vec.x >= 0.0 && vec.y >= 0.0 then
            screen[int vec.x, int vec.y] <- Color(255,255,255),100

    //let mat = new Mat(Size(info.size, info.size), MatType.CV_8UC3)
    //let indexer = mat.GetGenericIndexer<Vec3b>()
    //let (w,h) = screen.Size()
    //for y in 0..h-1 do
    //    for x in 0..w-1 do
    //        let c = screen.Pixel(x,y)
    //        let vec = Vec3b(byte c.b, byte c.g, byte c.r)
    //        indexer[y,x] <- vec
    //Cv2.ImShow("IFS", mat)
    //Cv2.WaitKey()
