module DrawWithTexture

open OpenCvSharp
open Engine.Core.Color
open Engine.Core.Light
open Engine.Core.Point
open Engine.Core.Camera
open Engine.Core.RenderTarget
open Engine.Core.Pipeline
open Engine.Core.Transformation
open Engine.Core.Texture
open Engine.Model.Obj



let RemoveBackfacesFace (cam:Camera) (vs:Point[]) (idxs:(Indexer*Indexer*Indexer)[]) =
    idxs |> Array.filter (fun (idx,_,_) ->
        let u = vs[idx.j] - vs[idx.i]
        let v = vs[idx.k] - vs[idx.i]
        let normal = u.Cross(v)
        let view = cam.Position - vs[idx.i]
        let dp = normal.Dot(view)
        dp > 0.0)
//
// pos,vs,idxs is 3D Object property
let PipelineDrawTexture (screen:Screen) (cam:Camera) (lights:Light[]) (color:Color) (pos:Point) (vs:Point[]) (idxs:(Indexer*Indexer*Indexer)[]) (uvs:Point2D[]) (texture:Texture) =
    screen.Reset()
    let worldVS = LocalToWorld pos vs
    let vsw = vs |> (fun (vs) ->
            let mat = Matrix4x4.MakeDisplacementMatrix(pos.x, pos.y, pos.z)
            vs |> Array.map (fun p -> mat.Multiply(p)))
    let frontIdxs = RemoveBackfacesFace cam worldVS idxs
    let tmpidxs = frontIdxs |> Array.map (fun (idx,_,_) ->idx)
    let finColor = lights |> Array.map (fun light -> light.Render(cam, color, worldVS, tmpidxs)) |>
                    Array.reduce (fun a b ->
                        assert(a.Length = b.Length)
                        Array.zip a b |> Array.map (fun (l,r) -> l+r))
    let screenVS = worldVS |>
                    WorldToCamera cam |>
                    CameraToPerspective cam |>
                    PerspectiveToScreen cam

    let mutable i = 0
    for (idx,uvidx,_) in frontIdxs do
        let p1 = screenVS[idx.i]
        let p2 = screenVS[idx.j]
        let p3 = screenVS[idx.k]
        let uv1 = uvs[uvidx.i]
        let uv2 = uvs[uvidx.j]
        let uv3 = uvs[uvidx.k]
        let points = DrawTrangle_new cam [|p1;p2;p3|] [|uv1;uv2;uv3|]
        points |> Array.map (fun (x,y,z,uv) ->
            let color = texture.Sample(uv.x, uv.y)
            let final = finColor[i].Uniform() * color
            screen[x,y] <- final,z) |> ignore

        i <- i + 1

let DrawCarWithTexture() =
    //let car = LoadModel("../../../../3DModel/Renault12TL/Renault12TL.obj")
    //let baseColor = Cv2.ImRead("../../../../3DModel/Renault12TL/Renault12TL_BaseColor.png")
    //let car = LoadModel("../../../../3DModel/Cube/Cube.obj")
    //let baseColor = Cv2.ImRead("../../../../3DModel/Cube/wall1.tif")
    let car = LoadModel("../../../../3DModel/spot/spot_triangulated_good.obj")
    let baseColor = Cv2.ImRead("../../../../3DModel/spot/spot_texture.png")
    let tmpArr = Array2D.create baseColor.Width baseColor.Height (Color())
    let idx = baseColor.GetGenericIndexer<Vec3b>()
    let width = baseColor.Width-1
    let height = baseColor.Height-1
    for y in 0..height do
        for x in 0..width do
            let t = idx[y,x]
            let color = Color(float t.Item2, float t.Item1, float t.Item0)
            tmpArr[x,height - y] <- color

    let texture = Texture(tmpArr, baseColor.Width, baseColor.Height)
    let cam = new Camera(Point(2,9,-15),Vector(-0.1, -0.5, 1), 90.0, 0.5, 1000.0, 1024, 768)
    let degree = 20.0
    let mutable current = 60.0

    let mutable key = 0
    while key <> int 'q' do
        let trans = Matrix4x4.MakeScaleMatrix(5,5,5) * Matrix4x4.MakeRotateMatrix(0,0,current)

        let vs = car.Vertexs |> Array.map (fun v -> v*trans)
        let idxs = car.Faces |> Array.map (fun (i,_,_) -> i)
        let uvidxs = car.Faces |> Array.map (fun (_,i,_) -> i)
        let uvs = car.TextureCoords

        let screen = new Screen(1024, 768)

        let rand = System.Random(10)
        let color = Color(rand.Next(255), rand.Next(255), rand.Next(255))
        let light = AmbientLight (Ambient_Light(Color(30,30,30,1)))
        let dirLight = DirectionLight (Direction_Light(Color(), Color(200,200,200,1), Color(), Vector(0,-30,1)))

        PipelineDrawTexture screen cam [|light;dirLight|] color (Point()) vs car.Faces uvs texture

        let mat = new Mat(Size(1024, 768), MatType.CV_8UC3)
        let indexer = mat.GetGenericIndexer<Vec3b>()
        let (w,h) = screen.Size()
        for y in 0..h-1 do
            for x in 0..w-1 do
                let c = screen.Pixel(x,y)
                let vec = Vec3b(byte c.b, byte c.g, byte c.r)
                indexer[y,x] <- vec
        Cv2.ImShow("Manga", mat)
        key <- Cv2.WaitKey()
        current <- current  + degree


