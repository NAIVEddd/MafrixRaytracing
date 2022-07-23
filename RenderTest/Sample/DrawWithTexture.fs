module DrawWithTexture

open Engine.Core.Color
open Engine.Core.Light
open Engine.Core.Point
open Engine.Core.Camera
open Engine.Core.RenderTarget
open Engine.Core.Pipeline
open Engine.Core.Transformation
open Engine.Core.Texture
open Engine.Model.Obj


let DrawCarWithTexture() =
    //let car = LoadModel("../../../../3DModel/Renault12TL/Renault12TL.obj")
    //let baseColorFile = "../../../../3DModel/Renault12TL/Renault12TL_BaseColor.png"
    //let car = LoadModel("../../../../3DModel/Cube/Cube.obj")
    //let baseColorFile = "../../../../3DModel/Cube/wall1.tif"
    let car = LoadModel("../../../../3DModel/spot/spot_triangulated_good.obj")
    let baseColorFile = "../../../../3DModel/spot/spot_texture.png"

    let texture = TextureFromFile(baseColorFile)
    let cam = new Camera(Point(2,15,-35),Vector(-0.1, -0.5, 1), 90.0, 0.5, 1000.0, 1024, 768)
    let degree = 20.0
    let mutable current = 60.0

    let mutable key = 0
    while key <> int 'q' do
        let trans = Matrix4x4.MakeScaleMatrix(5,5,5) * Matrix4x4.MakeRotationMatrix(0,current,0)

        let vs = car.Vertexs |> Array.map (fun v -> v*trans)
        let idxs = car.Faces |> Array.map (fun (i,_,_) -> i)
        let uvidxs = car.Faces |> Array.map (fun (_,i,_) -> i)
        let uvs = car.TextureCoords

        let screen = new Screen(1024, 768)

        let rand = System.Random(10)
        let color = Color(rand.Next(255), rand.Next(255), rand.Next(255))
        let light = AmbientLight (Ambient_Light(Color(60,60,60,1)))
        let dirLight = DirectionLight (Direction_Light(Color(), Color(200,200,200,1), Color(), Vector(0,-30,1)))

        PipelineDraw screen cam [|light;dirLight|] color (Point()) vs car.Faces uvs car.VertexNormals texture

        //let mat = new Mat(Size(1024, 768), MatType.CV_8UC3)
        //let indexer = mat.GetGenericIndexer<Vec3b>()
        //let (w,h) = screen.Size()
        //for y in 0..h-1 do
        //    for x in 0..w-1 do
        //        let c = screen.Pixel(x,y)
        //        let vec = Vec3b(byte c.b, byte c.g, byte c.r)
        //        indexer[y,x] <- vec
        //Cv2.ImShow("Manga", mat)
        //key <- Cv2.WaitKey()
        //current <- current  + degree


