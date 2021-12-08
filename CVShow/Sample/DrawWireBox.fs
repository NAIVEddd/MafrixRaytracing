module DrawWireBox

open OpenCvSharp
open Engine.Core.Color
open Engine.Core.Light
open Engine.Core.Point
open Engine.Core.Camera
open Engine.Core.Pipeline

let DrawUpTrangle (cam:Camera, _p1:Point, _p2:Point, _p3:Point) =
    let minX,minY,maxX,maxY = int (max (min _p1.x _p2.x) 0.0),
                              int (max _p1.y 0.0),
                              int (min (max _p1.x _p3.x) (double (cam.width-1))),
                              int (min _p2.y (double (cam.height-1)))
    
    [|
    for y in minY..maxY do
        let theta = double (y- int _p1.y)/ double (_p2.y - _p1.y)
        let left = int (_p1.x + (_p2.x-_p1.x)*theta + 0.5)
        let right = int (_p1.x + (_p3.x-_p1.x)*theta + 0.5)
        for x in left..right do
            if x <= maxX && x >= minX then
                yield x,y
    |]

let DrawDownTrangle (cam:Camera, _p1:Point, _p2:Point, _p3:Point) =
    let minX,minY,maxX,maxY = int (max (min _p1.x _p3.x) 0.0),
                              int (max _p1.y 0.0),
                              int (min (max _p2.x _p3.x) (double (cam.width-1))),
                              int (min _p3.y (double (cam.height - 1)))

    [|
    for y in minY..maxY do
        let theta = double (y-minY)/ double (int _p3.y-minY)
        let left = int (_p1.x + (_p3.x-_p1.x)*theta + 0.5)
        let right = int (_p2.x + (_p3.x-_p2.x)*theta + 0.5)
        for x in left..right do
            if x <= maxX && x >= minX then
                yield x,y
    |]

let DrawTrangle (cam:Camera, _p1:Point, _p2:Point, _p3:Point) =
    let (p1,p2,p3) =
        if _p1.y <= _p2.y && _p1.y <= _p3.y then
            if _p2.y <= _p3.y then
                (_p1,_p2,_p3)
            else
                (_p1,_p3,_p2)
        else if _p2.y <= _p1.y && _p2.y <= _p3.y then
            if _p1.y <= _p3.y then
                (_p2,_p1,_p3)
            else
                (_p2,_p3,_p1)
        else
            if _p1.y <= _p2.y then
                (_p3,_p1,_p2)
            else
                (_p3,_p2,_p1)
    if p1.y = p2.y then
        let left,right =
            if p1.x <= p2.x then
                p1,p2
            else
                p2,p1
        DrawDownTrangle (cam,left,right,p3)
    else if p2.y = p3.y then
        let left,right =
            if p2.x <= p3.x then
                p2,p3
            else
                p3,p2
        DrawUpTrangle(cam,p1,left,right)
    else
        let mid =
            let theta = (p1.y - p2.y) / (p1.y - p3.y)
            Point(p1.x + (p3.x-p1.x)*theta,p2.y, p1.z + (p3.z-p1.z) * theta)
        let left, right =
            if mid.x <= p2.x then
                mid,p2
            else
                p2,mid
        Array.append (DrawUpTrangle(cam,p1,left,right)) (DrawDownTrangle(cam,left,right,p3))

let LightRander(cam:Camera, light:Light, objColor:Scalar, vs:Point[], idxs:Indexer[]) =
    
    [|
    for idx in idxs do
        let col =
            match light with
            | AmbientLight ambient-> Scalar(objColor.Val0 * (float ambient.color.r/255.0),
                                    objColor.Val1 * (float ambient.color.g/255.0),
                                    objColor.Val2 * (float ambient.color.b/255.0))
            | DirectionLight direction ->
                let u = vs[idx.j] - vs[idx.i]
                let v = vs[idx.k] - vs[idx.i]
                let normal = u.Cross(v).Normalize
                let dp = normal.Dot(direction.dir.Normalize)
                if dp < 0 then
                    Scalar(-dp * objColor.Val0 * (float direction.diffuse.r/255.0),
                        -dp * objColor.Val1 * (float direction.diffuse.g/255.0),
                        -dp * objColor.Val2 * (float direction.diffuse.b/255.0))
                else
                    Scalar()
            | PointLight point -> Scalar()
        yield col
    |]

let DrawWireframeToScreen (screen:Mat) (color:Scalar) (cam:Camera) (pos:Point) (vs:Point[]) (idxs:Indexer[]) =
    let light = AmbientLight (Ambient_Light(Color(60,60,60,60)))
    let dirLight = DirectionLight (Direction_Light(Color(), Color(200,200,200,0), Color(), Vector(1,-1,1)))
    let worldVS = LocalToWorld pos vs
    let frontIdxs = RemoveBackfaces cam worldVS idxs
    printfn "filted idx:%A\n" frontIdxs
    let colors = LightRander(cam, light, color, worldVS, frontIdxs)
    let colors2 = LightRander(cam, dirLight, color, worldVS, frontIdxs)
    let finColor =
        Array.zip colors colors2 |>
        Array.map (fun (a,b) -> Scalar(a.Val0+b.Val0,a.Val1+b.Val1,a.Val2+b.Val2,a.Val3+b.Val3))
    let screenVS = worldVS |>
                    WorldToCamera cam |>
                    CameraToPerspecctive cam |>
                    PerspectiveToScreen cam
    let rand = System.Random(10)

    let mutable i = 0
    for idx in frontIdxs do
        let p1 = screenVS[idx.i]
        let p2 = screenVS[idx.j]
        let p3 = screenVS[idx.k]
        let points = DrawTrangle(cam,p1,p2,p3)
        let indexer = screen.GetGenericIndexer<Vec3b>()
        let vec = Vec3b(byte (finColor[i].Val2), byte (finColor[i].Val1), byte (finColor[i].Val0))
        for (x,y) in points do
            indexer[y,x] <- vec
        i <- i + 1


let DrawSimpleBox() =
    let cam = new Camera(Point(10,10,-25),Vector(0, 0, 1), 90.0, 0.5, 1000.0, 1024, 768)
    let vs = [| Point(5, 5, 5); Point(-5, 5, 5); Point(-5, 5, -5); Point(5, 5, -5);
                Point(5, -5, 5); Point(-5, -5, 5); Point(-5, -5, -5); Point(5, -5, -5)|]

    let idxs = [|Indexer(2,1,0);Indexer(3,2,0);Indexer(4,7,0);Indexer(7,3,0);
                Indexer(6,7,4);Indexer(5,6,4);Indexer(2,6,1);Indexer(6,5,1);
                Indexer(7,6,3);Indexer(6,2,3);Indexer(5,4,0);Indexer(1,5,0)|]

    let mat = new Mat(Size(1024, 768), MatType.CV_8UC3)

    let rand = System.Random(10)
    let color = Scalar(rand.Next(255), rand.Next(255), rand.Next(255))
    DrawWireframeToScreen mat color cam (Point()) vs idxs

    Cv2.ImShow("Manga", mat)

    Cv2.WaitKey() |>ignore
