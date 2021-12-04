module DrawWireBox

open OpenCvSharp
open Engine.Core.Point
open Engine.Core.Camera
open Engine.Core.Pipeline


let DrawWireframeToScreen (screen:OpenCvSharp.Mat) (color:OpenCvSharp.Scalar) (cam:Camera) (pos:Point) (vs:Point[]) (idxs:Indexer[]) =
    let worldVS = LocalToWorld pos vs
    let frontIdxs = RemoveBackfaces cam worldVS idxs
    printfn "filted idx:%A\n" frontIdxs
    let screenVS = worldVS |>
                    WorldToCamera cam |>
                    CameraToPerspecctive cam |>
                    PerspectiveToScreen cam
    for idx in frontIdxs do
        let p1 = screenVS[idx.i]
        let p2 = screenVS[idx.j]
        let p3 = screenVS[idx.k]
        let cvp1 = OpenCvSharp.Point(p1.x, p1.y)
        let cvp2 = OpenCvSharp.Point(p2.x, p2.y)
        let cvp3 = OpenCvSharp.Point(p3.x, p3.y)
        screen.Line(cvp1, cvp2, color)
        screen.Line(cvp1, cvp3, color)
        screen.Line(cvp2, cvp3, color)

let DrawSimpleBox() =
    let cam = new Camera(Point(10,0,-25),Vector(0, 0, 1), 90.0, 0.5, 1000.0, 1024, 768)
    let vs = [| Point(5, 5, 5); Point(-5, 5, 5); Point(-5, 5, -5); Point(5, 5, -5);
                Point(5, -5, 5); Point(-5, -5, 5); Point(-5, -5, -5); Point(5, -5, -5)|]

    let idxs = [|Indexer(2,1,0);Indexer(3,2,0);Indexer(4,7,0);Indexer(7,3,0);
                Indexer(6,7,4);Indexer(5,6,4);Indexer(2,6,1);Indexer(6,5,1);
                Indexer(7,6,3);Indexer(6,2,3);Indexer(5,4,0);Indexer(1,5,0)|]

    let mat = new Mat(Size(1024, 768), MatType.CV_8UC3)

    let color = Scalar(255, 255, 255)
    DrawWireframeToScreen mat color cam (Point()) vs idxs

    Cv2.ImShow("Manga", mat)

    Cv2.WaitKey() |>ignore
