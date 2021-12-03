open OpenCvSharp
open Engine.Model.Obj
open Engine.Core.Point
open Engine.Core.Transformation


// For more information see https://aka.ms/fsharp-console-apps
//LoadModel("C:\\allFiles\\Game\\LittleGraphicShow\\.vs\\Assets\\Renault12TL\\Renault12TL.obj")

//ImreadModes.Color
let mat = new Mat(Size(1024, 768), MatType.CV_8UC3)
//let mat4 = new Mat(mat)
let indexer = mat.GetGenericIndexer<Vec3b>()
for i in 0..(mat.Width - 1) do
    let vec = Vec3b(byte 0, byte 0, byte 255)
    indexer[200, i] <- vec

Cv2.ImShow("Manga", mat)

Cv2.WaitKey() |>ignore
