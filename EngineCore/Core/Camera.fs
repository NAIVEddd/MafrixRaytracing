module Engine.Core.Camera
open Point
open Transformation

type Camera(view:int) =
    let CamPosition = Point()
    let U = Vector()
    let V = Vector()
    let N = Vector()

    new() = Camera(0)
    member this.GetUVNTransMatrix() =
        let mtransi = Matrix4x4.MakeDisplacementInvMatrix(CamPosition.x, CamPosition.y, CamPosition.z)
        let muvni = Matrix4x4([|
                    U.x; V.x; N.x; 0.0;
                    U.y; V.y; N.y; 0.0;
                    U.z; V.z; N.z; 0.0;
                    0.0; 0.0; 0.0; 1.0;|])
        mtransi * muvni