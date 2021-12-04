module Engine.Core.Pipeline
open Point
open Camera
open Transformation

let LocalToWorld (pos:Point) (vs:Point[])=
    let mat = Matrix4x4.MakeDisplacementMatrix(pos.x, pos.y, pos.z)
    vs |> Array.map (fun p -> p*mat)

let RemoveBackfaces (cam:Camera) (vs:Point[]) (idxs:Indexer[]) =
    idxs |> Array.filter (fun idx ->
        let u = vs[idx.j] - vs[idx.i]
        let v = vs[idx.k] - vs[idx.i]
        let normal = u.Cross(v)
        let view = cam.Position - vs[idx.i]
        let dp = normal.Dot(view)
        dp > 0.0)

let WorldToCamera (cam:Camera) (vs:Point[]) =
    let camMat = cam.GetUVNTransMatrix()
    vs |> Array.map (fun v -> v * camMat)

let CameraToPerspecctive (cam:Camera) (vs:Point[]) =
    vs |> Array.map (fun v ->
        let z = v.z
        assert(z > 0.0)
        Point(v.x/z, v.y/z, z))

let PerspectiveToScreen (cam:Camera) (vs:Point[]) =
    let alpha = 0.5 * (double cam.width) - 0.5
    let beta = 0.5 * (double cam.height) - 0.5
    vs |> Array.map (fun v ->
        Point(alpha * (1.0+v.x), beta * (1.0 - v.y), v.z))
