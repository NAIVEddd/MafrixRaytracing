module Engine.Core.Pipeline
open Point
open Color
open Light
open Camera
open Texture
open RenderTarget
open Transformation

let LocalToWorld (pos:Point) (vs:Point[])=
    let mat = Matrix4x4.MakeDisplacementMatrix(pos.x, pos.y, pos.z)
    vs |> Array.map (fun p -> p*mat)

let RemoveBackfaces (cam:Camera) (vs:Point[]) (idxs:(Indexer*Indexer*Indexer)[]) =
    idxs |> Array.filter (fun (idx,_,_) ->
        let u = vs[idx.j] - vs[idx.i]
        let v = vs[idx.k] - vs[idx.i]
        let normal = u.Cross(v)
        let view = cam.Position - vs[idx.i]
        let dp = normal.Dot(view)
        dp > 0.0)

let WorldToCamera (cam:Camera) (vs:Point[]) =
    let camMat = cam.GetUVNTransMatrix()
    vs |> Array.map (fun v -> v * camMat)

let CameraToPerspective (cam:Camera) (vs:Point[]) =
    let m = cam.GetPerspectiveMatrix()
    vs |> Array.map (fun v ->
        let z = v.z
        assert(z > 0.0)
        v* m)

let PerspectiveToScreen (cam:Camera) (vs:Point[]) =
    let alpha = 0.5 * (double cam.width) - 0.5
    let beta = 0.5 * (double cam.height) - 0.5
    vs |> Array.map (fun v ->
        Point(alpha * (1.0+v.x), beta * (1.0 - v.y), v.z))

let ComputeBarycentric2D(x:double, y:double, v:Point[]) =
    let alpha = (x*(v[1].y - v[2].y) + (v[2].x - v[1].x)*y + v[1].x*v[2].y - v[2].x*v[1].y) / (v[0].x*(v[1].y - v[2].y) + (v[2].x - v[1].x)*v[0].y + v[1].x*v[2].y - v[2].x*v[1].y)
    let beta = (x*(v[2].y - v[0].y) + (v[0].x - v[2].x)*y + v[2].x*v[0].y - v[0].x*v[2].y) / (v[1].x*(v[2].y - v[0].y) + (v[0].x - v[2].x)*v[1].y + v[2].x*v[0].y - v[0].x*v[2].y)
    let gamma = (x*(v[0].y - v[1].y) + (v[1].x - v[0].x)*y + v[0].x*v[1].y - v[1].x*v[0].y) / (v[2].x*(v[0].y - v[1].y) + (v[1].x - v[0].x)*v[2].y + v[0].x*v[1].y - v[1].x*v[0].y)
    (alpha,beta,gamma)

let DrawTrangle (cam:Camera) (points:Point[]) (uvs:Point2D[]) (nms:Vector[]) =
    let minX,maxX,minY,maxY = max (int (points |> Array.minBy (fun e -> e.x)).x) 0,
                              min (int (points |> Array.maxBy (fun e -> e.x)).x) (cam.width-1),
                              max (int (points |> Array.minBy (fun e -> e.y)).y) 0,
                              min (int (points |> Array.maxBy (fun e -> e.y)).y) (cam.height-1)
    let points2d = points |> Array.map (fun p -> Vector(p.x,p.y,1.0))
    let f0 = points2d[1].Cross(points2d[0])
    let f1 = points2d[2].Cross(points2d[1])
    let f2 = points2d[0].Cross(points2d[2])
    [|
    for y in minY..maxY do
        for x in minX..maxX do
            let alpha,beta,gamma = ComputeBarycentric2D(x,y,points)
            if alpha >= 0 && beta >= 0 && gamma >= 0 &&
                alpha <= 1 && beta <= 1 && gamma <= 1 then
                let zp = (alpha * points[0].z + beta * points[1].z + gamma * points[2].z)
                let uv = alpha * uvs[0] + beta * uvs[1] + gamma * uvs[2]
                let normal = alpha*nms[0] + beta*nms[1] + gamma*nms[2]
                yield x,y,zp,uv,normal
    |]

//
// pos,vs,idxs is 3D Object property
let PipelineDraw (screen:Screen) (cam:Camera) (lights:Light[]) (color:Color) (pos:Point) (vs:Point[]) (idxs:(Indexer*Indexer*Indexer)[]) (uvs:Point2D[]) (nms:Vector[]) (texture:Texture) =
    screen.Reset()
    let worldVS = LocalToWorld pos vs
    let vsw = vs |> (fun (vs) ->
            let mat = Matrix4x4.MakeDisplacementMatrix(pos.x, pos.y, pos.z)
            vs |> Array.map (fun p -> mat.Multiply(p)))
    let frontIdxs = RemoveBackfaces cam worldVS idxs
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
    for (idx,uvidx,nmidx) in frontIdxs do
        let p1 = screenVS[idx.i]
        let p2 = screenVS[idx.j]
        let p3 = screenVS[idx.k]
        let uv1 = uvs[uvidx.i]
        let uv2 = uvs[uvidx.j]
        let uv3 = uvs[uvidx.k]
        let nm1 = nms[nmidx.i]
        let nm2 = nms[nmidx.j]
        let nm3 = nms[nmidx.k]
        let points = DrawTrangle cam [|p1;p2;p3|] [|uv1;uv2;uv3|] [|nm1;nm2;nm3|]
        points |> Array.map (fun (x,y,z,uv,normal) ->
            let color = texture.Sample(uv.x, uv.y)
            let fin = lights[1].Sample_Li(cam, Point(x,y,z), normal)
            let final = fin * color
            screen[x,y] <- final,z) |> ignore

        i <- i + 1