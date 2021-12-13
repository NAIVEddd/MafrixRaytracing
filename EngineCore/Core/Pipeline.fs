module Engine.Core.Pipeline
open Point
open Color
open Light
open Camera
open RenderTarget
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

let DrawUpTrangle (cam:Camera, _p1:Point, _p2:Point, _p3:Point) =
    let minX,minY,maxX,maxY = int (max (min _p1.x _p2.x) 0.0),
                              int (max _p1.y 0.0),
                              int (min (max _p1.x _p3.x) (double (cam.width-1))),
                              int (min _p2.y (double (cam.height-1)))
    
    [|
    for y in minY..maxY do
        let theta = (double y- _p1.y)/ (_p2.y - _p1.y)
        let left = int (_p1.x + (_p2.x-_p1.x)*theta + 0.5)
        let right = int (_p1.x + (_p3.x-_p1.x)*theta + 0.5)
        let z_l = _p1.z + (_p2.z-_p1.z)*theta
        let z_r = _p1.z + (_p3.z-_p1.z)*theta
        for x in left..right do
            if x <= maxX && x >= minX then
                let z = z_l + (z_r-z_l)*(double (x-left)/ double(right-left))
                yield x,y,z
    |]

let DrawDownTrangle (cam:Camera, _p1:Point, _p2:Point, _p3:Point) =
    let minX,minY,maxX,maxY = int (max (min _p1.x _p3.x) 0.0),
                              int (max _p1.y 0.0),
                              int (min (max _p2.x _p3.x) (double (cam.width-1))),
                              int (min _p3.y (double (cam.height - 1)))

    [|
    for y in minY..maxY do
        let theta = (double y- _p1.y)/ (_p3.y-_p1.y)
        let left = int (_p1.x + (_p3.x-_p1.x)*theta + 0.5)
        let right = int (_p2.x + (_p3.x-_p2.x)*theta + 0.5)
        let z_l = _p1.z + (_p3.z-_p1.z)*theta
        let z_r = _p2.z + (_p3.z-_p2.z)*theta
        for x in left..right do
            if x <= maxX && x >= minX then
                let z = z_l + (z_r-z_l)*(double (x-left)/ double(right-left))
                yield x,y,z
    |]

// Deprecated
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

let ComputeBarycentric2D(x:double, y:double, v:Point[]) =
    let alpha = (x*(v[1].y - v[2].y) + (v[2].x - v[1].x)*y + v[1].x*v[2].y - v[2].x*v[1].y) / (v[0].x*(v[1].y - v[2].y) + (v[2].x - v[1].x)*v[0].y + v[1].x*v[2].y - v[2].x*v[1].y)
    let beta = (x*(v[2].y - v[0].y) + (v[0].x - v[2].x)*y + v[2].x*v[0].y - v[0].x*v[2].y) / (v[1].x*(v[2].y - v[0].y) + (v[0].x - v[2].x)*v[1].y + v[2].x*v[0].y - v[0].x*v[2].y)
    let gamma = (x*(v[0].y - v[1].y) + (v[1].x - v[0].x)*y + v[0].x*v[1].y - v[1].x*v[0].y) / (v[2].x*(v[0].y - v[1].y) + (v[1].x - v[0].x)*v[2].y + v[0].x*v[1].y - v[1].x*v[0].y)
    (alpha,beta,gamma)

// new version of DrawTrangle, rename in next update
let DrawTrangle_new (cam:Camera) (points:Point[]) (uvs:Point2D[]) =
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
                yield x,y,zp,uv
    |]

//
// pos,vs,idxs is 3D Object property
let PipelineDraw (screen:Screen) (cam:Camera) (lights:Light[]) (color:Color) (pos:Point) (vs:Point[]) (idxs:Indexer[]) =
    screen.Reset()
    let worldVS = LocalToWorld pos vs
    let frontIdxs = RemoveBackfaces cam worldVS idxs
    
    let finColor = lights |> Array.map (fun light -> light.Render(cam, color, worldVS, frontIdxs)) |>
                    Array.reduce (fun a b ->
                        assert(a.Length = b.Length)
                        Array.zip a b |> Array.map (fun (l,r) -> l+r))
    let screenVS = worldVS |>
                    WorldToCamera cam |>
                    CameraToPerspective cam |>
                    PerspectiveToScreen cam

    let mutable i = 0
    for idx in frontIdxs do
        let p1 = screenVS[idx.i]
        let p2 = screenVS[idx.j]
        let p3 = screenVS[idx.k]
        let points = DrawTrangle(cam,p1,p2,p3)
        for (x,y,z) in points do
            screen[x,y] <- finColor[i],z
        i <- i + 1