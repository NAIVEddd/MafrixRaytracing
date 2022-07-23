module Engine.Core.Camera
open System
open Point
open Ray
open Transformation
open Interfaces.ICamera

type UVN(dir:Vector, ?up:Vector) =
    let mutable u = Vector()
    let mutable v = Vector()
    let mutable n = Vector()
    do
        assert(dir.Length <> 0.0)
        n <- dir.Normalize
        v <- (defaultArg up (Vector(0,1,0))).Normalize
        u <- n.Cross(v).Normalize
        v <- u.Cross(n).Normalize
    new(pos:Point, target:Point, ?up:Vector) = UVN(target-pos, defaultArg up (Vector(0,1,0)))
    // phi: degree to lookup, 0 is forward; theta: degree to look right, 0 is forward.
    new(phi:double, theta:double, ?up:Vector) =
        let p = phi * (Math.PI / 180.0)
        let t = theta * (Math.PI / 180.0)
        let sin_phi = sin p
        let cos_phi = cos p
        let sin_theta = sin t
        let cos_theta = cos t
        let x = cos_phi * sin_theta
        let y = sin_phi
        let z = cos_phi * cos_theta
        UVN(Vector(x,y,z), defaultArg up (Vector(0,1,0)))

    member this.U = u
    member this.V = v
    member this.N = n

    member this.RotateMatrix =
        Matrix4x4([|
            u.x; v.x; n.x; 0.0;
            u.y; v.y; n.y; 0.0;
            u.z; v.z; n.z; 0.0;
            0.0; 0.0; 0.0; 1.0;|])

type Camera(pos:Point, dir:Vector, fov:double, nearClip:double, farClip:double, width:int, height:int) =
    let mutable camPosition = pos
    let uvn = UVN(dir)
    let viewportCenter = Point2D(double ((width-1)/2), double ((height-1)/2))
    let aspectRatio = double width / double height
    let viewplane = Point2D(2.0, 2.0/aspectRatio)
    let viewDistance = (0.5 * double width) * tan(fov * Math.PI / 360.0)
    let mutable upperLeftCorner = Point()
    let mutable horizontal = Vector()
    let mutable verticle = Vector()

    do
        upperLeftCorner <- pos - 0.5 * (float width) * uvn.U - 0.5 * (float -height) * uvn.V + viewDistance * uvn.N
        horizontal <- (float width) * uvn.U
        verticle <- (float -height) * uvn.V

    member this.Position = camPosition
    member this.width = width
    member this.height = height
    member this.ViewDistance = viewDistance
    member this.GetUVNTransMatrix() =
        let mtransi = Matrix4x4.MakeDisplacementInvMatrix(camPosition.x, camPosition.y, camPosition.z)
        let muvni = uvn.RotateMatrix
        mtransi * muvni
    member this.GetPerspectiveMatrix() =
        let arr = [|viewDistance; 0; 0; 0;
                    0; viewDistance; 0; 0;
                    0; 0; nearClip + farClip; 1;
                    0; 0; - nearClip*farClip; 0|]
        Matrix4x4(arr) * this.GetOrthogriphicMatrix()
    member this.GetOrthogriphicMatrix() =
        let arr = [|4.0/(double width); 0; 0; 0;
                    0; 4.0/(double height); 0; 0;
                    0; 0; 2.0/(farClip-nearClip); 0;
                    0; 0; 0; 1|]
        Matrix4x4(arr)
    member this.GetRay(x:float, y:float) =
        assert(x >= 0. && x < (float width + 1.))
        assert(y >= 0. && y < (float height + 1.))
        let xx = x / float width
        let yy = y / float height
        let target = upperLeftCorner + xx * horizontal + yy * verticle
        let dir = (target-this.Position).Normalize
        Ray(this.Position, dir)

[<Struct>]
type CameraCoordinate =
    val forward : Vector
    val up : Vector
    val right : Vector
    val down : Vector
    //val up_size = tan(f * Math.PI / 360.) * 2.0
    //val hori_size = up_size * asp
    new(dir:Vector, ?up:Vector) =
        let dir = dir.Normalize
        let _up = (defaultArg up (Vector(0,1,0))).Normalize
        let hori = dir.Cross(_up.Normalize)
        let vert = hori.Cross(dir)
        {
            forward=dir; up=vert;
            right=hori;  down= -vert;
        }
    new(dir:Vector, up:Vector, right:Vector) =
        {
            forward=dir; up=up;
            right=right; down= -up;
        }
    member this.TopLeft(pos:Point, dist:float) =
        pos + dist * this.forward - 0.5 * this.right + 0.5 * this.up

type PinholeCamera =
    val mutable position : Point
    val mutable topleft : Point
    val mutable coord : CameraCoordinate
    val fov : float
    val hori_size : float
    val vert_size : float
    override this.ToString() =
        sprintf "PinholeCamera:\n\tPosition=%A\n\tLookAt=%A\n\tFov=%A\n\tAspectRatio=%A" this.position this.coord.forward this.fov (this.hori_size/this.vert_size)
    new(pos:Point, dir:Vector, fov:float, aspectRatio:float) =
        let tmp = CameraCoordinate(dir)
        // visual plane place in the front 0.5 unit
        let hori = tan(0.5 * fov * Math.PI / 360.)
        let vert = hori / aspectRatio
        let coord = CameraCoordinate(tmp.forward, tmp.up * vert, tmp.right * hori)
        {
            position = pos; coord = coord;
            fov = fov;
            hori_size = hori; vert_size = vert;
            topleft = coord.TopLeft(pos, 0.5);
        }
    member this.GetRay(u:float, v:float) =
        assert(u >= 0. && u <= 1.)
        assert(v >= 0. && v <= 1.)
        let target = this.topleft + u * this.coord.right + v * this.coord.down
        let dir = (target - this.position).Normalize
        Ray(this.position, dir)

    interface ICamera with
        member this.GetRay(u:float, v:float) = this.GetRay(u,v)