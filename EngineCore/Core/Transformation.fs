module Engine.Core.Transformation

open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Collections
open System
open Point

type Matrix4x4(numList:double[]) =
    do
        assert(numList.Length = 16)

    new() = Matrix4x4([|for i = 1 to 16 do 0.0|])

    member this.GetRow(idx:int) =
        let iter = [| for i in (idx * 4) .. (idx*4 + 3) do i |]
        [|for i in iter do numList[i] |]
    member this.GetColumn(idx:int) =
        let idxs = [|for i in 0..3 do idx + i*4 |]
        [| for i in idxs do numList[i] |]
    member this.Combine (other:Matrix4x4) =
        this * other
    member this.Transpose() =
        let t = [|for i in 0..3 do
                    this.GetColumn i |] |> Array.concat
        Matrix4x4(t)
    
    override this.ToString() =
        sprintf "\n%A\n%A\n%A\n%A\n" (this.GetRow 0) (this.GetRow 1) (this.GetRow 2) (this.GetRow 3)

    member this.Multiply(l:Point) =
        let pl = [|l.x; l.y; l.z; 1|]
        let res = [|
            for i in 0..3 do
                Array.zip pl (this.GetColumn i) |> Array.map (fun (f, b) -> f*b) |> Array.sum |]
        Point(res[0], res[1], res[2]), res[3]
    static member (*) (l:Matrix4x4, r:Matrix4x4) =
        let res = [|
            for i in 0..3 do
                for j in 0..3 do
                    Array.zip (l.GetRow i) (r.GetColumn j) |> Array.map (fun (f,b) -> f*b) |> Array.sum |]
        Matrix4x4(res)
    static member (*) (l:Point, r:Matrix4x4) =
        let pl = [|l.x; l.y; l.z; 1|]
        let res = [|
            for i in 0..3 do
                Array.zip pl (r.GetColumn i) |> Array.map (fun (f, b) -> f*b) |> Array.sum |]
        match res[3] with
        | 1.0 -> Point(res[0], res[1], res[2])
        | 0.0 -> assert(false)
                 Point()
        | w -> Point(res[0]/w, res[1]/w, res[2]/w)
    static member (*) (l:Vector, r:Matrix4x4) =
        let vl = [|l.x; l.y; l.z; 0|]
        let res = [|
            for i in 0..3 do
                Array.zip vl (r.GetColumn i) |> Array.map (fun (f, b) -> f*b) |> Array.sum |]
        Vector(res[0], res[1], res[2])

    static member MakeIdentity() =
        let a = [| 1.0; 0.0; 0.0; 0.0;
                   0.0; 1.0; 0.0; 0.0;
                   0.0; 0.0; 1.0; 0.0;
                   0.0; 0.0; 0.0; 1.0|]
        Matrix4x4(a)

    static member MakeDisplacementMatrix(x, y, z) =
        let a = [| 1.0; 0.0; 0.0; 0.0;
                   0.0; 1.0; 0.0; 0.0;
                   0.0; 0.0; 1.0; 0.0;
                   x; y; z; 1.0|]
        Matrix4x4(a)
    static member MakeDisplacementInvMatrix(x, y, z) =
        let a = [| 1.0; 0.0; 0.0; 0.0;
                   0.0; 1.0; 0.0; 0.0;
                   0.0; 0.0; 1.0; 0.0;
                   -x; -y; -z; 1.0|]
        Matrix4x4(a)
    static member MakeRotationXMatrix(theta) =
        let deg_theta = -theta * Math.PI / 180.0
        let cos_theta = cos deg_theta
        let sin_theta = sin deg_theta
        let a = [|  1.0; 0.0; 0.0; 0.0;
                    0.0; cos_theta; -sin_theta; 0.0;
                    0.0; sin_theta; -cos_theta; 0.0;
                    0.0; 0.0; 0.0; 1.0|]
        Matrix4x4(a)
    static member MakeRotationXInvMatrix(theta) =
        Matrix4x4.MakeRotationXMatrix(-theta)
    static member MakeRotationYMatrix(theta) =
        let deg_theta = theta * Math.PI / 180.0
        let cos_theta = cos deg_theta
        let sin_theta = sin deg_theta
        let a = [|  cos_theta; 0.0; sin_theta; 0.0;
                    0.0; 1.0; 0.0; 0.0;
                    -sin_theta; 0.0; cos_theta; 0.0;
                    0.0; 0.0; 0.0; 1.0|]
        Matrix4x4(a)
    static member MakeRotationYInvMatrix(theta) =
        Matrix4x4.MakeRotationYMatrix(-theta)
    static member MakeRotationZMatrix(theta) =
        let deg_theta = -theta * Math.PI / 180.0
        let cos_theta = cos deg_theta
        let sin_theta = sin deg_theta
        let a = [|  cos_theta; -sin_theta; 0.0; 0.0;
                    sin_theta; cos_theta; 0.0; 0.0;
                    0.0; 0.0; 1.0; 0.0;
                    0.0; 0.0; 0.0; 1.0|]
        Matrix4x4(a)
    static member MakeRotationZInvMatrix(theta) =
        Matrix4x4.MakeRotationZMatrix(-theta)
    static member MakeRotationMatrix(roll, pitch, yaw) =
        Matrix4x4.MakeRotationXMatrix(roll) *
        Matrix4x4.MakeRotationYMatrix(pitch) *
        Matrix4x4.MakeRotationZMatrix(yaw)
    static member MakeRotationInvMatrix(roll, pitch, yaw) =
        Matrix4x4.MakeRotationZInvMatrix(yaw) *
        Matrix4x4.MakeRotationYInvMatrix(pitch) *
        Matrix4x4.MakeRotationXInvMatrix(roll)
    static member MakeScaleMatrix(sx, sy, sz) =
        let a = [|  sx; 0.0; 0.0; 0.0;
                    0.0; sy; 0.0; 0.0;
                    0.0; 0.0; sz; 0.0;
                    0.0; 0.0; 0.0; 1.0|]
        Matrix4x4(a)
    static member MakeScaleInvMatrix(sx, sy, sz) =
        assert(sx >0. && sy >0. && sz >0.)
        let a = [|  1.0/sx; 0.0; 0.0; 0.0;
                    0.0; 1.0/sy; 0.0; 0.0;
                    0.0; 0.0; 1.0/sz; 0.0;
                    0.0; 0.0; 0.0; 1.0|]
        Matrix4x4(a)