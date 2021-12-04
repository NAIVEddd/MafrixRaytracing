﻿module Engine.Core.Camera
open System
open Point
open Transformation

type UVN(dir:Vector, ?up:Vector) =
    let mutable u = Vector()
    let mutable v = Vector()
    let mutable n = Vector()
    do
        n <- dir.Normalize
        v <- (defaultArg up (Vector(0,1,0))).Normalize
        u <- v.Cross(n).Normalize
        v <- n.Cross(u).Normalize
    new(pos:Point, target:Point, ?up:Vector) = UVN(target-pos, defaultArg up (Vector(0,1,0)))
    new(phi:double, theta:double, ?up:Vector) =
        let p = phi * (Math.PI / 180.0)
        let t = theta * (Math.PI / 180.0)
        let sin_phi = sin p
        let cos_phi = cos p
        let sin_theta = sin t
        let cos_theta = cos t
        let x = -1.0 * sin_phi * sin_theta
        let y = cos_phi
        let z = sin_phi * cos_theta
        UVN(Vector(x,y,z), defaultArg up (Vector(0,1,0)))
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
    let viewDistancce = (0.5 * viewplane.x) * tan(fov * Math.PI / 360.0)

    member this.Position = camPosition
    member this.width = width
    member this.height = height
    member this.GetUVNTransMatrix() =
        let mtransi = Matrix4x4.MakeDisplacementInvMatrix(camPosition.x, camPosition.y, camPosition.z)
        let muvni = uvn.RotateMatrix
        mtransi * muvni