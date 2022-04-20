module Engine.Core.Shapes.Trangle
open Microsoft.FSharp.Core
open Microsoft.FSharp.Collections
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Point
open Engine.Core.Ray

type VertexsRef =
    struct
        val vertexs:Point array
        val normals:Vector array
        val textureCoords:Point2D array
        new(vs, nms, texs) = {vertexs=vs;normals=nms;textureCoords=texs}
    end

type Trangle =
    struct
        val vertexs:VertexsRef
        val vertexIndex:Indexer
        val normalIndex:Indexer
        val textureIndex:Indexer
        //.val faceNormal:Vector
        val material: IMaterial

        new(vertex,vsidx,nmidx,texidx, material) =
            {
                vertexs=vertex;
                vertexIndex=vsidx;
                normalIndex=nmidx;
                textureIndex=texidx;
                //faceNormal=
                material=material;
            }
        member this.Intersect(ray:Ray, v0:Point, v1:Point, v2:Point) =
            let e1 = v1-v0
            let e2 = v2-v1
            let s1 = ray.Direction().Cross(e2)
            let divisor = s1.Dot(e1)
            if divisor = 0. then
                false, 0.,0.,0.
            else
                let invDivisor = 1./divisor
                let d = ray.Origin() - v0
                let b1 = d.Dot(s1) * invDivisor
                if b1 <0. || b1 > 1. then
                    false, 0.,0., 0.
                else
                    let s2 = d.Cross(e1)
                    let b2 = ray.Direction().Dot(s2) * invDivisor
                    if b2 < 0. || b2 > 1. then
                        false, 0., 0., 0.
                    else
                        let t = e2.Dot(s2) * invDivisor
                        if t < 0.00001 then
                            false, 0.,0.,0.
                        else
                            let b0 = 1. - b1 - b2
                            if b0 < 0. || b0 > 1. then
                                false, 0., 0., 0.
                            else
                                true, t, b1, b2
        member this.ShadowHit(ray:Ray) = (this:>IHitable).ShadowHit(ray)
        member this.Hit(r:Ray, tMin:float, tMax:float) = (this:>IHitable).Hit(r,tMin,tMax)
        interface IHitable with
            member this.BoundBox(t0:float,t1:float) =
                let pmin = Vector()
                let pmax = Vector(1,1,1)
                true, AABB(pmin,pmax)
            member this.ShadowHit(ray:Ray) =
                let record = this.Hit(ray, 0.00001, 99999999.)
                if record.bHit then
                    true, record.t
                else
                    false, 0.0
            member this.Hit(r:Ray, tMin:float, tMax:float) =
                let v0 = this.vertexs.vertexs[this.vertexIndex.i]
                let v1 = this.vertexs.vertexs[this.vertexIndex.j]
                let v2 = this.vertexs.vertexs[this.vertexIndex.k]
                let bHit, t, beta, gamma = this.Intersect(r, v0, v1, v2)
                if bHit then
                    let p = r.PointAtParameter(t)
                    let alpha = 1. - beta - gamma
                    assert(alpha >= 0. && alpha <= 1.)
                    let nm0 = this.vertexs.normals[this.normalIndex.i]
                    let nm1 = this.vertexs.normals[this.normalIndex.j]
                    let nm2 = this.vertexs.normals[this.normalIndex.k]
                    let normal = (alpha * nm0 + beta * nm1 + gamma * nm2).Normalize
                    HitRecord(true, t, p, normal, r, Some this.material)
                else
                    HitRecord.Nothing
    end