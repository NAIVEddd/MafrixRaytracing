module Engine.Core.Shapes.Box
open Engine.Core.Shapes.Rect
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Point
open Engine.Core.Ray
open System

type Box =
    struct
        val rect_list: IHitable array
        val pmin : Vector
        val pmax : Vector
        val aabb : AABB
        new(p0:Vector, p1:Vector, m:IMaterial) =
            {
                pmin = p0;
                pmax = p1;
                rect_list = [|  xy_rect(p0.x, p1.x, p0.y, p1.y, p1.z, m);
                                flip_normals(xy_rect(p0.x, p1.x, p0.y, p1.y, p0.z, m));
                                xz_rect(p0.x, p1.x, p0.z, p1.z, p1.y, m);
                                flip_normals(xz_rect(p0.x, p1.x, p0.z, p1.z, p0.y, m));
                                yz_rect(p0.y, p1.y, p0.z, p1.z, p1.x, m);
                                flip_normals(yz_rect(p0.y, p1.y, p0.z, p1.z, p0.x, m));
                |];
                aabb = AABB(p0, p1)
            }
        member this.ShadowHit(ray:Ray) = false, 0.0
        member this.Hit(r:Ray, tMin:float, tMax:float) =
            if this.aabb.hit(r,tMin,tMax) then
                this.rect_list |> Array.map(fun i -> i.Hit(r,tMin,tMax)) |> Array.minBy(fun hitrecord -> (
                    if hitrecord.bHit then hitrecord.t else tMax))
            else
                HitRecord.Nothing
        interface IHitable with
            member this.BoundBox(t0:float,t1:float) = true, AABB(this.pmin,this.pmax)
            member this.ShadowHit(ray:Ray) = this.ShadowHit(ray)
            member this.Hit(r:Ray, tMin:float, tMax:float) = this.Hit(r,tMin,tMax)
    end

type Translate =
    struct
        val ptr: IHitable
        val offset: Vector
        new(p:IHitable, displacement:Vector) = {ptr=p;offset=displacement;}
        member this.ShadowHit(ray:Ray) = false, 0.0
        member this.Hit(r:Ray, tMin:float, tMax:float) =
            let moved_ray = Ray(r.Origin() - this.offset, r.Direction())
            let hitRec = this.ptr.Hit(moved_ray, tMin, tMax)
            if hitRec.bHit then
                let p = hitRec.p + this.offset
                HitRecord(true, hitRec.t, p, hitRec.normal, hitRec.hitRay, hitRec.material)
            else
                hitRec
        interface IHitable with
            member this.BoundBox(t0:float,t1:float) =
                let b, box = this.ptr.BoundBox(t0,t1)
                if b then
                    true, AABB(box.min + this.offset, box.max + this.offset)
                else
                    false, AABB(box.min, box.max)
            member this.ShadowHit(ray:Ray) = this.ShadowHit(ray)
            member this.Hit(r:Ray, tMin:float, tMax:float) = this.Hit(r,tMin,tMax)
    end

type Rotate_y =
    struct
        val sin_theta:float
        val cos_theta:float
        val hasbox:bool
        val aabb:AABB
        val ptr:IHitable
        new(p:IHitable, angle:float) =
            let radians = (Math.PI / 180.) * angle
            let _, box = p.BoundBox(0,1)
            let pmin = Array.create 3 Double.PositiveInfinity
            let pmax = Array.create 3 Double.NegativeInfinity
            for i in 0..2 do
                for j in 0..2 do
                    for k in 0..2 do
                        let x = (float i) * box.max.x + float (1-i)*box.min.x
                        let y = (float j) * box.max.y + float (1-j)*box.min.y
                        let z = (float k) * box.max.z + float (1-k)*box.min.z
                        let newx = Math.Cos(radians)*x + Math.Sin(radians)*z
                        let newz = -Math.Sin(radians)*x + Math.Cos(radians)*z
                        let tester = Vector(newx,y,newz)
                        for c in 0..2 do
                            if tester[c] > pmax[c] then
                                pmax[c] <- tester[c]
                            if tester[c] < pmin[c] then
                                pmin[c] <- tester[c]
            let boxmin = Vector(pmin[0],pmin[1],pmin[2])
            let boxmax = Vector(pmax[0],pmax[1],pmax[2])
            {
                sin_theta = Math.Sin(radians);
                cos_theta = Math.Cos(radians);
                hasbox = false;
                aabb = AABB(boxmin,boxmax)
                ptr = p;
            }
        member this.ShadowHit(ray:Ray) = false, 0.0
        member this.Hit(r:Ray, tMin:float, tMax:float) =
            let origin = r.Origin()
            let direction = r.Direction()
            let O_x = this.cos_theta * origin[0] - this.sin_theta * origin[2]
            let O_z = this.sin_theta * origin[0] + this.cos_theta * origin[2]
            let D_x = this.cos_theta * direction[0] - this.sin_theta * direction[2]
            let D_z = this.sin_theta * direction[0] + this.cos_theta * direction[2]
            let new_origin = Point(O_x, origin.y, O_z)
            let new_direct = Vector(D_x, direction.y, D_z)
            let rotated_ray = Ray(new_origin, new_direct)
            let hitRec = this.ptr.Hit(rotated_ray, tMin, tMax)
            if hitRec.bHit then
                let p = hitRec.p
                let normal = hitRec.normal
                let p_x = this.cos_theta * p[0] + this.sin_theta * p[2]
                let p_z = -this.sin_theta * p[0] + this.cos_theta * p[2]
                let n_x = this.cos_theta * normal[0] + this.sin_theta * normal[2]
                let n_z = -this.sin_theta * normal[0] + this.cos_theta * normal[2]
                let new_p = Point(p_x, p.y, p_z)
                let new_normal = Vector(n_x, normal.y, n_z)
                HitRecord(true, hitRec.t, new_p, new_normal, hitRec.hitRay, hitRec.material)
            else
                hitRec
        interface IHitable with
            member this.BoundBox(t0:float,t1:float) = true, this.aabb
            member this.ShadowHit(ray:Ray) = this.ShadowHit(ray)
            member this.Hit(r:Ray, tMin:float, tMax:float) = this.Hit(r,tMin,tMax)
    end