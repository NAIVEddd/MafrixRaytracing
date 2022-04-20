module Engine.Core.Accels.BvhNode
open Engine.Core.Point
open Engine.Core.Ray
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Interfaces.IHitable
open System

let SurroundingBox(box0:AABB, box1:AABB) =
    let small = Vector(min box0.min.x box1.min.x, min box0.min.y box1.min.y, min box0.min.z box1.min.z)
    let big = Vector(max box0.max.x box1.max.x, max box0.max.y box1.max.y, max box0.max.z box1.max.z)
    AABB(small,big)

type BvhNode =
    val left:IHitable
    val right:IHitable
    val box:AABB
    new(l,r,b) = {left=l;right=r;box=b}
    member this.BoundBox(t0:float,t1:float) =
        true, this.box
    member this.Hit(r:Ray, tMin:float, tMax:float) =
        let bhit = this.box.hit(r,tMin,tMax)
        if bhit = true then
            let record_left = this.left.Hit(r,tMin,tMax)
            let record_right = this.right.Hit(r,tMin,tMax)
            if record_left.bHit && record_right.bHit then
                if record_left.t < record_right.t then
                    record_left
                else
                    record_right
            elif record_left.bHit then
                record_left
            elif record_right.bHit then
                record_right
            else
                HitRecord.Nothing
        else
            HitRecord.Nothing
    interface IHitable with
        member this.BoundBox(t0:float,t1:float) = this.BoundBox(t0,t1)
        member this.Hit(r:Ray, tMin:float, tMax:float) = this.Hit(r,tMin,tMax)
        member this.ShadowHit(r:Ray) =
            let bLeft, fLeft = this.left.ShadowHit(r)
            let bRight, fRight = this.right.ShadowHit(r)
            bLeft, fRight   // BUG Imp

let rec BuildBvhNode(l:IHitable array, n:int, t0:float, t1:float) =
    let axis = int (3.0 * Random.Shared.NextDouble())
    let ls =
        if axis = 0 then
            l |> Array.sortBy (fun x ->
                let _,box = x.BoundBox(0,0)
                box.min.x)
        elif axis = 1 then
            l |> Array.sortBy (fun x ->
                let _,box = x.BoundBox(0,0)
                box.min.y)
        else
            l |> Array.sortBy (fun x ->
                let _,box = x.BoundBox(0,0)
                box.min.z)
    let left,right =
        if n = 1 then
            l[0],l[0]
        elif n = 2 then
            l[0],l[1]
        else
            let half = n / 2
            let l0,l1 = Array.take half ls, Array.skip half ls
            BuildBvhNode(l0,half,t0,t1),BuildBvhNode(l1,n-half,t0,t1)
    let _,bLeft = left.BoundBox(t0,t1)
    let _,bRight = right.BoundBox(t0,t1)
    let box = SurroundingBox(bLeft,bRight)
    BvhNode(left,right,box)