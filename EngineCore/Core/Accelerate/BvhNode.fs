module Engine.Core.Accels.BvhNode
open Engine.Core.Aggregate
open Engine.Core.Point
open Engine.Core.Ray
open Engine.Core.Shapes.Sphere
open Engine.Core.Interfaces.HitRecord
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
    let ls = l |> Array.sortBy (fun x ->
                let _,box = x.BoundBox(0,0)
                box.min[axis])
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

type NewBvhNode =
    struct
        val bound : Bound
        val first : int
        val count : int
        new(b,f,c) = {bound=b;first=f;count=c;}
    end
type Bvh =
    struct
        val indices : int array
        val primitives : INewHitable array
        val nodes : NewBvhNode array
        private new(ids,prims,ns) = {indices=ids;primitives=prims;nodes=ns;}
        static member Build(prims:INewHitable array) =
            let indices = Array.init prims.Length (fun i -> i)
            let nodes = Array.zeroCreate<NewBvhNode> (prims.Length * 2 - 1)
            let root = Bvh.InitNode(prims,indices,0,prims.Length)
            nodes[0] <- root
            Bvh.Subdivide(prims,indices,nodes,0)
            Bvh(indices,prims,nodes)

        static member private InitNode(prims:INewHitable array, indices:int array, start:int, count:int) =
            let arr = Array.sub indices start count
            let bound =
                arr |> Array.map (fun i -> prims[i].BoundBox()) |>
                    Array.reduce (fun l r -> Bound.Union(l,r))
            NewBvhNode(bound,start,count)

        static member private LeafNodeCount = 3
        static member private LeftIdx(i) = i * 2 + 1
        static member private RightIdx(i) = i * 2 + 2
        static member private Subdivide(prims:INewHitable array, indices:int array, nodes:NewBvhNode array, i:int) =
            let node = nodes[i]
            if node.count > Bvh.LeafNodeCount then
                let arr = Array.sub indices node.first node.count
                let axis = node.bound.MaximumExtent()
                arr |> Array.sortInPlaceBy (fun i ->
                            let b = prims[i].BoundBox()
                            let dig = b.Diagnal() * 0.5
                            let p = b.pMin + dig
                            p.[axis])
                Array.blit arr 0 indices node.first node.count
                let leftcount = node.count / 2
                let left = Bvh.InitNode(prims, indices, node.first, leftcount)
                let right = Bvh.InitNode(prims, indices, node.first + leftcount, node.count - leftcount)
                let leftidx = Bvh.LeftIdx(i)
                let rightidx = Bvh.RightIdx(i)
                nodes[leftidx] <- left
                nodes[rightidx] <- right
                Bvh.Subdivide(prims,indices,nodes,leftidx)
                Bvh.Subdivide(prims,indices,nodes,rightidx)
        member private this.CheckHit(ray:Ray, tMin:float, tMax:float, idx:int) : NewHitRecord =
            let node = this.nodes[idx]
            let aabb = NewAABB(node.bound.pMin,node.bound.pMax)
            if aabb.hit(ray,tMin,tMax) then
                if node.count > Bvh.LeafNodeCount then  // have two child
                    let l = this.CheckHit(ray,tMin,tMax,Bvh.LeftIdx(idx))
                    let r = this.CheckHit(ray,tMin,tMax,Bvh.RightIdx(idx))
                    if l.hit && r.hit then
                        if l.t < r.t then l else r
                    elif l.hit then
                        l
                    else
                        r
                else        // leaf node
                    let arr = Array.sub this.indices node.first node.count
                    let self = this
                    arr |> Array.map (fun i ->
                        self.primitives[i].Hit(ray,tMin,tMax)
                    ) |> Array.minBy (fun hit -> if hit.hit then hit.t else tMax)
            else
                NewHitRecord.Empty
        member this.Hit(ray:Ray, tMin:float, tMax:float) = this.CheckHit(ray,tMin,tMax,0)
    end