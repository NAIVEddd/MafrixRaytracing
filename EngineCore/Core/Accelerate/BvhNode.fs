module Engine.Core.Accels.BvhNode
open Engine.Core.Aggregate
open Engine.Core.Point
open Engine.Core.Ray
open Engine.Core.Shapes.Sphere
open Engine.Core.Interfaces.HitRecord
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Interfaces.IHitable
open System

type BvhNode =
    struct
        val bound : Bound
        val first : int
        val count : int
        new(b,f,c) = {bound=b;first=f;count=c;}
    end
type Bvh =
    struct
        val indices : int array
        val primitives : IHitable array
        val nodes : BvhNode array
        private new(ids,prims,ns) = {indices=ids;primitives=prims;nodes=ns;}
        static member Build(prims:IHitable array) =
            let indices = Array.init prims.Length (fun i -> i)
            let nodes = Array.zeroCreate<BvhNode> (prims.Length * 2 - 1)
            let root = Bvh.InitNode(prims,indices,0,prims.Length)
            nodes[0] <- root
            Bvh.Subdivide(prims,indices,nodes,0)
            Bvh(indices,prims,nodes)

        static member private InitNode(prims:IHitable array, indices:int array, start:int, count:int) =
            let arr = Array.sub indices start count
            let bound =
                arr |> Array.map (fun i -> prims[i].BoundBox()) |>
                    Array.reduce (fun l r -> Bound.Union(l,r))
            BvhNode(bound,start,count)

        static member private LeafNodeCount = 3
        static member private LeftIdx(i) = i * 2 + 1
        static member private RightIdx(i) = i * 2 + 2
        static member private Subdivide(prims:IHitable array, indices:int array, nodes:BvhNode array, i:int) =
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
        member private this.CheckHit(ray:Ray, tMin:float, tMax:float, idx:int) : HitRecord =
            let node = this.nodes[idx]
            let aabb = AABB(node.bound.pMin,node.bound.pMax)
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
                HitRecord.Empty
        member this.Hit(ray:Ray, tMin:float, tMax:float) = this.CheckHit(ray,tMin,tMax,0)
    end