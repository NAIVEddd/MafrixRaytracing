module Engine.Core.Accels.BvhAccel
open Engine.Core.Interfaces.IHitable
open Engine.Core.Aggregate
open Engine.Core.Point

type BVHBuildNode =
    struct
        val mutable left:option<BVHBuildNode>
        val mutable right:option<BVHBuildNode>
        val mutable bound:Bound
        val mutable splitAxis:int
        new(n1,n2,axis) =
            {
                left=n1
                right=n2
                bound=Bound.Union(n1.Value.bound,n2.Value.bound)
                splitAxis=axis
            }
    end

type BVHPrimitiveInfo =
    struct
        val primitiveNumber:int
        val bound:Bound
        val centroid:Point
        new(idx:int, b:Bound) = {primitiveNumber=idx;bound=b;centroid=b.pMin+0.5*b.Diagnal()}
    end

let rec RecursiveBuild(prims:Primitive array) =
    match prims.Length with
    | 0 -> assert(false);BVHBuildNode(None,None,0)
    | 1 -> BVHBuildNode(None,None,0)
    | 2 ->
        let l = RecursiveBuild([|prims[0]|])
        let r = RecursiveBuild([|prims[1]|])
        BVHBuildNode(Some l,Some r,0)
    | _ ->
        let range = [|0..prims.Length-1|]
        let ctp = Point(0.5,0.5,0.5)
        let centroidBounds =
            range |> Array.map(fun i -> prims[i].WorldBound().Lerp(ctp))
            |> Array.fold (fun b c -> Bound.Union(b,c)) Bound.DefaultValue
        let dim = centroidBounds.MaximumExtent()
        let sortedPrims =
            match dim with
            | 0 ->
                prims |> Array.sortBy(fun l -> l.WorldBound().Lerp(ctp).x)
            | 1 ->
                prims |> Array.sortBy(fun l -> l.WorldBound().Lerp(ctp).y)
            | 2 ->
                prims |> Array.sortBy(fun l -> l.WorldBound().Lerp(ctp).y)
            | _ -> assert(false); [||]
        let mid = prims.Length / 2
        let arr1,arr2 = Array.take mid sortedPrims, Array.skip mid sortedPrims
        let l = RecursiveBuild(arr1)
        let r = RecursiveBuild(arr2)
        BVHBuildNode(Some l,Some r,dim)
type BVHAccel =
    struct
        val mutable primitives:Primitive array
        val mutable totalNodes:int
        val mutable root:BVHBuildNode
        new(prims:Primitive array, _maxPrimsInNode:int, splitMethod:int) =
            {
                root = RecursiveBuild(prims)
                totalNodes = 0
                primitives = prims
            }
        //interface IHitable with
            
    end