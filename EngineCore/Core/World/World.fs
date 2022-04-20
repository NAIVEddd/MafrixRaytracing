module Engine.Core.World
open Engine.Core.Ray
open Engine.Core.Point
open Engine.Core.Accels.BvhNode
open Engine.Core.Interfaces.ILight
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.ITracer
open Engine.Core.Interfaces.IWorld

type World(_items:IHitable[], amb:ILight, _lights:ILight[]) =
    let mutable items = _items
    let mutable bvh = Unchecked.defaultof<BvhNode>
    let mutable lights = _lights
    let mutable tracer = Unchecked.defaultof<ITracer>
    interface IWorld with
        member this.BoundBox(t0:float,t1:float) =
                let pmin = Vector()
                let pmax = Vector(1,1,1)
                true, AABB(pmin,pmax)
        member this.ShadowHit(ray:Ray) =
            let record = (this:IHitable).Hit(ray, 0.00001, 99999999.0)
            if record.bHit then
                true, record.t
            else
                false, 0.0
        member this.Hit(r:Ray, tMin:float, tMax:float) = bvh.Hit(r,tMin,tMax)
        member this.GetAmbientLight() = amb
        member this.GetLights() = lights
        member this.AddLight(light) = lights <- Array.append lights [|light|]
        member this.GetObjects() = items
        member this.AddObject(obj) = items <- Array.append items [|obj|]
        member this.Build() = bvh <- BuildBvhNode(items, items.Length, 0, 1) :?> BvhNode
        member this.SetTracer(t) = tracer <- t
        member this.GetTracer() = tracer