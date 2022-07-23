module Engine.Core.Integrator

open Engine.Core.Interfaces.ICamera
open Engine.Core.Interfaces.IIntegrator
open Engine.Core.Interfaces.ILight
open Engine.Core.Interfaces.HitRecord
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Color
open Engine.Core.Point
open Engine.Core.Ray
open Engine.Core.Texture
open Engine.Core.Camera
open Engine.Core.Accels.BvhNode
open System

type VisibilityTester(bvh:Bvh) =
    member this.CheckVisibility(ray:Ray, light:INewLight) = true

[<Struct>]
type SingleDirectLightIntegrator(light:INewLight, bvh:Bvh) =
    member this.VisibilityTest(hit:HitRecord) =
        let dist,toLight = light.GetDirection(hit)
        let unitToLight = toLight/dist
        let shadowHit = bvh.Hit(Ray(hit.point,unitToLight),1e-6,dist-1e-6)
        if shadowHit.hit then
            Color()
        else
            let l = light.L(hit,toLight)
            unitToLight.Dot(hit.normal) * l
    member this.VisibilityTest(hit:HitRecord, toLight:Vector) =
        
        let dist,toLight = light.GetDirection(hit)
        let unitToLight = toLight/dist
        let shadowHit = bvh.Hit(Ray(hit.point,unitToLight),1e-6,dist-1e-6)
        if shadowHit.hit then
            Color()
        else
            let l = light.L(hit,toLight)
            unitToLight.Dot(hit.normal) * l
    interface IDirectIntegrator with
        member this.Eval(hit:HitRecord, wi:outref<Vector>, color:outref<Color>, pdf:outref<float>) =
            let p, dist,toLight = light.Sample_Li(hit)
            let unitToLight = toLight/dist
            let shadowHit = bvh.Hit(Ray(hit.point,unitToLight),1e-6,dist-1e-6)
            wi <- unitToLight
            pdf <- p
            color <-
                if shadowHit.hit then
                    Color()
                else
                    let l = light.L(hit,toLight)
                    unitToLight.Dot(hit.normal) * l
        member this.Eval(hit:HitRecord, wi:Vector, color:outref<Color>, pdf:outref<float>) =
            ()

[<Struct>]
type RandomDirectLightIntegrator =
    val lights : INewLight array
    val rand : Random
    val bvh : Bvh
    new(ls:INewLight array, bvh:Bvh) =
        {
            lights=ls;
            rand = new Random();
            bvh = bvh;
        }
    interface IDirectIntegrator with
        member this.Eval(hit:HitRecord, wi:outref<Vector>, color:outref<Color>, pdf:outref<float>) =
            let n = int(floor(this.rand.NextDouble() * float this.lights.Length))
            let light = this.lights[n]
            let integrator :IDirectIntegrator = SingleDirectLightIntegrator(light, this.bvh)
            integrator.Eval(hit, &wi, &color, &pdf)
            
        member this.Eval(hit:HitRecord, wi:Vector, color:outref<Color>, pdf:outref<float>) =
            let n = int(floor(this.rand.NextDouble() * float this.lights.Length))
            let light = this.lights[n]
            let integrator :IDirectIntegrator = SingleDirectLightIntegrator(light, this.bvh)
            integrator.Eval(hit, wi, &color, &pdf)

// [<Struct>]
// type AllDirectLightIntegrator(light:INewLight) =
//     interface IDirectIntegrator with
//         member this.Eval(hit:HitRecord, wi:outref<Vector>, color:outref<Color>, pdf:outref<float) =
//             ()
//         member this.Eval(hit:HitRecord, wi:Vector, color:outref<Color>, pdf:outref<float) =
//             ()

[<Struct>]
type IndirectIntegrator =
    interface IIndirectIntegrator with
        member this.Eval(hit:HitRecord, wi:outref<Vector>, color:outref<Color>, pdf:outref<float>) =
            ()
        member this.Eval(hit:HitRecord, wi:Vector, color:outref<Color>, pdf:outref<float>) =
            ()

[<Struct>]
type PathIntegrator(bvh:Bvh, maxDepth : int, light:INewLight) =
    member this.VisibilityTest(hit:HitRecord) =
        let dist,toLight = light.GetDirection(hit)
        let unitToLight = toLight/dist
        let shadowHit = bvh.Hit(Ray(hit.point,unitToLight),1e-6,dist-1e-6)
        if shadowHit.hit then
            Color()
        else
            let l = light.L(hit,toLight)
            unitToLight.Dot(hit.normal) * l
    member this.TraceRay(ray:Ray, depth:int) =
        let hit = bvh.Hit(ray,1e-6,99999999.)
        if hit.hit && depth >= 0 then
            // let material = MaterialManager.GetManager()[hit.materialIndex]
            // let col, r = material.Scatter(ray, hit)
            // let t = this.TraceRay(r, depth - 1)
            // let shade = material.Shade(hit,r,t)
            // let l = this.VisibilityTest(hit)
            // l * col // direct light
            //     + col * shade

            let material = MaterialManager.GetManager()[hit.materialIndex]
            // Indirect Integrator ( sample from material )
            let bxdf = material.GetBxdf()
            let wo = - ray.Direction()
            let wi = ref (Vector())
            let pdf, col = bxdf.SampleF(hit, wo, wi, Point2D())

            // Direct Integrator ( sample from lights )
            let lightInteg : IDirectIntegrator = SingleDirectLightIntegrator(light, bvh)
            let wo_li = ref (Vector())
            let pdf_li = ref(0.)
            let l = ref(Color())
            lightInteg.Eval(hit, wo_li, l, pdf_li)
            // l * col // direct light
            //     + col / pdf * t

            // recursive use MIS to calc the final pixel color
            let r = Ray(hit.point, wi.Value)
            (l.Value/pdf_li.Value + this.TraceRay(r, depth - 1)) * col / pdf
        else
            Color()
    interface IPathTracer with
        member this.TraceRay(ray:Ray) =
            this.TraceRay(ray, maxDepth)

[<Struct>]
type PixelIntegrator =
    val width : float
    val height : float
    val texture : Texture2D<Color>
    val pixelCoords : (int*int) array
    val cam : ICamera
    val tracer : IPathTracer

    new(width:int, height:int, cam:ICamera, tracer: IPathTracer) =
        {
            width = float width; height = float height;
            texture = new Texture2D<Color>(Array2D.zeroCreate<Color> width height, width, height);
            pixelCoords = Array.allPairs [|0..width-1|] [|0..height-1|];
            cam = cam; tracer = tracer;
        }
    
    interface IPixelIntegrator with
        member this.Sample(n:int) =
            let rand = new Random()
            let self = this
            this.pixelCoords |> Array.Parallel.iter(fun(i,j) ->
                let mutable color = Color()
                for s in 0..n-1 do
                    let u = (float i + rand.NextDouble()) / self.width
                    let v = (float j + rand.NextDouble()) / self.height
                    let ray = self.cam.GetRay(u,v)
                    color <- color + self.tracer.TraceRay(ray)
                self.texture[i,j] <- color / float n)
            this.texture