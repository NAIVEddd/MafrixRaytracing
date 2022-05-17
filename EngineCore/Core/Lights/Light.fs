module Engine.Core.Light
open Engine.Core.Shapes.Rect
open Engine.Core.Interfaces.HitRecord
open Engine.Core.Interfaces.ILight
open Color
open Point
open Camera

type NewPointLight =
    struct
        val position: Point
        val intensity : Color
        new(pos:Point,inten:Color) =
            {
                position=pos;intensity=inten;
            }
        member this.GetDirection(hit:NewHitRecord) =
            let p = hit.point
            let toLight = this.position - p
            let dist = toLight.Length
            dist, toLight
        member this.L(hit:NewHitRecord, toLight:Vector) =
            let dist = toLight.LengthSquare
            this.intensity / dist
        interface INewLight with
            member this.GetDirection(hit) = this.GetDirection(hit)
            member this.L(hit,toLig) = this.L(hit,toLig)
    end

type NewAreaLight =
    struct
        val rect : Rect
        val normal : Vector
        val color : Color
        new(p0:Point,p1:Point,p2:Point,p3:Point,nm:Vector,c:Color) =
            {
                rect = Rect(p0,p1,p2,p3,0);
                normal = nm;
                color = c;
            }
        member this.GetDirection(hit:NewHitRecord) =
            let p = hit.point
            let lp = this.rect.SamplePoint()
            let toLight = lp - p
            let dist = toLight.Length
            dist, toLight
        member this.L(hit:NewHitRecord,toLight:Vector) =
            let cos_o = toLight.Dot(this.normal)
            if cos_o < 0. then
                let dist = toLight.LengthSquare
                let intensity = this.color
                let solidAngle = (abs cos_o) * this.rect.Area() / dist
                intensity * solidAngle
            else
                Color()
        interface INewLight with
            member this.GetDirection(hit) = this.GetDirection(hit)
            member this.L(hit,tolig) = this.L(hit,tolig)
    end

type Ambient_Light(color:Color) =
    member this.color = color

type Direction_Light(color:Color, diffuse:Color, specular:Color, dir:Vector) =
    member this.color = color
    member this.diffuse = diffuse
    member this.specular = specular
    member this.dir = dir

type Point_Light(color:Color, diffuse:Color, pos:Point, kc:double, kl:double, kq:double) =
    let color = color

type Light =
    | AmbientLight of Ambient_Light
    | DirectionLight of Direction_Light
    | PointLight of Point_Light
    member this.Render(cam:Camera, objColor:Color, vs:Point[], idxs:Indexer[]) =
        [|
            for idx in idxs do
                let col =
                    match this with
                    | AmbientLight ambient->
                        let s = ambient.color.Uniform()
                        objColor * s
                    | DirectionLight direction ->
                        let u = vs[idx.j] - vs[idx.i]
                        let v = vs[idx.k] - vs[idx.i]
                        let normal = u.Cross(v).Normalize
                        let dp = normal.Dot(direction.dir.Normalize)
                        if dp < 0 then
                            let diffuse = direction.diffuse.Uniform()
                            -dp * objColor * diffuse
                        else
                            Color()
                    | PointLight point -> Color()
                yield col
        |]

    member this.Sample_Li(cam:Camera, pos:Point, nm:Vector) =
        let col =
            match this with
            | AmbientLight ambient->
                ambient.color.Uniform()
            | DirectionLight direction ->
                let dp = nm.Dot(direction.dir.Normalize)
                if dp < 0 then
                    let diffuse = direction.diffuse.Uniform()
                    -dp * diffuse
                else
                    Color()
            | PointLight point -> Color()
        col