module Engine.Core.Light
open Color
open Point
open Camera

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