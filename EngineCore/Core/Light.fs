module Engine.Core.Light
open Color
open Point

type AmbientLight(color:Color) =
    member this.color = color

type DirectionLight(color:Color, diffuse:Color, specular:Color, dir:Vector) =
    member this.color = color
    member this.diffuse = diffuse
    member this.specular = specular
    member this.dir = dir

type PointLight(color:Color, diffuse:Color, pos:Point, kc:double, kl:double, kq:double) =
    let color = color

type Light =
    | Ambient of AmbientLight
    | Direction of DirectionLight
    | Point of PointLight