module Engine.Core.Texture

open Color
open RenderTarget

type Texture(data:Color[,], _width:int, _height:int) =
    let data = data
    let width = _width - 1
    let height = _height - 1

    member this.Sample(u:double, v:double) =
        let U = int (double width * u)
        let V = int (double height * v)
        data[U,V]