module Engine.Core.RenderTarget
open Color

let MaxDepth = 999999999.0
type RenderTarget(width:int, height:int) =
    let buffer = Array2D.create width height (Color())
    let zBuffer = Array2D.create width height MaxDepth
    
    member this.Reset() =
        for i in 0..width-1 do
            for j in 0..height-1 do
                buffer[i,j] <- Color()
                zBuffer[i,j] <- MaxDepth
    member this.Size() = (width,height)
    member this.Item
        with get(x,y) = buffer[x,y], zBuffer[x,y]
        and  set(x,y) (color,z) =
            if z < zBuffer[x,y] then
                buffer[x,y]<-color
                zBuffer[x,y]<-z
    member this.Depth
        with get(x,y) = zBuffer[x,y]
    member this.Pixel
        with get(x,y) = buffer[x,y]
    member this.Buffer = buffer

type Screen = RenderTarget