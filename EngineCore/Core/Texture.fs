module Engine.Core.Texture

open System
open System.Drawing
open System.ComponentModel
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

open Color

type Texture(data:Color[,], _width:int, _height:int) =
    let data = data
    let width = _width - 1
    let height = _height - 1

    member this.Sample(u:double, v:double) =
        let U = int (double width * u)
        let V = int (double height * v)
        data[U,V]

type Texture2D<'t>(data:Color[,], width:int, height:int) =
    member this.Sample(u:float, v:float) =
        let U = int (float width * u)
        let V = int (float height * v)
        data[U,V]
    member this.Item
        with get (i, j) = data[i,j]
        and set(i,j) (v) = data[i,j] <- v

let TextureFromFile(file:string) =
    use tmpImage = Image.Load<Rgba32>(file)
    let w = tmpImage.Width
    let h = tmpImage.Height
    let size = w * h
    let pixels = Array.zeroCreate<Rgba32> size// new Rgba32[size]
    tmpImage.CopyPixelDataTo(pixels)
    let color2d = Array2D.zeroCreate<Color> tmpImage.Width tmpImage.Height
    for y in 0..h-1 do
        for x in 0..w-1 do
            let i = y * w + x
            let p = pixels[i]
            let c = Color(float p.R, float p.G, float p.B)
            color2d[x,h-y-1] <- c   // copy picture upside down
    Texture(color2d, w, h)
