module Engine.Core.Color

type Color(r:byte, g:byte, b:byte, a:byte) =
    new() = Color(byte 0, byte 0,byte 0,byte 0)
    new(r:int,g:int,b:int,a:int) =
        assert(r >= 0 && r <= 255)
        assert(g >= 0 && g <= 255)
        assert(b >= 0 && b <= 255)
        assert(a >= 0 && a <= 255)
        Color(byte r, byte g,byte b,byte a)
    member this.r = r
    member this.g = g
    member this.b = b
    member this.a = a