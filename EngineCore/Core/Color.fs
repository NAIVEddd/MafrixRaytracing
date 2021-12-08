module Engine.Core.Color

type Color(r:float, g:float, b:float, a:float) =
    new() = Color(0, 0, 0, 0)
    new(r:float,g:float,b:float) = Color(r,g,b,1.0)
    member this.r = r
    member this.g = g
    member this.b = b
    member this.a = a
    member this.Uniform() = Color(r/255.0,g/255.0,b/255.0,a)
    static member (*) (l:Color, r:Color) = Color(l.r*r.r, l.g*r.g, l.b*r.b, min (l.a*r.a) 1.0)
    static member (*) (l:float, r:Color) = Color(l*r.r, l*r.g, l*r.b, min (l*r.a) 1.0)
    static member (*) (l:Color, r:float) = r*l
    static member (+) (l:Color, r:Color) = Color(l.r+r.r, l.g+r.g, l.b+r.b, min (l.a+r.a) 1.0)