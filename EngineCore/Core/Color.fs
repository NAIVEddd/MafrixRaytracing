module Engine.Core.Color

[<Struct>]
type Color(_r:float, _g:float, _b:float, _a:float) =
    new(r:float,g:float,b:float) = Color(r,g,b,1.0)
    member this.r = _r
    member this.g = _g
    member this.b = _b
    member this.a = _a
    member this.Uniform() = Color(this.r/255.0,this.g/255.0,this.b/255.0,this.a)
    static member (*) (l:Color, r:Color) = Color(l.r*r.r, l.g*r.g, l.b*r.b, min (l.a*r.a) 1.0)
    static member (*) (l:float, r:Color) = Color(l*r.r, l*r.g, l*r.b, min (l*r.a) 1.0)
    static member (*) (l:Color, r:float) = r*l
    static member (+) (l:Color, r:Color) = Color(l.r+r.r, l.g+r.g, l.b+r.b, min (l.a+r.a) 1.0)
    static member (-) (l:Color, r:Color) = Color(l.r-r.r, l.g-r.g, l.b-r.b, min (l.a+r.a) 1.0)
    static member (/) (l:Color, r:float) = Color(l.r/r, l.g/r, l.b/r, 1.0)