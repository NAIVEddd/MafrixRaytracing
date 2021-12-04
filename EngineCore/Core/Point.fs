module Engine.Core.Point

type Point2D(x:double, y:double) =
    new() = Point2D(0, 0)
    override this.ToString() =
        sprintf "(%A, %A)" x y

    member this.x = x
    member this.y = y

type Point(x:double, y:double, z:double) =
    new() = Point(0)
    new(n:double) = Point(n,n,n)
    override this.ToString() =
        sprintf "P(%A, %A, %A)" x y z

    member this.x = x
    member this.y = y
    member this.z = z
    static member (-) (l:Point, r:Point) =
        Vector(l.x-r.x, l.y-r.y, l.z-r.z)

and Vector(x:double, y:double, z:double) =
    //let length = sqrt(x*x + y*y + z*z)
    new() = Vector(0, 0, 0)
    override this.ToString() =
        sprintf "V(%A, %A, %A)" x y z

    member this.x = x
    member this.y = y
    member this.z = z
    member this.Length = sqrt(x*x + y*y + z*z)//length
    member this.Normalize =
        let length = this.Length
        match length with
        | 0.0 -> Vector()
        | _ -> Vector(x/length, y/length, z/length)
    member this.Cross (v:Vector) = Vector(y*v.z - z*v.y, z*v.x - x*v.z, x*v.y - y*v.x)
    member this.Dot(v:Vector) = x*v.x + y*v.y + z*v.z
    static member (+) (l:Vector, r:Vector) = Vector(l.x + r.x, l.y + r.y, l.z + r.z)
    static member (+) (l:Point, r:Vector) = Point(l.x+r.x, l.y+r.y, l.z+r.z)
    static member (+) (l:Vector, r:Point) = r+l
    static member (*) (a, v:Vector) = Vector(v.x*a, v.y*a, v.z*a)
    static member (*) (v:Vector, a) = Vector(v.x*a, v.y*a, v.z*a)

type Indexer(i1:int, i2:int, i3:int) =
    new() = Indexer(0, 0, 0)
    override this.ToString() =
        sprintf "(%A, %A, %A)" i1 i2 i3

    member this.i = i1
    member this.j = i2
    member this.k = i3
