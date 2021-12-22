module Engine.Core.Point

[<Struct>]
type Point2D(_x:double, _y:double) =
    override this.ToString() =
        sprintf "(%A, %A)" this.x this.y

    member this.x = _x
    member this.y = _y
    static member (+) (l:Point2D, r:Point2D) = Point2D(l.x + r.x, l.y + r.y)
    static member (*) (l:double, r:Point2D) = Point2D(l * r.x, l * r.y)
    static member (*) (l:Point2D, r:double) = r*l

[<Struct>]
type Point(_x:double, _y:double, _z:double) =
    new(n:double) = Point(n,n,n)
    override this.ToString() =
        sprintf "P(%A, %A, %A)" this.x this.y this.z

    member this.x = _x
    member this.y = _y
    member this.z = _z
    static member (-) (l:Point, r:Point) =
        Vector(l.x-r.x, l.y-r.y, l.z-r.z)

and [<Struct>] Vector(_x:double, _y:double, _z:double) =
    //let length = sqrt(x*x + y*y + z*z)
    override this.ToString() =
        sprintf "V(%A, %A, %A)" this.x this.y this.z

    member this.x = _x
    member this.y = _y
    member this.z = _z
    member this.Length = sqrt(this.x*this.x + this.y*this.y + this.z*this.z)//length
    member this.Normalize =
        let length = this.Length
        match length with
        | 0.0 -> Vector()
        | _ -> Vector(this.x/length, this.y/length, this.z/length)
    member this.Cross (v:Vector) = Vector(this.y*v.z - this.z*v.y, this.z*v.x - this.x*v.z, this.x*v.y - this.y*v.x)
    member this.Dot(v:Vector) = this.x*v.x + this.y*v.y + this.z*v.z
    static member (+) (l:Vector, r:Vector) = Vector(l.x + r.x, l.y + r.y, l.z + r.z)
    static member (+) (l:Point, r:Vector) = Point(l.x+r.x, l.y+r.y, l.z+r.z)
    static member (+) (l:Vector, r:Point) = r+l
    static member (*) (a, v:Vector) = Vector(v.x*a, v.y*a, v.z*a)
    static member (*) (v:Vector, a) = Vector(v.x*a, v.y*a, v.z*a)

[<Struct>]
type Indexer(i1:int, i2:int, i3:int) =
    override this.ToString() =
        sprintf "(%A, %A, %A)" i1 i2 i3

    member this.i = i1
    member this.j = i2
    member this.k = i3
