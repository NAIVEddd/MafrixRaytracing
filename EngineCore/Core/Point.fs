module Engine.Core.Point

type Point2D(x:double, y:double) =
    new() = Point2D(0, 0)

type Point(x:double, y:double, z:double) =
    new() = Point(0)
    new(n:double) = Point(n,n,n)

type Vector(x:double, y:double, z:double) =
    new() = Vector(0, 0, 0)

type Indexer(i1:int, i2:int, i3:int) =
    new() = Indexer(0, 0, 0)