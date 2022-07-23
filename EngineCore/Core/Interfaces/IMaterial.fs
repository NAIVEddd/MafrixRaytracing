module Engine.Core.Interfaces.IMaterial
open Engine.Core.Interfaces.HitRecord
open Engine.Core.Color
open Engine.Core.Point
open Engine.Core.Ray

type IBxdf =
    abstract member F : wo:Vector * wi:Vector -> Color
    abstract member Pdf : wo:Vector * wi:Vector -> float
    abstract member SampleF : HitRecord * wo:Vector * wi:outref<Vector> * sample:Point2D -> pdf:float * Color
    abstract member Rho : wo:Vector * sample:Point2D -> Color   // get reflectance

type IMaterial =
    abstract member GetBxdf : unit -> IBxdf
    abstract member Scatter : ray:Ray * HitRecord -> Color * Ray
    abstract member Shade : HitRecord * scatteredRay:Ray * indirectLight:Color -> Color
    abstract member BaseColor : unit -> Color
    abstract member Emit : unit -> Color

type MaterialManager() as this =
    do
        this.materials<-Array.empty
    [<DefaultValue>]
    val mutable materials : IMaterial[]
    static let DefaultManager = MaterialManager()
    static member GetManager() = DefaultManager

    member this.Item
        with get(i) = this.materials[i]
        and  set i m = this.materials[i] <- m
    member this.Add(mat:IMaterial) =
        let len = this.materials.Length
        this.materials <- Array.insertAt len mat this.materials
        len
    member this.Add(mats:IMaterial seq) = this.materials <- Array.insertManyAt this.materials.Length mats this.materials
