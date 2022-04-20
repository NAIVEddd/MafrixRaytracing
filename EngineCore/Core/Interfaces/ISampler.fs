module Engine.Core.Interfaces.ISampler
open Engine.Core.Point

type ISampler =
    abstract GenerateSamples : unit -> unit
    abstract SampleUnitSquare : unit -> Point2D
    abstract SampleHemisphere : unit -> Point

[<AbstractClass>]
type Sampler(numSamples:int) =
    let samples = Array.zeroCreate<Point2D> numSamples
    let mutable index = 0
    member this.Samples = samples
    member this.SampTo3D(p:Point2D) =
        let phi = p.x * 360.
        let theta = p.y * 360.
        let p = phi * (System.Math.PI / 180.0)
        let t = theta * (System.Math.PI / 180.0)
        let sin_phi = sin p
        let cos_phi = cos p
        let sin_theta = sin t
        let cos_theta = cos t
        let x = cos_phi * sin_theta
        let y = sin_phi
        let z = cos_phi * cos_theta
        Point(x,y,z)

    abstract member SetShuffledIndices : unit -> unit
    abstract member ShuffleSamples : unit -> unit
    abstract member SampleUnitSquare : unit -> Point2D
    abstract SampleHemisphere : unit -> Point
    override this.SetShuffledIndices() = ()
    override this.ShuffleSamples() = ()
    override this.SampleUnitSquare() =
        let p = samples[index]
        index <- (index + 1)%numSamples
        p
    override this.SampleHemisphere() =
        let p = samples[index]
        index <- (index + 1)%numSamples
        //let z = p.x
        //let r = sqrt(max 0. 1. - z*z)
        //let phi = 2. * r * System.Math.PI * p.y
        //let x = cos(phi)
        //let y = sin(phi)
        this.SampTo3D(p)
        //Point(x,y,z)

    abstract member GenerateSamples : unit -> unit

    interface ISampler with
        member this.GenerateSamples() = this.GenerateSamples()
        member this.SampleUnitSquare() = this.SampleUnitSquare()
        member this.SampleHemisphere() = this.SampleHemisphere()