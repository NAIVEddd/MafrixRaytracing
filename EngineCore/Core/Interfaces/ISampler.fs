module Engine.Core.Interfaces.ISampler
open Engine.Core.Point

type ISampler =
    abstract GenerateSamples : unit -> unit
    abstract SampleUnitSquare : unit -> Point2D

[<AbstractClass>]
type Sampler(numSamples:int) =
    let samples = Array.zeroCreate<Point2D> numSamples
    let mutable index = 0
    member this.Samples = samples

    abstract member SetShuffledIndices : unit -> unit
    abstract member ShuffleSamples : unit -> unit
    abstract member SampleUnitSquare : unit -> Point2D
    override this.SetShuffledIndices() = ()
    override this.ShuffleSamples() = ()
    override this.SampleUnitSquare() =
        let p = samples[index]
        index <- (index + 1)%numSamples
        p

    abstract member GenerateSamples : unit -> unit

    interface ISampler with
        member this.GenerateSamples() = this.GenerateSamples()
        member this.SampleUnitSquare() = this.SampleUnitSquare()