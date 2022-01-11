module Engine.Core.Samplers.JitteredSampler
open Engine.Core.Point
open Engine.Core.Interfaces.ISampler
open System

type JitteredSampler(numSamples:int) =
    inherit Sampler(numSamples)

    let rand = new Random()

    override this.GenerateSamples() =
        let n = int (sqrt(float numSamples))
        assert(numSamples%n = 0)
        for j in 0..n-1 do
            for k in 0..n-1 do
                let point = Point2D((float k + rand.NextDouble()) / float n, (float k + rand.NextDouble()) / float n)
                base.Samples[j*n+k] <- point