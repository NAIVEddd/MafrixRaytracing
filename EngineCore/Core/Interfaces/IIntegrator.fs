module Engine.Core.Interfaces.IIntegrator

open Engine.Core.Color
open Engine.Core.Point
open Engine.Core.Ray
open Engine.Core.Texture
open Engine.Core.Interfaces.HitRecord

/// <Description>The Render Equation</Description>
/// <summary> Lo(x,Wo) = Le(x,Wo) + Integration( Li(x,Wi) * Fx(Wi,Wo) * dot(Wi,N) ) </summary>
/// IDirectIntegrator get sampled Wi according to LightSource.
/// IIndirectIntegrator get sampled Wi according to Material/Brdf.
/// IPathTracer use IDirectIntegrator and IIndirectIntegrator to solve integration in The Render Equation with MIS(multi importantance sample)

type ILiIntegrator =
    /// <summary></summary>
    /// <param name="hitRecord"></param>
    /// <param name="wi"></param>
    /// <param name="color"></param>
    /// <param name="pdf"></param>
    /// <returns></returns>
    abstract Eval : HitRecord * outref<Vector> * outref<Color> * outref<float> -> unit
    abstract Eval : HitRecord * Vector * outref<Color> * outref<float> -> unit

type IDirectIntegrator =
    inherit ILiIntegrator

type IIndirectIntegrator =
    inherit ILiIntegrator

// eval the rander equation, with IDirectIntergrator and IIndirectIntergrator help
type IPathTracer =
    abstract TraceRay : Ray -> Color

type IPixelIntegrator =
    /// <summary>sample all pixels to get 'texture</summary>
    /// <param name"nSample">samples per pixel</param>
    /// <typeparam name="int"></typeparam>
    /// <returns>'texture: include one frame of screen.</returns>
    abstract Sample : int -> Texture2D<Color>
