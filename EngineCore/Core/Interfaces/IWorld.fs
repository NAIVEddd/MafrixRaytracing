module Engine.Core.Interfaces.IWorld
open Engine.Core.Interfaces.ILight
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.ITracer

// type IWorld =
//     inherit IHitable
//     abstract member GetAmbientLight: unit -> ILight
//     abstract member GetLights: unit -> ILight[]
//     abstract member AddLight: ILight -> unit
//     abstract member GetObjects: unit -> IHitable[]
//     abstract member AddObject: IHitable -> unit
//     abstract member Build: unit -> unit
//     abstract member SetTracer: ITracer -> unit
//     abstract member GetTracer: unit -> ITracer