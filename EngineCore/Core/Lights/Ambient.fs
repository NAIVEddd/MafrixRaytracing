module Engine.Core.Lights.Ambient
open Engine.Core.Interfaces.ILight
open Engine.Core.Color
open Engine.Core.Point

// ls: light intencity scale
// type Ambient(ls:float, color:Color) =
//     inherit Light()
//     new() = Ambient(0.0, Color(1,1,1))
//     override this.GetDirection(record) = Vector()
//     override this.L(record) = ls*color