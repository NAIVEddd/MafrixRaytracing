module Engine.Core.Lights.Directional
open Engine.Core.Interfaces.ILight
open Engine.Core.Color
open Engine.Core.Point

// type Directional(ls:float, color:Color, direction:Vector) =
//     inherit Light()
//     override this.GetDirection(record) = direction
//     override this.L(record) = ls*color