module Engine.Core.Interfaces.ICamera
open Engine.Core.Ray

type ICamera =
    abstract member GetRay : float * float -> Ray