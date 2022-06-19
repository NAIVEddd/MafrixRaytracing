module Assignment_6_837.Obj

open Microsoft.FSharp.Core
open Microsoft.FSharp.Collections
open Engine.Core.Point

  
type LineDefineType =
    | Empty
    | Comment
    | Vertex
    | TextureCoordinate
    | VertexNormal
    | Face
    | RefMaterial
    | UseMaterial
    | ObjectName
    | GroupName

let ParseLineType (str:string) : LineDefineType =
    match str with
    | "" -> Empty
    | str ->
        let head = str.Split(' ')[0]
        match head with
        | "#" -> Comment
        | "v" ->Vertex
        | "vt" -> TextureCoordinate
        | "vn" -> VertexNormal
        | "f" -> Face
        | "mtllib" -> RefMaterial
        | "usemtl" -> UseMaterial
        | "o" -> ObjectName
        | "g" -> GroupName
        | _ ->  
                printfn "Not supported format :%s" head
                //assert(false)
                Empty

let ObjBackFolder (lineType: LineDefineType) (state: list<LineDefineType * int>) : list<LineDefineType * int> =
    match lineType with
    | Vertex | TextureCoordinate | VertexNormal | Face ->
        match state with
        | [] -> [(lineType, 1)]
        | (frontType, count) :: tail when frontType = lineType -> (lineType, count + 1) :: tail
        | _ -> (lineType, 1) :: state
    | _ -> (lineType, 1) :: state

type ObjIncludeInfo (init:list<LineDefineType * int>) =
    let mutable vertexs = Array.zeroCreate<Point> 1
    let mutable textureCoords = Array.zeroCreate<Point2D> 1
    let mutable vertexNormals = Array.zeroCreate<Vector> 1
    // vertex indexer * textureCoords indexer * normal indexer
    let mutable faces = Array.zeroCreate<Indexer*Indexer*Indexer> 1

    do
        for it in init do
            match it with
            | (Vertex, count) -> vertexs <- Array.zeroCreate count
            | (TextureCoordinate, count) -> textureCoords <- Array.zeroCreate count
            | (VertexNormal, count) -> vertexNormals <- Array.zeroCreate count
            | (Face, count) -> faces <- Array.zeroCreate count
            | _ -> ()

    member this.Vertexs = vertexs
    member this.TextureCoords = textureCoords
    member this.VertexNormals = vertexNormals
    member this.Faces = faces

let LoadModel (filePath:string) =
    let lines = System.IO.File.ReadAllLines(filePath)
    let types = Array.map ParseLineType lines

    let objsize = Array.foldBack ObjBackFolder types []
    let objinfo = ObjIncludeInfo(objsize)
    //printfn "%A" objsize

    assert(lines.Length = types.Length)
    let mutable count = 0
    for i = 0 to lines.Length - 1 do
        match types[i] with
        | Vertex ->
            let point = lines[i] |> (fun line ->
                let nums = line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                            |> Array.skip 1
                            |> Array.map double
                assert(nums.Length = 3)
                Point(nums[0], nums[1], nums[2]))
            objinfo.Vertexs[count] <- point
            count <- if (count + 1) = objinfo.Vertexs.Length then 0 else count + 1

        | TextureCoordinate ->
            let coord = lines[i] |> (fun line ->
                let nums = line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                            |> Array.skip 1
                            |> Array.map double
                assert(nums.Length = 2 || nums.Length = 3)
                Point2D(nums[0], nums[1]))
            objinfo.TextureCoords[count] <- coord
            count <- if (count + 1) = objinfo.TextureCoords.Length then 0 else count + 1

        | VertexNormal ->
            let normal = lines[i] |> (fun line ->
                let nums = line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                            |> Array.skip 1
                            |> Array.map double
                assert(nums.Length = 3)
                Vector(nums[0], nums[1], nums[2]))
            objinfo.VertexNormals[count] <- normal
            count <- if (count + 1) = objinfo.VertexNormals.Length then 0 else count + 1

        | Face ->
            let face = lines[i] |> (fun line ->
                let nums = line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                            |> Array.skip 1
                            |> Array.map (fun s ->
                                s.Split('/', System.StringSplitOptions.RemoveEmptyEntries)
                                    |> Array.map int)
                assert(nums.Length = 3)
                assert(nums[0].Length = nums[1].Length && nums[1].Length = nums[2].Length)
                Array.zip3 nums[0] nums[1] nums[2]
                    |> Array.map (fun (a, b, c) -> Indexer(a-1, b-1, c-1)))
            //assert(face.Length = 3)
            if face.Length = 3 then
                objinfo.Faces[count] <- (face[0], face[1], face[2])
            elif face.Length = 2 then
                objinfo.Faces[count] <- (face[0], face[1], Indexer())
            else
                objinfo.Faces[count] <- (face[0], Indexer(), Indexer())
            count <- if (count + 1) = objinfo.Faces.Length then 0 else count + 1

        | _ -> ()

    objinfo