module Engine.Model.ObjLoader
open Engine.Model.Obj_Mtl
open Engine.Core.Color
open Engine.Core.Point
open Engine.Core.Shapes.Triangle
open Engine.Core.Shapes.Rect
open Engine.Core.Interfaces.IHitable
open FParsec
open System.Collections
open FSharp.Collections
open System.IO

type VertexData =
    | Vertex of Point
    | Texture of Point2D
    | Normal of Vector

type ObjState =
    val vertices : Point ResizeArray
    val textures : Point2D ResizeArray
    val normals : Vector ResizeArray
    val mutable facesMaps : Map<string, IHitable ResizeArray>
    val facesDef : (string * IHitable) ResizeArray
    val faces : IHitable ResizeArray
    val mutable curGroup : string
    new() =
        let emp = Map [("default", ResizeArray<IHitable>())]
        {
            vertices = ResizeArray<Point>();
            textures = ResizeArray<Point2D>();
            normals = ResizeArray<Vector>();
            facesMaps = emp;
            facesDef = ResizeArray<string * IHitable>();
            curGroup = "default";
            faces = ResizeArray<IHitable>();
        }
    member this.InsertData(data:VertexData) =
        match data with
        | Vertex p -> this.vertices.Add(p)
        | Texture p -> this.textures.Add(p)
        | Normal n -> this.normals.Add(n)
    member this.InsertData(face:IHitable) =
        let tmparray = Map.find this.curGroup this.facesMaps
        tmparray.Add(face)
        this.facesMaps <- this.facesMaps.Add (this.curGroup, tmparray)
        this.facesDef.Add(this.curGroup, face)
        this.faces.Add(face)
    member this.UpdateGroup(n:string) =
        this.curGroup <- n
        let m = Map.tryFind n this.facesMaps
        match m with
        | Some _ -> ()
        | None -> this.facesMaps <- this.facesMaps.Add (n, ResizeArray<IHitable>())

// first reference number is the geometric vertex
// second reference number is the texture vertex, can be empty
// third reference number is the vertex normal, cam be empty
type VertexReferencing =
    | Vertex of int
    | Vertex_Texture of int * int
    | Vertex_Normal of int * int
    | Vertex_Texture_Normal of int * int * int
    member inline private this.VI(i:int, len:int) =
        if i > 0 then i - 1 else len + i
    member this.VertexIndex(len:int) =
        match this with
        | Vertex i -> this.VI(i,len)
        | Vertex_Texture (i,_) -> this.VI(i,len)
        | Vertex_Normal (i,_) -> this.VI(i,len)
        | Vertex_Texture_Normal (i,_,_) -> this.VI(i,len)
type Face =
    val vertices: VertexReferencing list
    new(vs: VertexReferencing list) =
        assert(vs.Length >= 3)
        {vertices=vs;}
    member this.ToHitable(vertex:ObjState, materialIdx:int) :IHitable =
        let vLen = vertex.vertices.Count
        match this.vertices.Length with
        | 3 ->
            let p0 = vertex.vertices[this.vertices[0].VertexIndex(vLen)]
            let p1 = vertex.vertices[this.vertices[1].VertexIndex(vLen)]
            let p2 = vertex.vertices[this.vertices[2].VertexIndex(vLen)]
            Triangle(p0,p1,p2,materialIdx)
        | 4 ->
            let p0 = vertex.vertices[this.vertices[0].VertexIndex(vLen)]
            let p1 = vertex.vertices[this.vertices[1].VertexIndex(vLen)]
            let p2 = vertex.vertices[this.vertices[2].VertexIndex(vLen)]
            let p3 = vertex.vertices[this.vertices[3].VertexIndex(vLen)]
            Rect(p0,p1,p2,p3, materialIdx)
        | _ ->
            assert(false)
            Rect()
type GroupStatement =
    | Name of string
type StateSetting =
    | Smooth of bool    // off or 0 is false, on or number greater than 0 is true
    | Usemtl of string
    | Usemap of string
    | Mtllib of string[]
    | Maplib of string[]
type StateSettings =
    val mutable smooth : bool
    val mutable usemtl : string
    val mutable usemap : string
    val mutable mtllib : string[]
    val mutable maplib : string[]
    new() =
        {
            smooth = true;
            usemtl = "white";
            usemap = "";
            mtllib = [||];
            maplib = [||];
        }
    member this.Update(setting : StateSetting) =
        match setting with
        | Smooth s -> this.smooth <- s
        | Usemtl s -> this.usemtl <- s
        | Usemap s -> this.usemap <- s
        | Mtllib s -> this.mtllib <- s
        | Maplib s -> this.maplib <- s
type ObjStatement =
    | VertexData of VertexData
    //| VertexReferencing of VertexReferencing
    | Faces of Face
    | GroupStatement of GroupStatement
    | StateSetting of StateSetting
    | Comment of string

let pKeyword_Vertex : Parser<string,unit> = pstring "v" .>> spaces
let pKeyword_TextureVertex : Parser<string,unit> = pstring "vt" .>> spaces
let pKeyword_VertexNormal : Parser<string,unit> = pstring "vn" .>> spaces
let pKeyword_Comment : Parser<string,unit> = pstring "#"
let pKeyword_Face : Parser<string,unit> = pstring "f" .>> spaces
let pKeyword_Smooth : Parser<string,unit> = pstring "s" .>> spaces
let pKeyword_Group : Parser<string,unit> = pstring "g" .>> spaces
let pKeyword_Usemtl : Parser<string,unit> = pstring "usemtl" .>> spaces
let pKeyword_Usemap : Parser<string,unit> = pstring "usemap" .>> spaces
let pKeyword_Mtllib : Parser<string,unit> = pstring "mtllib" .>> spaces
let pKeyword_Maplib : Parser<string,unit> = pstring "maplib" .>> spaces

let toColor(nums:float list) =
    assert(nums.Length = 3)
    Color(nums[0],nums[1],nums[2])
let toVector(nums:float list) =
    assert(nums.Length = 3 || nums.Length = 4)
    if nums.Length = 3 then
        Vector(nums[0],nums[1],nums[2])
    else
        assert(nums[3] > 1e-6)
        Vector(nums[0]/nums[3],nums[1]/nums[3],nums[2]/nums[3])
let toPoint2D(nums:float list) =
    assert(nums.Length = 2 || nums.Length = 3)
    if nums.Length = 2 then
        Point2D(nums[0],nums[1])
    else
        assert(nums[2] > 1e-6)
        Point2D(nums[0]/nums[2],nums[1]/nums[2])
let toPoint(nums:float list) =
    assert(nums.Length = 3)
    Point(nums[0],nums[1],nums[2])
let pFloat :Parser<float,unit> = pfloat
let pFloatList = many (spaces >>. pFloat .>> spaces)
let pColor = pFloatList |>> toColor
let pLine =
    manySatisfy (isNoneOf "\n") .>> spaces
let pVertexReference : Parser<VertexReferencing, unit> =
    let pSlash = pchar '/' .>> spaces
    fun stream ->
        let vertexReply = (pint32 .>> spaces) stream
        if vertexReply.Status = Ok then
            let s = pSlash stream
            if s.Status = Ok then   // a/*
                let textureReply = attempt (pint32 .>> spaces) stream
                if textureReply.Status = Ok then
                    let s = pSlash stream
                    if s.Status = Ok then   // a/b/c
                        let normalReply = (pint32 .>> spaces) stream
                        if normalReply.Status = Ok then
                            let res = VertexReferencing.Vertex_Texture_Normal(vertexReply.Result,textureReply.Result,normalReply.Result)
                            Reply(res)
                        else    // not have normal reference
                            let messageList = normalReply.Error
                            let errorString = expectedString "need normal index"
                            Reply(normalReply.Status, ErrorMessageList.Merge(messageList,errorString))
                    else    // a/b
                        let res = VertexReferencing.Vertex_Texture(vertexReply.Result,textureReply.Result)
                        Reply(res)
                else    // a//c
                    let s = pSlash stream
                    if s.Status = Ok then   // a//c
                        let normalReply = (pint32 .>> spaces) stream
                        if normalReply.Status = Ok then
                            let res = VertexReferencing.Vertex_Normal(vertexReply.Result,normalReply.Result)
                            Reply(res)
                        else    // not have normal reference
                            let messageList = normalReply.Error
                            let errorString = expectedString "need normal index"
                            Reply(normalReply.Status, ErrorMessageList.Merge(messageList,errorString))
                    else    // a
                        let res = VertexReferencing.Vertex(vertexReply.Result)
                        Reply(res)
            else    // a
                let res = VertexReferencing.Vertex(vertexReply.Result)
                Reply(res)
        else
            let messagelist = vertexReply.Error
            let errorString = expectedString "at least one int in face define."
            Reply(vertexReply.Status, ErrorMessageList.Merge(messagelist,errorString))
let pSmooth : Parser<StateSetting, unit> =
    fun stream ->
        let prefix = pKeyword_Smooth stream
        if prefix.Status = Ok then
            let o = (attempt pint32) stream
            if o.Status = Ok then
                Reply(StateSetting.Smooth(o.Result > 0))
            else
                let str = (manyCharsTill anyChar (pchar '\n') .>> spaces) stream
                let b =
                    let s = str.Result.Trim()
                    if s = "on" then
                        true
                    else
                        false
                Reply(StateSetting.Smooth(b))
        else
            Reply(prefix.Status, expectedString("s"))
let pUsemtl : Parser<StateSetting, unit> =
    fun stream ->
        let prefix = pKeyword_Usemtl stream
        if prefix.Status = Ok then
            let file = (many1CharsTill anyChar (pchar '\n') .>> spaces) stream
            if file.Status = Ok then
                let str = file.Result.Trim()
                Reply(StateSetting.Usemtl(str))
            else
                Reply(file.Status, expectedString("filename"))
        else
            Reply(prefix.Status, expectedString("usemtl"))
let pUsemap : Parser<StateSetting, unit> =
    fun stream ->
        let prefix = pKeyword_Usemap stream
        if prefix.Status = Ok then
            let file = (many1CharsTill anyChar (pchar '\n') .>> spaces) stream
            if file.Status = Ok then
                let str = file.Result.Trim()
                Reply(StateSetting.Usemtl(str))
            else
                Reply(file.Status, expectedString("filename"))
        else
            Reply(prefix.Status, expectedString("usemap"))
let pFilenames : Parser<string[], unit> =
    fun stream ->
        let line = pLine stream
        if line.Status = Ok then
            let strs = line.Result.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
            Reply(strs)
        else
            Reply(line.Status, expectedString("Some filenames"))
let pMtllib =
    pKeyword_Mtllib >>. pFilenames |>> StateSetting.Mtllib
let pMaplib =
    pKeyword_Maplib >>. pFilenames |>> StateSetting.Maplib
let Parse_GroupStatement =
    pKeyword_Group >>. pLine |>> GroupStatement.Name
let Parse_Comment =
    pKeyword_Comment >>. pLine
let Parse_GermetricVertex =
    pKeyword_Vertex >>. pFloatList |>> toPoint |>> VertexData.Vertex
let Parse_TextureVertices =
    pKeyword_TextureVertex >>. pFloatList |>> toPoint2D |>> VertexData.Texture
let Parse_VertexNormal =
    pKeyword_VertexNormal >>. pFloatList |>> toVector |>> VertexData.Normal
let Parse_Face =
    pKeyword_Face >>. (many pVertexReference) |>> Face
let Parse_StateSetting =
    choice [
        pSmooth
        pUsemtl
        pUsemap
        pMtllib
        pMaplib
    ]
let ParseObjFile =
    many (choice [
        //pLine |>> ObjStatement.Comment
        Parse_Comment |>> ObjStatement.Comment
        Parse_GroupStatement |>> ObjStatement.GroupStatement
        Parse_GermetricVertex |>> ObjStatement.VertexData
        Parse_TextureVertices |>> ObjStatement.VertexData
        Parse_VertexNormal |>> ObjStatement.VertexData
        Parse_Face |>> ObjStatement.Faces
        Parse_StateSetting |>> ObjStatement.StateSetting
    ])

let UpdateState(state:ObjState, statement:ObjStatement, setting:StateSettings, materials:MtlMaterialReference) =
    match statement with
    | VertexData d -> state.InsertData(d)
    | Faces f ->
        let index = materials[setting.usemtl]
        state.InsertData(f.ToHitable(state,index))
    | GroupStatement (GroupStatement.Name g) -> state.UpdateGroup(g)
    | StateSetting s -> setting.Update(s)
    | Comment _ -> ()

let LoadObjModel(file:string) =
    let lines = File.ReadAllText(file)
    let parseResult = run ParseObjFile lines
    match parseResult with
    | Success(result, _, _)   ->
        let resultArray = Array.ofList result |> Array.filter(
            fun s ->
                match s with
                | Comment _ -> false
                | _ -> true
        )
        let mtlNameSetting = resultArray |> Array.tryFind (
            fun r ->
                match r with
                | StateSetting s ->
                    match s with
                    | Mtllib _ -> true
                    | _ -> false
                | _ -> false
        )
        let materialRefs =
            match mtlNameSetting with
            | Some (StateSetting (Mtllib names)) ->
                LoadObjMtl(names[0])
            | _ -> MtlMaterialReference([])
        let state = new ObjState()
        let setting = new StateSettings()
        resultArray |> Array.iter (
            fun s ->
                UpdateState(state, s, setting, materialRefs)
        )
        state
    | Failure(errorMsg, _, _) ->
        printfn "Failure: %s" errorMsg
        new ObjState()
    