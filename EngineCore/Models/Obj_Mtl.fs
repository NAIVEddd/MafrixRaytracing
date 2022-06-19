module Engine.Model.Obj_Mtl
open Engine.Core.Color
open Engine.Core.Point
open Engine.Core.Material
open Engine.Core.Interfaces.IMaterial
//open Engine.Core.Materials.
open FParsec
open System.Collections
open System.IO

type MtlMaterialData =
    | Name of string
    | Ka of Color   // material ambient
    | Kd of Color   // material diffuse
    | Ks of Color   // material specular
    | Tr of Color   // transmitted
    | Illum of int
    | Ns of float   // material specular exponent
    | Ni of float   // index of refraction
    | MapKa of string
    | MapKd of string
    | MapKs of string
    | MapNs of string
    | Bump of string

type MtlFileData =
    | Data of MtlMaterialData
    | Comment of string

type MtlMaterialInfo =
    struct
        val file_name : string
        val ka : Color
        val kd : Color
        val ks : Color
        val tr : Color
        val ns : float
        val ni : float
        val illum : int
        val map_ka : string
        val map_kd : string
        new(n,a,d,s,r,ns,ni,ill,mapka,mapkd) =
            {
                file_name=n;ka=a;kd=d;ks=s;tr=r;
                ns=ns;ni=ni;illum=ill;map_ka=mapka;
                map_kd=mapkd;
            }
    end

type MtlMaterialReference =
    val mutable reference : Map<string, int>
    new(kvs) =
        {
            reference = Map<string,int>(kvs)
        }
    member this.Item 
        with get(s:string) = this.reference[s]
        //and set (s:string) (v:int) = this.reference <- this.reference.Add (s,v)

let pKeyword_Maplib : Parser<string,unit> = pstring "newmtl" .>> spaces
let pKeyword_Ka : Parser<string,unit> = pstring "Ka" .>> spaces
let pKeyword_Kd : Parser<string,unit> = pstring "Kd" .>> spaces
let pKeyword_Ks : Parser<string,unit> = pstring "Ks" .>> spaces
let pKeyword_Ke : Parser<string,unit> = pstring "Ke" .>> spaces
let pKeyword_Tr : Parser<string,unit> = pstring "Tr" .>> spaces
let pKeyword_Illum : Parser<string,unit> = pstring "illum" .>> spaces
let pKeyword_Ns : Parser<string,unit> = pstring "Ns" .>> spaces
let pKeyword_Ni : Parser<string,unit> = pstring "Ni" .>> spaces
let pKeyword_MapKa : Parser<string,unit> = pstring "map_Ka" .>> spaces
let pKeyword_MapKd : Parser<string,unit> = pstring "map_Kd" .>> spaces
let pKeyword_MapKs : Parser<string,unit> = pstring "map_Ks" .>> spaces
let pKeyword_MapNs : Parser<string,unit> = pstring "map_Ns" .>> spaces
let pKeyword_Bump : Parser<string,unit> = pstring "bump" .>> spaces

let toColor(nums:float list) =
    assert(nums.Length = 3)
    Color(nums[0],nums[1],nums[2])
let pFloat :Parser<float,unit> = pfloat .>> spaces
let pFloatList = many (spaces >>. pfloat .>> spaces)
let pColor = pFloatList |>> toColor
let pKeyword_Comment : Parser<string,unit> = pstring "#"
let pLine = manySatisfy (isNoneOf "\n") .>> spaces |>> (fun s -> s.Trim())
    //manyCharsTill (noneOf "\n") (pchar '\n') .>> spaces |>> (fun s -> s.Trim())
let pMtllib =
    pKeyword_Maplib >>. pLine |>> MtlMaterialData.Name
let pKa =
    pKeyword_Ka >>. pColor |>> MtlMaterialData.Ka
let pKd =
    pKeyword_Kd >>. pColor |>> MtlMaterialData.Kd
let pKs =
    pKeyword_Ks >>. pColor |>> MtlMaterialData.Ks
let pKe =
    pKeyword_Ke >>. pLine
let pTr =
    pKeyword_Tr >>. pColor |>> MtlMaterialData.Tr
let pIllum =
    pKeyword_Illum >>. pint32 .>> spaces |>> MtlMaterialData.Illum
let pNs =
    pKeyword_Ns >>. pFloat |>> MtlMaterialData.Ns
let pNi =
    pKeyword_Ni >>. pFloat |>> MtlMaterialData.Ni
let pMapKa =
    pKeyword_MapKa >>. pLine |>> MtlMaterialData.MapKa
let pMapKd =
    pKeyword_MapKd >>. pLine |>> MtlMaterialData.MapKd
let pMapKs =
    pKeyword_MapKs >>. pLine |>> MtlMaterialData.MapKs
let pMapNs =
    pKeyword_MapNs >>. pLine |>> MtlMaterialData.MapNs
let pBump =
    pKeyword_Bump >>. pLine |>> MtlMaterialData.Bump
let Parse_Comment =
    pKeyword_Comment >>. pLine
let ParseMtlFile =
    many (
        choice [
            pMtllib
            pKa
            pKd
            pKs
            pTr
            pIllum
            pNs
            pNi
            pMapKa
            pMapKd
            pMapKs
            pMapNs
            pBump
        ] |>> (fun x -> MtlFileData.Data(x))
        <|> (pKe <|> Parse_Comment |>> (fun x -> MtlFileData.Comment(x))))

let SplitArray(data:MtlFileData[]) =
    let mtlStartIndex = ResizeArray<int>()
    data |> Array.iteri (
        fun i x ->
            match x with
            | Data d ->
                match d with
                | Name _ -> mtlStartIndex.Add(i)
                | _ -> ()
            | _ -> ())
    let count = mtlStartIndex.Count
    mtlStartIndex.Add(data.Length)
    // split data with mtl define and drop all comments
    [|
        for i in 1..count do
            let beginIdx = mtlStartIndex[i-1]
            let len = mtlStartIndex[i] - beginIdx
            let arr = Array.sub data beginIdx len
            yield arr |> Array.filter (
                fun x ->
                    match x with
                    | Data _ -> true
                    | _ -> false)
    |]

let ProcessMaterial(data:MtlFileData[]) =
    let mutable name = ""
    let mutable ka = Color()
    let mutable kd = Color()
    let mutable ks = Color()
    let mutable tr = Color()
    let mutable ns = 0.
    let mutable ni = 0.
    let mutable illum = 0
    let mutable map_ka = ""
    let mutable map_kd = ""
    data |> Array.iter (
        fun d ->
            match d with
            | Data x ->
                match x with
                | Name s -> name <- s
                | Ka c -> ka <- c
                | Kd c -> kd <- c
                | Ks c -> ks <- c
                | Tr c -> tr <- c
                | Illum i -> illum <- i
                | Ns f -> ns <- f
                | Ni f -> ni <- f
                | MapKa s -> map_ka <- s
                | MapKd s -> map_kd <- s
                | MapKs _ -> ()
                | MapNs _ -> ()
                | Bump _ -> ()
            | Comment _ -> ()
    )
    let tmpMaterial = MtlMaterialInfo(name,ka,kd,ks,tr,ns,ni,illum,map_ka,map_kd)
    let lamb = Lambertian(tmpMaterial.ka)
    let index = MaterialManager.GetManager().Add(lamb)
    (tmpMaterial.file_name, index)

let LoadObjModel(file:string) =
    let lines = File.ReadAllText(file)
    let parseResult =
        let tmpResult = run ParseMtlFile lines
        match tmpResult with
        | Success(result, _, _)   ->
            result |> List.filter (
                fun d ->
                    match d with
                    | Comment _ -> false
                    | _ -> true
            )
        | Failure(errorMsg, _, _) ->
            printfn "Failure: %s" errorMsg
            []
    let fileInfos = Array.ofList (parseResult)
    let sa = SplitArray fileInfos
    let kvs = sa |> Array.map (fun x -> ProcessMaterial(x))
    MtlMaterialReference(kvs)
