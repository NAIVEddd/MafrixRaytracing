module Engine.Core.Film

open Engine.Core.Color
open Engine.Core.Point
open Engine.Core.Camera
open Engine.Core.Texture
open Engine.Core.Interfaces.IIntegrator
open Silk.NET.Windowing
open Silk.NET.Input
open Silk.NET.OpenGL
open Silk.NET.OpenGL.Extensions.ImGui

type Film(width:int, height:int) =
    let texture : Texture2D<Color> = Texture2D<Color>(Array2D.zeroCreate<Color> width height, width, height)
    let target = Texture2D<Color>(Array2D.zeroCreate<Color> width height, width, height)
    let pixelCoords = Array.allPairs [|0..width-1|] [|0..height-1|]
    let mutable frameCount = 0.
    member private this.AddSample(tex:Texture2D<Color>) =
        frameCount <- frameCount + 1.
        pixelCoords |> Array.Parallel.iter(fun(i,j) ->
            let c = texture[i,j] + tex[i,j]
            texture[i,j] <- c
            target[i,j] <- c / frameCount)
    member this.Size = width, height
    member this.PixelCoords = pixelCoords
    member this.Reset() =
        frameCount <- 0.
        pixelCoords |> Array.iter(fun(i,j) ->
            texture[i,j] <- Color()
            target[i,j] <- Color())

    member this.GetFrame(intergrator:IPixelIntegrator, samples) =
        this.AddSample(intergrator.Sample(samples))
        target

    //member this.GetFrame() = target

type Window(w:int, h:int) =
    let window = Window.Create(WindowOptions.Default)
    let mutable gl : GL = null// window.CreateOpenGL()
    let mutable inputContext : IInputContext = null// window.CreateInput()
    let mutable controller : ImGuiController = null// new ImGuiController(gl, window, inputContext)
    let mutable imageTexture :uint32= 0u // gl.GenTexture()
    let imageBuffer = Array.zeroCreate<byte> (w*h*4)
    member private this.CopyData() =
        //#nowarn "9"
        //use data = fixed &imageBuffer[0]
        gl.TexImage2D(GLEnum.Texture2D, 0, InternalFormat.Rgba, (uint32)w, (uint32)h, 0, GLEnum.Rgba, GLEnum.UnsignedByte, System.ReadOnlySpan<byte>(imageBuffer))
    member this.Init(render:float * (byte array) -> unit) =
        window.add_Load(fun () ->
            gl <- window.CreateOpenGL()
            inputContext <- window.CreateInput()
            controller <- new ImGuiController(gl, window, inputContext)
            imageTexture <- gl.GenTexture()
            
            gl.BindTexture(GLEnum.Texture2D, imageTexture)
            let p1 = (int GLEnum.Linear)
            gl.TexParameterI(GLEnum.Texture2D, GLEnum.TextureMinFilter, &p1)
            gl.TexParameterI(GLEnum.Texture2D, GLEnum.TextureMagFilter, &p1)
            let p2 = (int)GLEnum.ClampToEdge
            gl.TexParameterI(GLEnum.Texture2D, GLEnum.TextureWrapS, &p2)
            gl.TexParameterI(GLEnum.Texture2D, GLEnum.TextureWrapT, &p2)
        )
        window.add_FramebufferResize(fun s ->
            gl.Viewport(s)
        )
        window.add_Render(fun delta ->
            controller.Update((float32)delta)

            gl.ClearColor(System.Drawing.Color.FromArgb(255, (int)(0.45f * 255.f), (int)(0.55f * 255.f), (int)(0.60f * 255.f)))
            gl.Clear(ClearBufferMask.ColorBufferBit)

            render(delta, imageBuffer)

            this.CopyData()
            ImGuiNET.ImGui.Begin(string("Render Result")) |> ignore
            ImGuiNET.ImGui.Image((nativeint)imageTexture, new System.Numerics.Vector2(float32 w,float32 h))
            ImGuiNET.ImGui.End()

            controller.Render()
        )
        //inputContext.Keyboards[0].add_KeyDown

        window.add_Closing(fun () ->
            controller.Dispose()
            inputContext.Dispose()
            gl.Dispose()
        )

    member this.Run() = window.Run()
    member this.Close() =
        window.Dispose()