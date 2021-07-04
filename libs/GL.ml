type capability =
  | Blend
  | CullFace
  | DepthTest
  | Dither
  | PolygonOffsetFill
  | SampleAlphaToCoverage
  | SampleCoverage
  | ScissorTest
  | StencilTest

type blend_function =
  | Zero
  | One
  | SrcColor
  | OneMinusSrcColor
  | DstColor
  | OneMinusDstColor
  | SrcAlpha
  | OneMinusSrcAlpha
  | DstAlpha
  | OneMinusDstAlpha
  | ConstantColor
  | OneMinusConstantColor
  | ConstantAlpha
  | OneMinusConstantAlpha
  | SrcAlphaSaturate

type buffer_bit =
  | ColorBit
  | DepthBit
  | StencilBit

type shader_kind =
  | VertexShader
  | FragmentShader

type shader [@@immediate]

type program [@@immediate]

type attachment =
  | ColorAttachment
  | DepthAttachment
  | StencilAttachment

type framebuffer [@@immediate]

type buffer [@@immediate]

type buffer_target =
  | ArrayBuffer
  | ElementArrayBuffer

type attrib_location [@@immediate]

type uniform_location [@@immediate]

type texture [@@immediate]

type texture_target =
  | Texture2D
  | TextureCubeMapPositiveX
  | TextureCubeMapNegativeX
  | TextureCubeMapPositiveY
  | TextureCubeMapNegativeY
  | TextureCubeMapPositiveZ
  | TextureCubeMapNegativeZ

type texture_format =
  | Alpha
  | Luminance
  | LuminanceAlpha
  | RGB
  | RGBA

type pixel_data_type =
  | UnsignedByte
  | UnsignedShort565
  | UnsignedShort4444
  | UnsignedShort5551

type 'k texture_filter =
  | Nearest : [<`mag|`min] texture_filter
  | Linear : [<`mag|`min] texture_filter
  | NearestMipmapNearest : [`min] texture_filter
  | LinearMipmapNearest : [`min] texture_filter
  | NearestMipmapLinear : [`min] texture_filter
  | LinearMipmapLinear : [`min] texture_filter

type texture_wrap =
  | ClampToEdge
  | MirroredRepeat
  | Repeat

type 'k texture_parameter =
  | MinFilter : [`min] texture_filter texture_parameter
  | MagFilter : [`mag] texture_filter texture_parameter
  | WrapS : texture_wrap texture_parameter
  | WrapT : texture_wrap texture_parameter

type pixel_param =
  | PackAlignment
  | UnpackAlignment

type usage =
  | StreamDraw
  | StaticDraw
  | DynamicDraw

type gl_type =
  | Float
  | UnsignedShort

type draw_mode =
  | Points
  | LineStrip
  | LineLoop
  | Lines
  | TriangleStrip
  | TriangleFan
  | Triangles

let default_framebuffer : framebuffer = Obj.magic 0

open Bigarray

external getError : unit -> int = "caml_glGetError" [@@noalloc]
external enable : cap:capability -> unit = "caml_glEnable" [@@noalloc]
external disable : cap:capability -> unit = "caml_glDisable" [@@noalloc]
external blendFunc : sfactor:blend_function -> dfactor:blend_function -> unit
  = "caml_glBlendFunc" [@@noalloc]
external clear : buffers:buffer_bit list -> unit = "caml_glClear" [@@noalloc]
external viewport : x:int -> y:int -> width:int -> height:int -> unit
  = "caml_glViewport" [@@noalloc]
external createShader : shader_kind:shader_kind -> shader
  = "caml_glCreateShader" [@@noalloc]
external shaderSource : shader:shader -> source:string -> unit
  = "caml_glShaderSource" [@@noalloc]
external compileShader : shader:shader -> unit
  = "caml_glCompileShader" [@@noalloc]
external getShaderInfoLog : shader:shader -> string option
  = "caml_glGetShaderInfoLog"
external createProgram : unit -> program = "caml_glCreateProgram" [@@noalloc]
external attachShader : program:program -> shader:shader -> unit
  = "caml_glAttachShader" [@@noalloc]
external linkProgram : program:program -> unit
  = "caml_glLinkProgram" [@@noalloc]
external getProgramInfoLog : program:program -> string option
  = "caml_glGetProgramInfoLog"
external useProgram : program:program -> unit = "caml_glUseProgram" [@@noalloc]
external genFramebuffer : unit -> framebuffer
  = "caml_glGenFramebuffer" [@@noalloc]
external framebufferTexture2D :
  attachment:attachment -> textarget:texture_target -> texture:texture
  -> level:int -> unit
  = "caml_glFramebufferTexture2D" [@@noalloc]
external bindFramebuffer : framebuffer:framebuffer -> unit
  = "caml_glBindFramebuffer" [@@noalloc]
external genBuffer : unit -> buffer = "caml_glGenBuffer" [@@noalloc]
external deleteBuffer : buffer:buffer -> unit
  = "caml_glDeleteBuffer" [@@noalloc]
external bindBuffer : target:buffer_target -> buffer:buffer -> unit
  = "caml_glBindBuffer" [@@noalloc]
external bufferData :
  target:buffer_target -> data:('a, 'b, c_layout) Array1.t -> usage:usage
  -> unit
  = "caml_glBufferData" [@@noalloc]
external activeTexture : index:int -> unit = "caml_glActiveTexture" [@@noalloc]
external genTexture : unit -> texture = "caml_glGenTexture" [@@noalloc]
external bindTexture : target:texture_target -> texture:texture -> unit
  = "caml_glBindTexture" [@@noalloc]
external texImage2D :
  target:texture_target -> level:int -> internal_format:texture_format
  -> width:int -> height:int -> border:int -> format:texture_format
  -> ptype:pixel_data_type -> unit
  = "caml_glTexImage2D_byte" "caml_glTexImage2D"
external texSubImage2D :
  target:texture_target -> level:int -> x_offset:int -> y_offset:int
  -> format:texture_format -> ptype:pixel_data_type
  -> data:('a, 'b, c_layout) Array2.t -> unit
  = "caml_glTexSubImage2D_byte" "caml_glTexSubImage2D"
external texParameter :
  target:texture_target -> pname:'a texture_parameter -> param:'a -> unit
  = "caml_glTexParameter" [@@noalloc]
external pixelStorei : pname:pixel_param -> param:int -> unit
  = "caml_glPixelStorei" [@@noalloc]
external getAttribLocation : program:program -> name:string -> attrib_location
  = "caml_glGetAttribLocation" [@@noalloc]
external getUniformLocation : program:program -> name:string -> uniform_location
  = "caml_glGetUniformLocation" [@@noalloc]
external enableVertexAttribArray : location:attrib_location -> unit
  = "caml_glEnableVertexAttribArray" [@@noalloc]
external disableVertexAttribArray : location:attrib_location -> unit
  = "caml_glDisableVertexAttribArray" [@@noalloc]
external vertexAttribPointer :
  location:attrib_location -> size:int -> gl_type:gl_type -> normalized:bool
  -> stride:int -> pointer:int -> unit
  = "caml_glVertexAttribPointer_byte" "caml_glVertexAttribPointer" [@@noalloc]
external uniform1i : location:uniform_location -> v:int -> unit
  = "caml_glUniform1i" [@@noalloc]
external uniform1f : location:uniform_location -> v:float -> unit
  = "caml_glUniform1f" [@@noalloc]
external uniform2f : location:uniform_location -> v1:float -> v2:float -> unit
  = "caml_glUniform2f" [@@noalloc]
external uniform3f :
  location:uniform_location -> v1:float -> v2:float -> v3:float -> unit
  = "caml_glUniform3f" [@@noalloc]
external uniform4f :
  location:uniform_location -> v1:float -> v2:float -> v3:float -> v4:float
  -> unit
  = "caml_glUniform4f" [@@noalloc]
external drawArrays : mode:draw_mode -> first:int -> count:int -> unit
  = "caml_glDrawArrays" [@@noalloc]
external drawElements :
  mode:draw_mode -> count:int -> gl_type:gl_type -> indices:int -> unit
  = "caml_glDrawElements" [@@noalloc]
