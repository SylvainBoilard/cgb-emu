let trace = ref false

let load_program directory name =
  let filename_prefix = Printf.sprintf "%s/%s" directory name in
  let load_shader kind ext =
    let shader = GL.createShader kind in
    begin
      try
        let file = open_in (filename_prefix ^ ext) in
        let shader_src = really_input_string file (in_channel_length file) in
        close_in_noerr file;
        GL.shaderSource shader shader_src;
        GL.compileShader shader;
        Option.iter
          (fun s ->
            Printf.eprintf "=== %s shader compilation log ===\n%!" (name ^ ext);
            Printf.eprintf "%s\n%!" s)
          (GL.getShaderInfoLog shader)
      with Sys_error str -> Printf.eprintf "%s\n%!" str
    end;
    shader
  in
  let vertex_shader = load_shader GL.VertexShader ".vert" in
  let fragment_shader = load_shader GL.FragmentShader ".frag" in
  let program = GL.createProgram () in
  GL.attachShader program vertex_shader;
  GL.attachShader program fragment_shader;
  GL.linkProgram program;
  Option.iter
    (fun s ->
      Printf.eprintf "=== %s program linkage log ===\n%!" name;
      Printf.eprintf "%s\n%!" s)
    (GL.getProgramInfoLog program);
  program

let signed_int8 v = Sys.opaque_identity (v lsl 55) asr 55
