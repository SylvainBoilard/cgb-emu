open Utils
open Bigarray
open Memory

type t = {
    program : GL.program;
    vertex_buffer : GL.buffer;
    lcd_texture : GL.texture;
    line_data : (int, int16_unsigned_elt, c_layout) Array1.t;
    vertex_position_attrib_pos : GL.attrib_location;
    vertex_texture_coords_attrib_pos : GL.attrib_location;
    texture_uniform_pos : GL.uniform_location;
  }

let create () =
  let program = load_program "shaders" "lcd" in
  let vertex_buffer = GL.genBuffer () in
  let vertex_data =
    Array1.of_array Float32 C_layout [|
        -1.0;  1.0;   0.0; 0.0;
        -1.0; -1.0;   0.0; 1.0;
         1.0; -1.0;   1.0; 1.0;
         1.0;  1.0;   1.0; 0.0;
      |]
  in
  GL.bindBuffer GL.ArrayBuffer vertex_buffer;
  GL.bufferData GL.ArrayBuffer vertex_data GL.StaticDraw;
  let lcd_texture = GL.genTexture () in
  GL.bindTexture GL.Texture2D lcd_texture;
  GL.texImage2D GL.Texture2D 0 GL.RGBA 160 144 0 GL.RGBA GL.UnsignedShort5551;
  GL.texParameter GL.Texture2D GL.MinFilter GL.Nearest;
  GL.texParameter GL.Texture2D GL.MagFilter GL.Nearest;
  let line_data = Array1.create Int16_unsigned C_layout 160 in
  { program; vertex_buffer; lcd_texture; line_data;
    vertex_position_attrib_pos = GL.getAttribLocation program "VertexPosition";
    vertex_texture_coords_attrib_pos = GL.getAttribLocation program "VertexTextureCoords";
    texture_uniform_pos = GL.getUniformLocation program "LcdTexture" }

let get_color_index memory tile_index signed_index attrs px py =
  assert (px >= 0 && px < 8 && py >= 0 && py < 8);
  let px = if attrs land 0x20 = 0 then px else 7 - px in
  let py = if attrs land 0x40 = 0 then py else 7 - py in
  let vram = memory.ram_video_banks.(attrs lsr 3 land 0x01) in
  let offset =
    if signed_index
    then 0x1000 + signed_int8 tile_index * 16 + py * 2
    else tile_index * 16 + py * 2
  in
  let shift = 7 - px in
  (vram.{offset + 1} lsr shift land 0x01) lsl 1 lor (vram.{offset} lsr shift land 0x01)

let get_color palette index =
  let index = index * 2 in
  palette.{index + 1} lsl 8 lor palette.{index}
  
let render_line lcd memory lcd_y =
  let scroll_x, scroll_y = memory.io_registers.{0x43}, memory.io_registers.{0x42} in
  let window_x, window_y = memory.io_registers.{0x4b} - 7, memory.io_registers.{0x4a} in
  let world_y = (lcd_y + scroll_y) land 0xff in
  let lcdc = memory.io_registers.{0x40} in
  let bg_win_master_prio = lcdc land 0x01 <> 0 in
  let obj_enable = lcdc land 0x02 <> 0 in
  let bigger_sprites = lcdc land 0x04 <> 0 in
  let bg_base_tile_offset = if lcdc land 0x08 = 0 then 0x1800 else 0x1c00 in
  let bg_win_signed_indices = lcdc land 0x10 = 0 in
  let win_enable = lcdc land 0x20 <> 0 && window_y <= lcd_y in
  let win_base_tile_offset = if lcdc land 0x40 = 0 then 0x1800 else 0x1c00 in
  let sprites =
    let sprites_height = if bigger_sprites then 16 else 8 in
    let rec loop acc count = function
      | _ when count = 10 -> acc
      | 160 -> acc
      | i ->
         let y_pos = memory.oam.{i} - 16 in
         if y_pos <= lcd_y && lcd_y < y_pos + sprites_height
         then loop (i :: acc) (count + 1) (i + 4)
         else loop acc count (i + 4)
    in
    if obj_enable then loop [] 0 0 else []
  in
  let render_pixel lcd_x =
    let world_x = (lcd_x + scroll_x) land 0xff in
    let bg_win_color_index, bg_win_attrs =
      let x, y, base_tile_offset =
        if win_enable && window_x <= lcd_x
        then lcd_x - window_x, lcd_y - window_y, win_base_tile_offset
        else world_x, world_y, bg_base_tile_offset
      in
      let tile_offset = base_tile_offset + (x land 0xf8) lsr 3 lor (y land 0xf8) lsl 2 in
      let tile_index = memory.ram_video_banks.(0).{tile_offset} in
      let attrs = memory.ram_video_banks.(1).{tile_offset} in
      get_color_index memory tile_index bg_win_signed_indices attrs (x land 0x07) (y land 0x07), attrs
    in
    let rec aux = function
      | [] -> 0, 0
      | hd :: tl ->
         let x_pos = memory.oam.{hd + 1} - 8 in
         if x_pos <= lcd_x && lcd_x < x_pos + 8 then (
           let y_pos = memory.oam.{hd} - 16 in
           let px = lcd_x - x_pos in
           let py, tile_index =
             if not bigger_sprites
             then lcd_y - y_pos, memory.oam.{hd + 2}
             else if lcd_y < y_pos + 8
             then lcd_y - y_pos, memory.oam.{hd + 2} land 0xfe
             else lcd_y - y_pos - 8, memory.oam.{hd + 2} lor 0x01
           in
           let attrs = memory.oam.{hd + 3} in
           get_color_index memory tile_index false attrs px py, attrs
         ) else aux tl
    in
    let obj_color_index, obj_attrs = aux sprites in
    if obj_color_index = 0
       || bg_win_master_prio
          && (bg_win_attrs land 0x80 <> 0
              || (bg_win_color_index <> 0 && obj_attrs land 0x80 <> 0))
    then get_color memory.bg_palette_data ((bg_win_attrs lsl 2 land 0x1d) lor bg_win_color_index)
    else get_color memory.obj_palette_data ((obj_attrs lsl 2 land 0x1d) lor obj_color_index)
  in
  if lcdc land 0x80 = 0
  then Array1.fill lcd.line_data 0
  else
    (* The CGB stores colors in reverse order, so we should be using texSubImage2D
       with something like ABGR and UnsignedShort1555Rev but that is not possible
       in GLES without using an extension (which is not available on my computer
       setup). Instead we shift the color data one bit to the left in order to align
       the color components correctly, and we reorder them in the fragment shader. *)
    for lcd_x = 0 to 159 do
      let color = render_pixel lcd_x in
      lcd.line_data.{lcd_x} <- color lsl 1
    done;
  GL.texSubImage2D GL.Texture2D 0 0 lcd_y GL.RGBA GL.UnsignedShort5551 (reshape_2 (genarray_of_array1 lcd.line_data) 1 160)

let render_frame lcd =
  GL.useProgram lcd.program;
  GL.activeTexture 0;
  GL.bindTexture GL.Texture2D lcd.lcd_texture;
  GL.uniform1i lcd.texture_uniform_pos 0;
  GL.bindBuffer GL.ArrayBuffer lcd.vertex_buffer;
  GL.vertexAttribPointer lcd.vertex_position_attrib_pos 2 GL.Float false 16 0;
  GL.enableVertexAttribArray lcd.vertex_position_attrib_pos;
  GL.vertexAttribPointer lcd.vertex_texture_coords_attrib_pos 2 GL.Float false 16 8;
  GL.enableVertexAttribArray lcd.vertex_texture_coords_attrib_pos;
  GL.drawArrays GL.TriangleFan 0 4;
  GL.disableVertexAttribArray lcd.vertex_texture_coords_attrib_pos;
  GL.disableVertexAttribArray lcd.vertex_position_attrib_pos;
