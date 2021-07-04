open Bigarray

type t = {
    rom_0: (int, int8_unsigned_elt, c_layout) Array1.t;
    mutable rom_n: (int, int8_unsigned_elt, c_layout) Array1.t;
    mutable ram_video_n: (int, int8_unsigned_elt, c_layout) Array1.t;
    mutable ram_ext_n: (int, int8_unsigned_elt, c_layout) Array1.t;
    ram_work_0: (int, int8_unsigned_elt, c_layout) Array1.t;
    mutable ram_work_n: (int, int8_unsigned_elt, c_layout) Array1.t;
    oam: (int, int8_unsigned_elt, c_layout) Array1.t;
    io_registers: (int, int8_unsigned_elt, c_layout) Array1.t;
    ram_high: (int, int8_unsigned_elt, c_layout) Array1.t; (* + Interrupt Enable register at end *)

    rom_banks: (int, int8_unsigned_elt, c_layout) Array1.t array;
    ram_video_banks: (int, int8_unsigned_elt, c_layout) Array1.t array;
    ram_ext_banks: (int, int8_unsigned_elt, c_layout) Array1.t array;
    ram_work_banks: (int, int8_unsigned_elt, c_layout) Array1.t array;

    bg_palette_data: (int, int8_unsigned_elt, c_layout) Array1.t;
    obj_palette_data: (int, int8_unsigned_elt, c_layout) Array1.t;
  }

let init_from_rom filename =
  let open Unix in
  let fd = openfile filename [O_RDONLY] 0 in
  let rom_0 = array1_of_genarray (map_file fd Int8_unsigned C_layout false [| 16384 |]) in
  let rom_size = rom_0.{0x148} in
  let ram_size = rom_0.{0x149} in
  let rom_banks =
    Array.init (2 lsl rom_size) (fun i ->
        if i = 0 then rom_0
        else array1_of_genarray (map_file fd ~pos:(Int64.of_int (16384 * i)) Int8_unsigned C_layout false [| 16384 |]))
  in
  let ram_video_banks = Array.init 2 (fun _ -> Array1.create Int8_unsigned C_layout 8192) in
  let ram_ext_banks =
    if ram_size < 2
    then [| Array1.create Int8_unsigned C_layout 0 |]
    else
      (* TODO: back external ram with memory-mapped files for persistency. *)
      Array.init (1 lsl ((ram_size - 2) * 2)) (fun _ -> Array1.create Int8_unsigned C_layout 8192)
  in
  let ram_work_banks = Array.init 8 (fun _ -> Array1.create Int8_unsigned C_layout 4096) in
  let oam = Array1.create Int8_unsigned C_layout 160 in
  let io_registers = Array1.create Int8_unsigned C_layout 128 in
  Array1.fill io_registers 0;
  io_registers.{0x00} <- -1;
  let ram_high = Array1.create Int8_unsigned C_layout 128 in
  let bg_palette_data = Array1.create Int8_unsigned C_layout 64 in
  Array1.fill bg_palette_data ~-1;
  let obj_palette_data = Array1.create Int8_unsigned C_layout 64 in
  { rom_0; rom_n = rom_banks.(1); ram_video_n = ram_video_banks.(0); ram_ext_n = ram_ext_banks.(0);
    ram_work_0 = ram_work_banks.(0); ram_work_n = ram_work_banks.(1); oam; io_registers; ram_high;
    rom_banks; ram_video_banks; ram_ext_banks; ram_work_banks; bg_palette_data; obj_palette_data }
