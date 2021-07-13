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

    persistent_data: (int64, int64_elt, c_layout) Array1.t;
  }

type persistent_field =
  | RTC_Origin

external int_of_persistent_field : persistent_field -> int = "%identity"

let get_persistent_field memory field =
  memory.persistent_data.{int_of_persistent_field field}

let set_persistent_field memory field value =
  memory.persistent_data.{int_of_persistent_field field} <- value

let init_from_rom filename =
  let open Unix in
  let rom_fd = openfile filename [O_RDONLY] 0 in
  let rom_0 = array1_of_genarray (map_file rom_fd Int8_unsigned C_layout false [| 16384 |]) in
  let rom_size = rom_0.{0x148} in
  let ram_size = rom_0.{0x149} in
  let rom_banks =
    Array.init (2 lsl rom_size) (fun i ->
        if i = 0 then rom_0
        else array1_of_genarray (map_file rom_fd ~pos:(Int64.of_int (16384 * i)) Int8_unsigned C_layout false [| 16384 |]))
  in
  let ram_video_banks = Array.init 2 (fun _ -> Array1.create Int8_unsigned C_layout 8192) in
  let ram_ext_bank_count = if ram_size >= 2 then 1 lsl ((ram_size - 2) * 2) else 0 in
  let persistent_data_fd, persistant_data_file_created =
    let persistent_data_filename = filename ^ ".sav" in
    try
      openfile persistent_data_filename [O_RDWR] 0, false
    with Unix_error (ENOENT, _, _) ->
      openfile persistent_data_filename [O_RDWR; O_CREAT] 0o666, true
  in
  ftruncate persistent_data_fd (ram_ext_bank_count * 8192 + 8);
  let ram_ext_banks =
    if ram_ext_bank_count = 0
    then [| Array1.create Int8_unsigned C_layout 8192 |]
    else (
      Array.init ram_ext_bank_count (fun i ->
          array1_of_genarray (map_file persistent_data_fd ~pos:(Int64.of_int (8192 * i)) Int8_unsigned C_layout true [| 8192 |]))
    )
  in
  let ram_work_banks = Array.init 8 (fun _ -> Array1.create Int8_unsigned C_layout 4096) in
  let oam = Array1.create Int8_unsigned C_layout 160 in
  let io_registers = Array1.create Int8_unsigned C_layout 128 in
  Array1.fill io_registers 0;
  io_registers.{0x00} <- -1;
  io_registers.{0x0f} <- 0xe0;
  io_registers.{0x55} <- -1;
  let ram_high = Array1.create Int8_unsigned C_layout 128 in
  let bg_palette_data = Array1.create Int8_unsigned C_layout 64 in
  Array1.fill bg_palette_data ~-1;
  let obj_palette_data = Array1.create Int8_unsigned C_layout 64 in
  let persistent_data = array1_of_genarray (map_file persistent_data_fd ~pos:(Int64.of_int (8192 * ram_ext_bank_count)) Int64 C_layout true [| 1 |]) in
  if persistant_data_file_created then (
    persistent_data.{int_of_persistent_field RTC_Origin} <- Int64.of_float (time ())
  );
  close rom_fd;
  close persistent_data_fd;
  { rom_0; rom_n = rom_banks.(1); ram_video_n = ram_video_banks.(0); ram_ext_n = ram_ext_banks.(0);
    ram_work_0 = ram_work_banks.(0); ram_work_n = ram_work_banks.(1); oam; io_registers; ram_high;
    rom_banks; ram_video_banks; ram_ext_banks; ram_work_banks; bg_palette_data; obj_palette_data;
    persistent_data }
