open Utils
open Bigarray
open Memory

type data_kind = Undecided | Instruction | Data
external int_of_data_kind : data_kind -> int = "%identity"

let data_kind_of_int = function
  | 0 -> Undecided
  | 1 -> Instruction
  | 2 -> Data
  | _ -> invalid_arg "data_kind_of_int"

let string_of_data_kind = function
  | Undecided -> "Undecided"
  | Instruction -> "Instruction"
  | Data -> "Data"

let data_kinds_by_bank = ref []

let get_data_kinds_for_bank bank =
  try List.assq bank !data_kinds_by_bank
  with Not_found ->
    let data_kinds = Array1.create Int8_unsigned C_layout 0x4000 in
    Array1.fill data_kinds (int_of_data_kind Undecided);
    data_kinds_by_bank := (bank, data_kinds) :: !data_kinds_by_bank;
    data_kinds

type t = {
    registers: (int, int8_unsigned_elt, c_layout) Array1.t; (* BC DE HL A(znhc----) *)
    mutable program_ctr: int;
    mutable stack_ptr: int;
    mutable m_cycles: int; (* 2^20 per second *)
    mutable interrupt_master_enable: bool;
    mutable interrupt_master_enable_pending: bool;
    mutable halted: bool;
    mutable divider_register_last_tick: int;
    mutable timer_counter_last_tick: int;

    mutable inputs: int;
    mutable ext_ram_or_timer_enable: bool;
    mutable rtc_selected: int;
    mutable rtc_latched: int;
    mutable rtc_origin: int;
  }

type register = B | C | D | E | H | L | A | F
external int_of_register : register -> int = "%identity"

type wide_register = BC | DE | HL | AF
external int_of_wide_register : wide_register -> int = "%identity"

let ( .%{} ) cpu r = cpu.registers.{int_of_register r}

let ( .%{}<- ) cpu r v = cpu.registers.{int_of_register r} <- v

let ( .%%{} ) cpu rr =
  let rr = int_of_wide_register rr * 2 in
  cpu.registers.{rr} lsl 8 lor cpu.registers.{rr + 1}

let ( .%%{}<- ) cpu rr v =
  let rr = int_of_wide_register rr * 2 in
  cpu.registers.{rr} <- v lsr 8;
  cpu.registers.{rr + 1} <- v

type flag =
  | ZeroFlag
  | SubtractionFlag
  | HalfCarryFlag
  | CarryFlag

let set_flag cpu = function
  | ZeroFlag -> cpu.%{F} <- cpu.%{F} lor 0x80
  | SubtractionFlag -> cpu.%{F} <- cpu.%{F} lor 0x40
  | HalfCarryFlag -> cpu.%{F} <- cpu.%{F} lor 0x20
  | CarryFlag -> cpu.%{F} <- cpu.%{F} lor 0x10

let reset_flag cpu = function
  | ZeroFlag -> cpu.%{F} <- cpu.%{F} land lnot 0x8f
  | SubtractionFlag -> cpu.%{F} <- cpu.%{F} land lnot 0x4f
  | HalfCarryFlag -> cpu.%{F} <- cpu.%{F} land lnot 0x2f
  | CarryFlag -> cpu.%{F} <- cpu.%{F} land lnot 0x1f

let change_flag cpu flag = function
  | true -> set_flag cpu flag
  | false -> reset_flag cpu flag

let get_flag cpu = function
  | ZeroFlag -> cpu.%{F} land 0x80 <> 0
  | SubtractionFlag -> cpu.%{F} land 0x40 <> 0
  | HalfCarryFlag -> cpu.%{F} land 0x20 <> 0
  | CarryFlag -> cpu.%{F} land 0x10 <> 0

let string_of_flags cpu =
  Printf.sprintf "%c%c%c%c"
    (if get_flag cpu ZeroFlag then 'Z' else '-')
    (if get_flag cpu SubtractionFlag then 'N' else '-')
    (if get_flag cpu HalfCarryFlag then 'H' else '-')
    (if get_flag cpu CarryFlag then 'C' else '-')

let create () =
  let registers = Array1.create Int8_unsigned C_layout 8 in
  Array1.fill registers 0;
  registers.{int_of_register A} <- 0x11; (* CGB hardware *)
  { registers; program_ctr = 0x100; stack_ptr = 0xfffe; m_cycles = 0;
    interrupt_master_enable = true; interrupt_master_enable_pending = false; halted = false;
    divider_register_last_tick = 0; timer_counter_last_tick = 0;
    inputs = 0; ext_ram_or_timer_enable = false;
    rtc_selected = -1; rtc_latched = -1; rtc_origin = int_of_float (Unix.time ()) }

let perform_dma_step memory src dst size =
  let dst_sub = Array1.sub memory.ram_video_n (dst land 0x1ff0) size in
  match Char.chr (src lsr 8) with
  | '\x00'..'\x3f' -> Array1.(blit (sub memory.rom_0 (src land 0x3ff0) size) dst_sub)
  | '\x40'..'\x7f' -> Array1.(blit (sub memory.rom_n (src land 0x3ff0) size) dst_sub)
  | '\xa0'..'\xbf' -> Array1.(blit (sub memory.ram_ext_n (src land 0x1ff0) size) dst_sub)
  | '\xc0'..'\xcf' -> Array1.(blit (sub memory.ram_work_0 (src land 0x0ff0) size) dst_sub)
  | '\xd0'..'\xdf' -> Array1.(blit (sub memory.ram_work_n (src land 0x0ff0) size) dst_sub)
  | _ -> Printf.eprintf "perform_dma_step: DMA transfer to VRAM from address 0x%04x not implemented.\n%!" src

let read_8 cpu memory addr =
  cpu.m_cycles <- cpu.m_cycles + 1;
  match addr with
  | _ when addr < 0x0 || addr >= 0x10000 -> invalid_arg "read_8: address out of range"
  | _ when addr < 0x4000 -> memory.rom_0.{addr}
  | _ when addr < 0x8000 -> memory.rom_n.{addr - 0x4000}
  | _ when addr < 0xa000 -> memory.ram_video_n.{addr - 0x8000} (* TODO: inacessible during mode 3 *)
  | _ when addr < 0xc000 ->
     if not cpu.ext_ram_or_timer_enable
     then 0
     else if cpu.rtc_selected < 0
     then memory.ram_ext_n.{addr - 0xa000}
     else (
       let now = if cpu.rtc_latched = -1 then int_of_float (Unix.time ()) else cpu.rtc_latched in
       let delta = now - cpu.rtc_origin in
       match cpu.rtc_selected with
       | 0 -> delta mod 60
       | 1 -> delta / 60 mod 60
       | 2 -> delta / 60 / 60 mod 24
       | 3 -> delta / 60 / 60 / 24 mod 256
       | 4 -> delta / 60 / 60 / 24 / 256 mod 2 (* FIXME: implement halt and carry bits *)
       | n -> Printf.eprintf "read_8: RTC register %d does not exist.\n%!" n; 0
     )
  | _ when addr < 0xd000 -> memory.ram_work_0.{addr - 0xc000}
  | _ when addr < 0xe000 -> memory.ram_work_n.{addr - 0xd000}
  | _ when addr < 0xf000 -> memory.ram_work_0.{addr - 0xe000} (* prohibited; mirror of 0xc000-0xcfff *)
  | _ when addr < 0xfe00 -> memory.ram_work_n.{addr - 0xf000} (* prohibited; mirror of 0xd000-0xddff *)
  | _ when addr < 0xfea0 -> memory.oam.{addr - 0xfe00} (* TODO: inacessible during mode 2 and 3 except by DMA *)
  | _ when addr < 0xff00 -> 0 (* prohibited; 0xff during modes 2 and 3, can vary otherwise *)
  | 0xff00 ->
     memory.io_registers.{0x00}
     land (if memory.io_registers.{0x00} land 0x10 = 0 then lnot (cpu.inputs land 0x0f) else 0xff)
     land (if memory.io_registers.{0x00} land 0x20 = 0 then lnot (cpu.inputs lsr 4 land 0x0f) else 0xff)
  | 0xff01 | 0xff02 -> memory.io_registers.{addr - 0xff00} (* SILENCE: serial port *)
  | 0xff56 -> memory.io_registers.{addr - 0xff00} (* SILENCE: infrared port *)
  | 0xff69 -> memory.bg_palette_data.{memory.io_registers.{0x68} land 0x3f} (* TODO: inacessible during mode 3 *)
  | 0xff6b -> memory.obj_palette_data.{memory.io_registers.{0x6a} land 0x3f} (* TODO: inacessible during mode 3 *)
  | 0xff04 | 0xff05 | 0xff06 | 0xff07 | 0xff0f
    | 0xff40 | 0xff41 | 0xff42 | 0xff43 | 0xff44 | 0xff45 | 0xff46 | 0xff4a | 0xff4b | 0xff4d | 0xff4f
    | 0xff51 | 0xff52 | 0xff53 | 0xff54 | 0xff55 | 0xff68 | 0xff6a | 0xff70 ->
     memory.io_registers.{addr - 0xff00}
  | 0xff47 | 0xff48 | 0xff49 -> memory.io_registers.{addr - 0xff00} (* SILENCE: Grayscale palettes *)
  | _ when addr >= 0xff10 && addr < 0xff40 -> memory.io_registers.{addr - 0xff00} (* SILENCE: sound *)
  | _ when addr >= 0xff80 -> memory.ram_high.{addr - 0xff80}
  | _ -> Printf.eprintf "read_8: 0x%04x is outside implemented range.\n%!" addr; 0

let read_8_immediate ?(kind=Data) cpu memory =
  let addr = cpu.program_ctr in
  if addr < 0x8000 then (
    let data_kinds =
      if addr < 0x4000
      then get_data_kinds_for_bank memory.rom_0
      else get_data_kinds_for_bank memory.rom_n
    in
    let current_kind = data_kind_of_int data_kinds.{addr land 0x3fff} in
    if current_kind <> Undecided && current_kind <> kind then
      Printf.eprintf "Data kind at address 0x%04x changed from %s to %s.\n%!"
        addr (string_of_data_kind current_kind) (string_of_data_kind kind);
    data_kinds.{addr land 0x3fff} <- int_of_data_kind kind
  );
  cpu.program_ctr <- addr + 1;
  read_8 cpu memory addr

let read_16 cpu memory addr =
  read_8 cpu memory (addr + 1) lsl 8 lor read_8 cpu memory addr

let read_16_immediate cpu memory =
  let low = read_8_immediate cpu memory in
  read_8_immediate cpu memory lsl 8 lor low

let write_8 cpu memory addr value =
  cpu.m_cycles <- cpu.m_cycles + 1;
  match addr with
  | _ when addr < 0x0 || addr >= 0x10000 -> invalid_arg "write_8: address out of range"
  | _ when addr < 0x2000 -> cpu.ext_ram_or_timer_enable <- value land 0x0f = 0x0a
  | _ when addr < 0x4000 -> memory.rom_n <- memory.rom_banks.(max 1 value land 0x7f)
  | _ when addr < 0x6000 ->
     if value land 0x08 = 0 then (
       cpu.rtc_selected <- -1;
       memory.ram_ext_n <- memory.ram_ext_banks.(value land 0x03)
     ) else cpu.rtc_selected <- value land 0x07
  | _ when addr < 0x8000 ->
     if value land 0x01 = 0
     then cpu.rtc_latched <- -1
     else if cpu.rtc_latched = -1
     then cpu.rtc_latched <- int_of_float (Unix.time ())
  | _ when addr < 0xa000 -> memory.ram_video_n.{addr - 0x8000} <- value (* TODO: inacessible during mode 3 *)
  | _ when addr < 0xc000 ->
     if not cpu.ext_ram_or_timer_enable
     then ()
     else if cpu.rtc_selected < 0
     then memory.ram_ext_n.{addr - 0xa000} <- value
     else (
       let now = int_of_float (Unix.time ()) in
       let delta = now - cpu.rtc_origin in
       match cpu.rtc_selected with
       | 0 -> cpu.rtc_origin <- cpu.rtc_origin + delta mod 60 - value mod 60
       | 1 -> cpu.rtc_origin <- cpu.rtc_origin + (delta / 60 mod 60 - value mod 60) * 60
       | 2 -> cpu.rtc_origin <- cpu.rtc_origin + (delta / 60 / 60 mod 24 - value mod 24) * 60 * 60
       | 3 -> cpu.rtc_origin <- cpu.rtc_origin + (delta / 60 / 60 / 24 mod 256 - value mod 256) * 60 * 60 * 24
       | 4 -> cpu.rtc_origin <- cpu.rtc_origin + (delta / 60 / 60 / 24 / 256 mod 2 - value mod 2) * 60 * 60 * 24 * 256;
       (* FIXME: implement halt and carry bits *)
       | n -> Printf.eprintf "write_8: RTC register %d does not exist.\n%!" n
     )
  | _ when addr < 0xd000 -> memory.ram_work_0.{addr - 0xc000} <- value
  | _ when addr < 0xe000 -> memory.ram_work_n.{addr - 0xd000} <- value
  | _ when addr < 0xf000 -> memory.ram_work_0.{addr - 0xe000} <- value (* prohibited; mirror of 0xc000-0xcfff *)
  | _ when addr < 0xfe00 -> memory.ram_work_n.{addr - 0xf000} <- value (* prohibited; mirror of 0xd000-0xddff *)
  | _ when addr < 0xfea0 -> memory.oam.{addr - 0xfe00} <- value (* TODO: inacessible during mode 2 and 3 *)
  | _ when addr < 0xff00 -> () (* prohibited *)
  | 0xff00 (* Inputs *) -> memory.io_registers.{0x00} <- value lor lnot 0x30
  | 0xff01 | 0xff02 (* Serial port *) -> () (* SILENCE: serial port *)
  | 0xff04 (* Divider Register *) ->
     cpu.divider_register_last_tick <- cpu.m_cycles;
     cpu.timer_counter_last_tick <- cpu.m_cycles;
     memory.io_registers.{0x04} <- 0
  | 0xff05 (* Timer Counter *) -> memory.io_registers.{0x05} <- value
  | 0xff06 (* Timer Modulo *) -> memory.io_registers.{0x06} <- value
  | 0xff07 (* Timer Control *) -> memory.io_registers.{0x07} <- value land 0x07
  | 0xff0f (* Interrupt Flag *) -> memory.io_registers.{0x0f} <- value land 0x1f
  | _ when addr >= 0xff10 && addr < 0xff40 (* Sound *) -> memory.io_registers.{addr - 0xff00} <- value (* SILENCE: sound *)
  | 0xff40 (* LCD Control *) -> memory.io_registers.{0x40} <- value
  | 0xff41 (* LCD Status *) -> memory.io_registers.{0x41} <- (value land 0x78) lor (memory.io_registers.{0x41} land 0x07)
  | 0xff42 (* Scroll Y *) -> memory.io_registers.{0x42} <- value
  | 0xff43 (* Scroll X *) -> memory.io_registers.{0x43} <- value
  | 0xff44 (* LCD Y Coordinate *) ->
     Printf.eprintf "write_8: attempted write to 0xff44 (read-only, LCD Y coordinate).\n%!"
  | 0xff45 (* LCD Y Compare *) -> memory.io_registers.{0x45} <- value
  | 0xff46 (* DMA Transfer and Start Address *) ->
     memory.io_registers.{0x46} <- value;
     begin match Char.chr value with
     | '\x00'..'\x3f' -> Array1.(blit (sub memory.rom_0 (value lsl 8 land 0x3ff0) 160) memory.oam)
     | '\x40'..'\x7f' -> Array1.(blit (sub memory.rom_n (value lsl 8 land 0x3ff0) 160) memory.oam)
     | '\xa0'..'\xbf' -> Array1.(blit (sub memory.ram_ext_n (value lsl 8 land 0x1ff0) 160) memory.oam)
     | '\xc0'..'\xcf' -> Array1.(blit (sub memory.ram_work_0 (value lsl 8 land 0x0ff0) 160) memory.oam)
     | '\xd0'..'\xdf' -> Array1.(blit (sub memory.ram_work_n (value lsl 8 land 0x0ff0) 160) memory.oam)
     | _ -> Printf.eprintf "write_8: DMA transfer to OAM from address 0x%02x00 not implemented.\n%!" (value lsl 8)
     end
  | 0xff47 | 0xff48 | 0xff49 -> memory.io_registers.{addr - 0xff00} <- value (* SILENCE: Grayscale palettes *)
  | 0xff4a (* Window Y Position *) -> memory.io_registers.{0x4a} <- value
  | 0xff4b (* Window X Position + 7 *) -> memory.io_registers.{0x4b} <- value
  | 0xff4d (* Prepare Speed Switch *) ->
     memory.io_registers.{0x4d} <- (memory.io_registers.{0x4d} land 0xfe) lor (value land 0x01)
  | 0xff4f (* VRAM Bank *) ->
     memory.ram_video_n <- memory.ram_video_banks.(value land 0x01);
     memory.io_registers.{0x4f} <- value lor 0xfe
  | 0xff51 (* DMA Source, high *) -> memory.io_registers.{0x51} <- value
  | 0xff52 (* DMA Source, low *) -> memory.io_registers.{0x52} <- value
  | 0xff53 (* DMA Destination, high *) -> memory.io_registers.{0x53} <- value
  | 0xff54 (* DMA Destination, low *) -> memory.io_registers.{0x54} <- value
  | 0xff55 (* DMA Control *) ->
     if value land 0x80 <> 0 then (* hblank DMA *)
       memory.io_registers.{0x55} <- value land 0x7f
     else if memory.io_registers.{0x55} land 0x80 = 0 then (* terminate hblank DMA *)
       memory.io_registers.{0x55} <- memory.io_registers.{0x55} lor 0x80
     else ( (* immediate DMA *)
       let src = memory.io_registers.{0x51} lsl 8 lor memory.io_registers.{0x52} in
       let dst = memory.io_registers.{0x53} lsl 8 lor memory.io_registers.{0x54} in
       let length = (value land 0x7f) lsl 4 + 1 in
       perform_dma_step memory src dst length;
       cpu.m_cycles <- cpu.m_cycles + length / 2;
       memory.io_registers.{0x55} <- 0xff
     )
  | 0xff56 (* Infrared Port *) -> () (* SILENCE: infrared port *)
  | 0xff68 (* Color BG Palette Index *) -> memory.io_registers.{0x68} <- value
  | 0xff69 (* Color BG Palette Data *) -> (* TODO: inacessible during mode 3 *)
     let bgpi = memory.io_registers.{0x68} in
     memory.bg_palette_data.{bgpi land 0x3f} <- value;
     if bgpi land 0x80 <> 0 then memory.io_registers.{0x68} <- (bgpi + 1) land lnot 0x40
  | 0xff6a (* Color OBJ Palette Index *) -> memory.io_registers.{0x6a} <- value
  | 0xff6b (* Color OBJ Palette Data *) -> (* TODO: inacessible during mode 3 *)
     let obpi = memory.io_registers.{0x6a} in
     memory.obj_palette_data.{obpi land 0x3f} <- value;
     if obpi land 0x80 <> 0 then memory.io_registers.{0x6a} <- (obpi + 1) land lnot 0x40
  | 0xff70 (* WRAM Bank *) ->
     let n = value land 0x07 in
     memory.io_registers.{0x70} <- n lor 0xf8;
     memory.ram_work_n <- memory.ram_work_banks.(max 1 n)
  | _ when addr >= 0xff80 -> memory.ram_high.{addr - 0xff80} <- value
  | _ -> Printf.eprintf "write_8: 0x%04x is outside implemented range (value = 0x%02x).\n%!" addr value

let write_16 cpu memory addr value =
  write_8 cpu memory addr value;
  write_8 cpu memory (addr + 1) (value lsr 8)

let execute_cb_prefixed cpu memory =
  let opcode = read_8_immediate cpu memory in
  let operand = match opcode land 0x07 with
    | 0x6 -> read_8 cpu memory cpu.%%{HL}
    | 0x7 -> cpu.%{A}
    | n -> cpu.registers.{n}
  in
  let result = match Char.chr opcode with
    | '\x00'..'\x07' ->
       let c = if get_flag cpu CarryFlag then 0x01 else 0x00 in
       change_flag cpu CarryFlag (operand land 0x80 <> 0);
       operand lsl 1 lor c
    | '\x08'..'\x0f' ->
       let c = if get_flag cpu CarryFlag then 0x80 else 0x00 in
       change_flag cpu CarryFlag (operand land 0x01 <> 0);
       c lor operand lsr 1
    | '\x10'..'\x17' ->
       change_flag cpu CarryFlag (operand land 0x80 <> 0);
       operand lsl 1 lor operand lsr 7
    | '\x18'..'\x1f' ->
       change_flag cpu CarryFlag (operand land 0x01 <> 0);
       operand lsl 7 lor operand lsr 1
    | '\x20'..'\x27' ->
       change_flag cpu CarryFlag (operand land 0x80 <> 0);
       operand lsl 1
    | '\x28'..'\x2f' ->
       change_flag cpu CarryFlag (operand land 0x01 <> 0);
       operand lsr 1 lor (operand land 0x80)
    | '\x30'..'\x37' ->
       reset_flag cpu CarryFlag;
       operand lsl 4 lor operand lsr 4
    | '\x38'..'\x3f' ->
       change_flag cpu CarryFlag (operand land 0x01 <> 0);
       operand lsr 1
    | '\x40'..'\x7f' -> operand land 0x01 lsl (opcode lsr 3 land 0x7)
    | '\x80'..'\xbf' -> operand land lnot (0x01 lsl (opcode lsr 3 land 0x7))
    | '\xc0'..'\xff' -> operand lor 0x01 lsl (opcode lsr 3 land 0x7)
  in
  if opcode < 0x80 then (
    change_flag cpu ZeroFlag (result = 0);
    reset_flag cpu SubtractionFlag;
    change_flag cpu HalfCarryFlag (opcode >= 0x40)
  );
  match opcode land 0x07 with
  | _ when opcode land 0xc0 = 0x40 -> ()
  | 0x6 -> write_8 cpu memory cpu.%%{HL} result
  | 0x7 -> cpu.%{A} <- result
  | n -> cpu.registers.{n} <- result

let jr cpu memory cond =
  let offset = signed_int8 (read_8_immediate cpu memory) in
  if cond then (
    cpu.program_ctr <- cpu.program_ctr + offset;
    cpu.m_cycles <- cpu.m_cycles + 1
  )

let jp cpu memory cond =
  let addr = read_16_immediate cpu memory in
  if cond then (
    cpu.program_ctr <- addr;
    cpu.m_cycles <- cpu.m_cycles + 1
  )

let ld_rr_u16 cpu memory rr =
  cpu.%%{rr} <- read_16_immediate cpu memory

let ld_sp_u16 cpu memory =
  cpu.stack_ptr <- read_16_immediate cpu memory

let ld_r_addr cpu memory r addr =
  cpu.%{r} <- read_8 cpu memory addr

let ld_addr_r cpu memory addr r =
  write_8 cpu memory addr cpu.%{r}

let add_rr_rr cpu rrout rrin =
  let result = cpu.%%{rrout} + cpu.%%{rrin} in
  cpu.%%{rrout} <- result;
  reset_flag cpu SubtractionFlag;
  change_flag cpu HalfCarryFlag (result >= 0x1000);
  change_flag cpu CarryFlag (result >= 0x10000);
  cpu.m_cycles <- cpu.m_cycles + 1

let add_rr_sp cpu rrout =
  let result = cpu.%%{rrout} + cpu.stack_ptr in
  cpu.%%{rrout} <- result;
  reset_flag cpu SubtractionFlag;
  change_flag cpu HalfCarryFlag (result >= 0x1000);
  change_flag cpu CarryFlag (result >= 0x10000);
  cpu.m_cycles <- cpu.m_cycles + 1

let incr_rr cpu rr =
  cpu.%%{rr} <- cpu.%%{rr} + 1;
  cpu.m_cycles <- cpu.m_cycles + 1

let decr_rr cpu rr =
  cpu.%%{rr} <- cpu.%%{rr} - 1;
  cpu.m_cycles <- cpu.m_cycles + 1

let incr_sp cpu =
  cpu.stack_ptr <- cpu.stack_ptr + 1;
  cpu.m_cycles <- cpu.m_cycles + 1

let decr_sp cpu =
  cpu.stack_ptr <- cpu.stack_ptr - 1;
  cpu.m_cycles <- cpu.m_cycles + 1

let incr_r cpu r =
  let result = cpu.%{r} + 1 in
  cpu.%{r} <- result;
  change_flag cpu ZeroFlag (result = 0);
  reset_flag cpu SubtractionFlag;
  change_flag cpu HalfCarryFlag (result >= 0x10)

let decr_r cpu r =
  let result = cpu.%{r} - 1 in
  cpu.%{r} <- result;
  change_flag cpu ZeroFlag (result = 0);
  set_flag cpu SubtractionFlag;
  change_flag cpu HalfCarryFlag (result >= 0x10)

let ld_r_u8 cpu memory r =
  cpu.%{r} <- read_8_immediate cpu memory

let push_rr cpu memory rr =
  cpu.stack_ptr <- cpu.stack_ptr - 2;
  write_16 cpu memory cpu.stack_ptr cpu.%%{rr}

let pop_rr cpu memory rr =
  let value = read_16 cpu memory cpu.stack_ptr in
  cpu.%%{rr} <- (if rr = AF then value land 0xfff0 else value);
  cpu.stack_ptr <- cpu.stack_ptr + 2

let push_pc cpu memory =
  cpu.stack_ptr <- cpu.stack_ptr - 2;
  write_16 cpu memory cpu.stack_ptr cpu.program_ctr

let pop_pc cpu memory =
  cpu.program_ctr <- read_16 cpu memory cpu.stack_ptr;
  cpu.stack_ptr <- cpu.stack_ptr + 2

let ret cpu memory cond =
  if cond then (
    pop_pc cpu memory;
    cpu.m_cycles <- cpu.m_cycles + 2
  ) else cpu.m_cycles <- cpu.m_cycles + 1

let call cpu memory cond =
  let addr = read_16_immediate cpu memory in
  if cond then (
    push_pc cpu memory;
    cpu.program_ctr <- addr;
    cpu.m_cycles <- cpu.m_cycles + 1
  )

let execute cpu memory opcode = match Char.chr opcode with
  | '\xd3' | '\xdb' | '\xdd' | '\xe3' | '\xe4' | '\xeb'..'\xed' | '\xf4' | '\xfc' | '\xfd' ->
     Printf.eprintf "execute: illegal opcode 0x%02x at 0x%04x.\n%!" opcode (cpu.program_ctr - 1)
  | '\x00' -> ()
  | '\x08' -> write_16 cpu memory (read_16_immediate cpu memory) cpu.stack_ptr
  | '\x10' -> (* TODO: implement double speed support *)
     if memory.io_registers.{0x4d} land 0x01 <> 0 then (
       Printf.eprintf "execute: attempted speed switch (unimplemented, triggered by STOP at 0x%04x, masquerading).\n%!"
         (cpu.program_ctr - 1);
       memory.io_registers.{0x4d} <- memory.io_registers.{0x4d} lxor 0x81
     )
  | '\x18' -> jr cpu memory true
  | '\x20' -> jr cpu memory (not (get_flag cpu ZeroFlag))
  | '\x28' -> jr cpu memory (get_flag cpu ZeroFlag)
  | '\x30' -> jr cpu memory (not (get_flag cpu CarryFlag))
  | '\x38' -> jr cpu memory (get_flag cpu CarryFlag)

  | '\x01' -> ld_rr_u16 cpu memory BC
  | '\x09' -> add_rr_rr cpu HL BC
  | '\x11' -> ld_rr_u16 cpu memory DE
  | '\x19' -> add_rr_rr cpu HL DE
  | '\x21' -> ld_rr_u16 cpu memory HL
  | '\x29' -> add_rr_rr cpu HL HL
  | '\x31' -> ld_sp_u16 cpu memory
  | '\x39' -> add_rr_sp cpu HL

  | '\x02' -> ld_addr_r cpu memory cpu.%%{BC} A
  | '\x0a' -> ld_r_addr cpu memory A cpu.%%{BC}
  | '\x12' -> ld_addr_r cpu memory cpu.%%{DE} A
  | '\x1a' -> ld_r_addr cpu memory A cpu.%%{DE}
  | '\x22' ->
     let addr = cpu.%%{HL} in
     ld_addr_r cpu memory addr A;
     cpu.%%{HL} <- addr + 1
  | '\x2a' ->
     let addr = cpu.%%{HL} in
     ld_r_addr cpu memory A addr;
     cpu.%%{HL} <- addr + 1
  | '\x32' ->
     let addr = cpu.%%{HL} in
     ld_addr_r cpu memory addr A;
     cpu.%%{HL} <- addr - 1
  | '\x3a' ->
     let addr = cpu.%%{HL} in
     ld_r_addr cpu memory A addr;
     cpu.%%{HL} <- addr - 1

  | '\x03' -> incr_rr cpu BC
  | '\x0b' -> decr_rr cpu BC
  | '\x13' -> incr_rr cpu DE
  | '\x1b' -> decr_rr cpu DE
  | '\x23' -> incr_rr cpu HL
  | '\x2b' -> decr_rr cpu HL
  | '\x33' -> incr_sp cpu
  | '\x3b' -> decr_sp cpu

  | '\x04' -> incr_r cpu B
  | '\x0c' -> incr_r cpu C
  | '\x14' -> incr_r cpu D
  | '\x1c' -> incr_r cpu E
  | '\x24' -> incr_r cpu H
  | '\x2c' -> incr_r cpu L
  | '\x34' ->
     let addr = cpu.%%{HL} in
     let result = read_8 cpu memory addr + 1 in
     write_8 cpu memory addr result;
     change_flag cpu ZeroFlag (result = 0);
     reset_flag cpu SubtractionFlag;
     change_flag cpu HalfCarryFlag (result >= 0x10)
  | '\x3c' -> incr_r cpu A

  | '\x05' -> decr_r cpu B
  | '\x0d' -> decr_r cpu C
  | '\x15' -> decr_r cpu D
  | '\x1d' -> decr_r cpu E
  | '\x25' -> decr_r cpu H
  | '\x2d' -> decr_r cpu L
  | '\x35' ->
     let addr = cpu.%%{HL} in
     let result = read_8 cpu memory addr - 1 in
     write_8 cpu memory addr result;
     change_flag cpu ZeroFlag (result = 0);
     set_flag cpu SubtractionFlag;
     change_flag cpu HalfCarryFlag (result >= 0x10)
  | '\x3d' -> decr_r cpu A

  | '\x06' -> ld_r_u8 cpu memory B
  | '\x0e' -> ld_r_u8 cpu memory C
  | '\x16' -> ld_r_u8 cpu memory D
  | '\x1e' -> ld_r_u8 cpu memory E
  | '\x26' -> ld_r_u8 cpu memory H
  | '\x2e' -> ld_r_u8 cpu memory L
  | '\x36' -> write_8 cpu memory cpu.%%{HL} (read_8_immediate cpu memory)
  | '\x3e' -> ld_r_u8 cpu memory A

  | '\x07' ->
     let c = if get_flag cpu CarryFlag then 0x01 else 0x00 in
     change_flag cpu CarryFlag (cpu.%{A} land 0x80 <> 0);
     cpu.%{A} <- cpu.%{A} lsl 1 lor c
  | '\x0f' ->
     let c = if get_flag cpu CarryFlag then 0x80 else 0x00 in
     change_flag cpu CarryFlag (cpu.%{A} land 0x01 <> 0);
     cpu.%{A} <- c lor cpu.%{A} lsr 1
  | '\x17' ->
     change_flag cpu CarryFlag (cpu.%{A} land 0x80 <> 0);
     cpu.%{A} <- cpu.%{A} lsl 1 lor cpu.%{A} lsr 7
  | '\x1f' ->
     change_flag cpu CarryFlag (cpu.%{A} land 0x01 <> 0);
     cpu.%{A} <- cpu.%{A} lsl 7 lor cpu.%{A} lsr 1
  | '\x27' -> Printf.eprintf "execute: unimplemented opcode 0x%02x at 0x%04x\n%!" opcode (cpu.program_ctr - 1) (* TODO: DAA *)
  | '\x2f' ->
     set_flag cpu SubtractionFlag;
     set_flag cpu HalfCarryFlag;
     cpu.%{A} <- lnot cpu.%{A}
  | '\x37' ->
     reset_flag cpu SubtractionFlag;
     reset_flag cpu HalfCarryFlag;
     set_flag cpu CarryFlag
  | '\x3f' ->
     reset_flag cpu SubtractionFlag;
     reset_flag cpu HalfCarryFlag;
     change_flag cpu CarryFlag (not (get_flag cpu CarryFlag))

  | '\x76' -> cpu.halted <- true
  | '\x40'..'\x7f' (* 8-bit load *) ->
     let src_data = match opcode land 0x7 with
       | 0x6 -> read_8 cpu memory cpu.%%{HL}
       | 0x7 -> cpu.%{A}
       | n -> cpu.registers.{n}
     in
     begin match opcode lsr 3 land 0x7 with
     | 0x6 -> write_8 cpu memory cpu.%%{HL} src_data
     | 0x7 -> cpu.%{A} <- src_data
     | n -> cpu.registers.{n} <- src_data
     end

  | '\x80'..'\xbf' (* 8-bit alu *) ->
     let operator, operand  = opcode lsr 3 land 0x7, opcode land 0x7 in
     let operand_data = match operand with
       | 0x6 -> read_8 cpu memory cpu.%%{HL}
       | 0x7 -> cpu.%{A}
       | n -> cpu.registers.{n}
     in
     let result = match operator with
       | 0x0 -> cpu.%{A} + operand_data
       | 0x1 -> cpu.%{A} + operand_data + Bool.to_int (get_flag cpu CarryFlag)
       | 0x2 -> cpu.%{A} - operand_data
       | 0x3 -> cpu.%{A} - operand_data - Bool.to_int (get_flag cpu CarryFlag)
       | 0x4 -> cpu.%{A} land operand_data
       | 0x5 -> cpu.%{A} lxor operand_data
       | 0x6 -> cpu.%{A} lor operand_data
       | 0x7 -> cpu.%{A} - operand_data
       | _ -> assert false
     in
     if operator <> 0x7 then cpu.%{A} <- result;
     change_flag cpu ZeroFlag (result = 0);
     change_flag cpu SubtractionFlag (match operator with 0x2 | 0x3 | 0x7 -> true | _ -> false);
     change_flag cpu HalfCarryFlag (match operator with 0x4 -> true | 0x5 | 0x6 -> false | _ -> result >= 0x10);
     change_flag cpu CarryFlag (result >= 256 || result < 0)

  | '\xc0' -> ret cpu memory (not (get_flag cpu ZeroFlag))
  | '\xc8' -> ret cpu memory (get_flag cpu ZeroFlag)
  | '\xd0' -> ret cpu memory (not (get_flag cpu CarryFlag))
  | '\xd8' -> ret cpu memory (get_flag cpu CarryFlag)
  | '\xe0' -> write_8 cpu memory (0xff00 + read_8_immediate cpu memory) cpu.%{A}
  | '\xe8' ->
     let result = cpu.stack_ptr + signed_int8 (read_8_immediate cpu memory) in
     cpu.stack_ptr <- result land 0xffff;
     cpu.m_cycles <- cpu.m_cycles + 2;
     reset_flag cpu ZeroFlag;
     reset_flag cpu SubtractionFlag;
     change_flag cpu HalfCarryFlag (result >= 0x1000);
     change_flag cpu CarryFlag (result < 0 || result >= 0x10000)
  | '\xf0' -> cpu.%{A} <- read_8 cpu memory (0xff00 + read_8_immediate cpu memory)
  | '\xf8' ->
     let result = cpu.stack_ptr + signed_int8 (read_8_immediate cpu memory) in
     cpu.%%{HL} <- result;
     cpu.m_cycles <- cpu.m_cycles + 1;
     reset_flag cpu ZeroFlag;
     reset_flag cpu SubtractionFlag;
     change_flag cpu HalfCarryFlag (result >= 0x1000);
     change_flag cpu CarryFlag (result < 0 || result >= 0x10000)

  | '\xc1' -> pop_rr cpu memory BC
  | '\xc9' -> ret cpu memory true
  | '\xd1' -> pop_rr cpu memory DE
  | '\xd9' ->
     ret cpu memory true;
     cpu.interrupt_master_enable_pending <- true;
  | '\xe1' -> pop_rr cpu memory HL
  | '\xe9' -> cpu.program_ctr <- cpu.%%{HL}
  | '\xf1' -> pop_rr cpu memory AF
  | '\xf9' ->
     cpu.stack_ptr <- cpu.%%{HL};
     cpu.m_cycles <- cpu.m_cycles + 1

  | '\xc2' -> jp cpu memory (not (get_flag cpu ZeroFlag))
  | '\xca' -> jp cpu memory (get_flag cpu ZeroFlag)
  | '\xd2' -> jp cpu memory (not (get_flag cpu CarryFlag))
  | '\xda' -> jp cpu memory (get_flag cpu CarryFlag)
  | '\xe2' -> ld_addr_r cpu memory (0xff00 + cpu.%{C}) A
  | '\xea' -> ld_addr_r cpu memory (read_16_immediate cpu memory) A
  | '\xf2' -> ld_r_addr cpu memory A (0xff00 + cpu.%{C})
  | '\xfa' -> ld_r_addr cpu memory A (read_16_immediate cpu memory)

  | '\xc3' -> jp cpu memory true
  | '\xcb' -> execute_cb_prefixed cpu memory
  | '\xf3' ->
     cpu.interrupt_master_enable <- false;
     cpu.interrupt_master_enable_pending <- false;
  | '\xfb' -> cpu.interrupt_master_enable_pending <- true

  | '\xc4' -> call cpu memory (not (get_flag cpu ZeroFlag))
  | '\xcc' -> call cpu memory (get_flag cpu ZeroFlag)
  | '\xd4' -> call cpu memory (not (get_flag cpu CarryFlag))
  | '\xdc' -> call cpu memory (get_flag cpu CarryFlag)

  | '\xc5' -> push_rr cpu memory BC
  | '\xcd' -> call cpu memory true
  | '\xd5' -> push_rr cpu memory DE
  | '\xe5' -> push_rr cpu memory HL
  | '\xf5' -> push_rr cpu memory AF

  | '\xc6' | '\xce' | '\xd6' | '\xde' | '\xe6' | '\xee' | '\xf6' | '\xfe' ->
     let operator = opcode lsr 3 land 0x7 in
     let operand_data = read_8_immediate cpu memory in
     let result = match operator with
       | 0x0 -> cpu.%{A} + operand_data
       | 0x1 -> cpu.%{A} + operand_data + Bool.to_int (get_flag cpu CarryFlag)
       | 0x2 -> cpu.%{A} - operand_data
       | 0x3 -> cpu.%{A} - operand_data - Bool.to_int (get_flag cpu CarryFlag)
       | 0x4 -> cpu.%{A} land operand_data
       | 0x5 -> cpu.%{A} lxor operand_data
       | 0x6 -> cpu.%{A} lor operand_data
       | 0x7 -> cpu.%{A} - operand_data
       | _ -> assert false
     in
     if operator <> 0x7 then cpu.%{A} <- result;
     change_flag cpu ZeroFlag (result = 0);
     change_flag cpu SubtractionFlag (match operator with 0x2 | 0x3 | 0x7 -> true | _ -> false);
     change_flag cpu HalfCarryFlag (match operator with 0x4 -> true | 0x5 | 0x6 -> false | _ -> result >= 16);
     change_flag cpu CarryFlag (result >= 256 || result < 0)
  | '\xc7' | '\xcf' | '\xd7' | '\xdf' | '\xe7' | '\xef' | '\xf7' | '\xff' ->
     push_pc cpu memory;
     cpu.program_ctr <- opcode land 0x38;
     cpu.m_cycles <- cpu.m_cycles + 1
