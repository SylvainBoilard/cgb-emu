open Utils
open Bigarray
open Memory

type t = {
    registers: (int, int8_unsigned_elt, c_layout) Array1.t; (* BC DE HL (SP) A(znhc----) *)
    mutable program_ctr: int;
    mutable m_cycles: int; (* 2^20 per second *)
    mutable int_master_enable: bool;
    mutable halted: bool;
    mutable divider_register_last_tick: int;
    mutable timer_counter_last_tick: int;

    mutable inputs: int;
    mutable ext_ram_or_timer_enable: bool;
    mutable rtc_selected: int;
    mutable rtc_latched: int;
    mutable rtc_origin: int;
  }

let create () =
  let registers = Array1.create Int8_unsigned C_layout 10 in
  Array1.fill registers 0;
  registers.{6} <- 0xff; registers.{7} <- 0xfe; (* SP = 0xfffe *)
  registers.{8} <- 0x11; (* A = 0x11 (CGB hardware) *)
  { registers; program_ctr = 0x100; m_cycles = 0; int_master_enable = true; halted = false;
    divider_register_last_tick = 0; timer_counter_last_tick = 0;
    inputs = 0; ext_ram_or_timer_enable = false;
    rtc_selected = -1; rtc_latched = -1; rtc_origin = int_of_float (Unix.time ()) }

let get_rr cpu r =
  cpu.registers.{r land lnot 0x01} lsr 8 lor cpu.registers.{r lor 0x01}

let set_rr cpu r v =
  cpu.registers.{r land lnot 0x01} <- v lsl 8;
  cpu.registers.{r lor 0x01} <- v
    
type flag =
  | ZeroFlag
  | SubtractionFlag
  | HalfCarryFlag
  | CarryFlag

let set_flag ctx = function
  | ZeroFlag -> ctx.registers.{9} <- ctx.registers.{9} lor 0x80
  | SubtractionFlag -> ctx.registers.{9} <- ctx.registers.{9} lor 0x40
  | HalfCarryFlag -> ctx.registers.{9} <- ctx.registers.{9} lor 0x20
  | CarryFlag -> ctx.registers.{9} <- ctx.registers.{9} lor 0x10

let reset_flag ctx = function
  | ZeroFlag -> ctx.registers.{9} <- ctx.registers.{9} land lnot 0x80
  | SubtractionFlag -> ctx.registers.{9} <- ctx.registers.{9} land lnot 0x40
  | HalfCarryFlag -> ctx.registers.{9} <- ctx.registers.{9} land lnot 0x20
  | CarryFlag -> ctx.registers.{9} <- ctx.registers.{9} land lnot 0x10

let change_flag ctx flag = function
  | true -> set_flag ctx flag
  | false -> reset_flag ctx flag

let get_flag ctx = function
  | ZeroFlag -> ctx.registers.{9} land 0x80 <> 0
  | SubtractionFlag -> ctx.registers.{9} land 0x40 <> 0
  | HalfCarryFlag -> ctx.registers.{9} land 0x20 <> 0
  | CarryFlag -> ctx.registers.{9} land 0x10 <> 0

let read_8 ctx memory addr =
  ctx.m_cycles <- ctx.m_cycles + 1;
  let value =
    match addr with
    | _ when addr < 0x0 || addr >= 0x10000 -> invalid_arg "read_8: address out of range"
    | _ when addr < 0x4000 -> memory.rom_0.{addr}
    | _ when addr < 0x8000 -> memory.rom_n.{addr - 0x4000}
    | _ when addr < 0xa000 -> memory.ram_video_n.{addr - 0x8000} (* TODO: inacessible during mode 3 *)
    | _ when addr < 0xc000 ->
       if not ctx.ext_ram_or_timer_enable
       then 0
       else if ctx.rtc_selected < 0
       then memory.ram_ext_n.{addr - 0xa000}
       else (
         let now = if ctx.rtc_latched = -1 then int_of_float (Unix.time ()) else ctx.rtc_latched in
         let delta = now - ctx.rtc_origin in
         match ctx.rtc_selected with
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
       land (if memory.io_registers.{0x00} land 0x10 = 0 then lnot (ctx.inputs land 0x0f) else 0xff)
       land (if memory.io_registers.{0x00} land 0x20 = 0 then lnot (ctx.inputs lsr 4 land 0x0f) else 0xff)
    | 0xff69 -> memory.bg_palette_data.{memory.io_registers.{0x68} land 0x3f lxor 0x01} (* TODO: inacessible during mode 3 *)
    | 0xff6b -> memory.obj_palette_data.{memory.io_registers.{0x6a} land 0x3f lxor 0x01} (* TODO: inacessible during mode 3 *)
    | 0xff04 | 0xff05 | 0xff06 | 0xff07 | 0xff0f
      | 0xff40 | 0xff41 | 0xff42 | 0xff43 | 0xff44 | 0xff45 | 0xff46 | 0xff4a | 0xff4b | 0xff4d | 0xff4f
      | 0xff68 | 0xff6a | 0xff70 ->
       memory.io_registers.{addr - 0xff00}
    | 0xff47 | 0xff48 | 0xff49 -> memory.io_registers.{addr - 0xff00} (* SILENCE: Grayscale palettes *)
    | _ when addr >= 0xff10 && addr < 0xff40 -> memory.io_registers.{addr - 0xff00} (* SILENCE: sound *)
    | _ when addr >= 0xff80 -> memory.ram_high.{addr - 0xff80}
    | _ -> Printf.eprintf "read_8: 0x%04x is outside implemented range.\n%!" addr; 0
  in
  (* Printf.eprintf "Read value 0x%02x at 0x%04x\n%!" value addr; *)
  value

let read_16_le ctx memory addr = read_8 ctx memory (addr + 1) lsl 8 lor read_8 ctx memory addr

let write_8 ctx memory addr value =
  (* Printf.eprintf "Write value 0x%02x at 0x%04x\n%!" (value land 0xff) addr; *)
  ctx.m_cycles <- ctx.m_cycles + 1;
  match addr with
  | _ when addr < 0x0 || addr >= 0x10000 -> invalid_arg "write_8: address out of range"
  | _ when addr < 0x2000 -> ctx.ext_ram_or_timer_enable <- value land 0x0f = 0x0a
  | _ when addr < 0x4000 -> memory.rom_n <- memory.rom_banks.(max 1 value land 0x7f)
  | _ when addr < 0x6000 ->
     if value land 0x08 = 0 then (
       ctx.rtc_selected <- -1;
       memory.ram_ext_n <- memory.ram_ext_banks.(value land 0x03)
     ) else ctx.rtc_selected <- value land 0x07
  | _ when addr < 0x8000 ->
     if value land 0x01 = 0
     then ctx.rtc_latched <- -1
     else if ctx.rtc_latched = -1
     then ctx.rtc_latched <- int_of_float (Unix.time ())
  | _ when addr < 0xa000 -> memory.ram_video_n.{addr - 0x8000} <- value (* TODO: inacessible during mode 3 *)
  | _ when addr < 0xc000 ->
     if not ctx.ext_ram_or_timer_enable
     then ()
     else if ctx.rtc_selected < 0
     then memory.ram_ext_n.{addr - 0xa000} <- value
     else (
       let now = int_of_float (Unix.time ()) in
       let delta = now - ctx.rtc_origin in
       match ctx.rtc_selected with
       | 0 -> ctx.rtc_origin <- ctx.rtc_origin + delta mod 60 - value mod 60
       | 1 -> ctx.rtc_origin <- ctx.rtc_origin + (delta / 60 mod 60 - value mod 60) * 60
       | 2 -> ctx.rtc_origin <- ctx.rtc_origin + (delta / 60 / 60 mod 24 - value mod 24) * 60 * 60
       | 3 -> ctx.rtc_origin <- ctx.rtc_origin + (delta / 60 / 60 / 24 mod 256 - value mod 256) * 60 * 60 * 24
       | 4 -> ctx.rtc_origin <- ctx.rtc_origin + (delta / 60 / 60 / 24 / 256 mod 2 - value mod 2) * 60 * 60 * 24 * 256;
       (* FIXME: implement halt and carry bits *)
       | n -> Printf.eprintf "write_8: RTC register %d does not exist.\n%!" n
     )
  | _ when addr < 0xd000 -> memory.ram_work_0.{addr - 0xc000} <- value
  | _ when addr < 0xe000 -> memory.ram_work_n.{addr - 0xd000} <- value
  | _ when addr < 0xf000 -> memory.ram_work_0.{addr - 0xe000} <- value (* prohibited; mirror of 0xc000-0xcfff *)
  | _ when addr < 0xfe00 -> memory.ram_work_n.{addr - 0xf000} <- value (* prohibited; mirror of 0xd000-0xddff *)
  | _ when addr < 0xfea0 -> memory.oam.{addr - 0xfe00} <- value (* TODO: inacessible during mode 2 and 3 *)
  | _ when addr < 0xff00 -> () (* prohibited *)
  | 0xff00 (* Inputs *) -> memory.io_registers.{0x00} <- value lor lnot 0x30;
  | 0xff04 (* Divider Register *) ->
     ctx.divider_register_last_tick <- ctx.m_cycles;
     ctx.timer_counter_last_tick <- ctx.m_cycles;
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
     | '\x00'..'\x3f' -> Array1.(blit (sub memory.rom_0 (value lsl 8 land 0x3fff) 160) memory.oam)
     | '\x40'..'\x7f' -> Array1.(blit (sub memory.rom_n (value lsl 8 land 0x3fff) 160) memory.oam)
     | '\x80'..'\x9f' -> Array1.(blit (sub memory.ram_video_n (value lsl 8 land 0x1fff) 160) memory.oam)
     | '\xa0'..'\xbf' -> Array1.(blit (sub memory.ram_ext_n (value lsl 8 land 0x1fff) 160) memory.oam)
     | '\xc0'..'\xcf' -> Array1.(blit (sub memory.ram_work_0 (value lsl 8 land 0x0fff) 160) memory.oam)
     | '\xd0'..'\xdf' -> Array1.(blit (sub memory.ram_work_n (value lsl 8 land 0x0fff) 160) memory.oam)
     | _ -> Printf.eprintf "write_8: DMA transfer from address 0x%02x00 (> 0xe000) unimplemented.\n%!" (value lsl 8)
     end
  | 0xff47 | 0xff48 | 0xff49 -> memory.io_registers.{addr - 0xff00} <- value (* SILENCE: Grayscale palettes *)
  | 0xff4a (* Window Y Position *) -> memory.io_registers.{0x4a} <- value
  | 0xff4b (* Window X Position + 7 *) -> memory.io_registers.{0x4b} <- value
  | 0xff4d (* Prepare Speed Switch *) ->
     memory.io_registers.{0x4d} <- (memory.io_registers.{0x4d} land 0xfe) lor (value land 0x01)
  | 0xff4f (* VRAM Bank *) ->
     memory.ram_video_n <- memory.ram_video_banks.(value land 0x01);
     memory.io_registers.{0x4f} <- value lor 0xfe
  | 0xff68 (* Color BG Palette Index *) -> memory.io_registers.{0x68} <- value
  | 0xff69 (* Color BG Palette Data *) -> (* TODO: inacessible during mode 3 *)
     let bgpi = memory.io_registers.{0x68} in
     memory.bg_palette_data.{bgpi land 0x3f lxor 0x01} <- value;
     if bgpi land 0x80 <> 0 then memory.io_registers.{0x68} <- (bgpi + 1) land lnot 0x40
  | 0xff6a (* Color OBJ Palette Index *) -> memory.io_registers.{0x6a} <- value
  | 0xff6b (* Color OBJ Palette Data *) -> (* TODO: inacessible during mode 3 *)
     let obpi = memory.io_registers.{0x6a} in
     memory.obj_palette_data.{obpi land 0x3f lxor 0x01} <- value;
     if obpi land 0x80 <> 0 then memory.io_registers.{0x6a} <- (obpi + 1) land lnot 0x40
  | 0xff70 (* WRAM Bank *) ->
     let n = max 1 value land 0x07 in
     memory.io_registers.{0x70} <- n;
     memory.ram_work_n <- memory.ram_work_banks.(n)
  | _ when addr >= 0xff80 -> memory.ram_high.{addr - 0xff80} <- value
  | _ -> Printf.eprintf "write_8: 0x%04x is outside implemented range (value = 0x%02x).\n%!" addr value

let write_16_le ctx memory addr value = write_8 ctx memory addr value; write_8 ctx memory (addr + 1) (value lsr 8)

let execute_cb_prefixed cpu memory =
  let opcode = read_8 cpu memory cpu.program_ctr in
  cpu.program_ctr <- cpu.program_ctr + 1;
  let operand = match opcode land 0x07 with
    | 0x6 -> read_8 cpu memory (get_rr cpu 4)
    | 0x7 -> cpu.registers.{8}
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
    | '\x40'..'\x7f' -> operand land 0x01 lsl (opcode lsr 5)
    | '\x80'..'\xbf' -> operand land lnot (0x01 lsl (opcode lsr 5))
    | '\xc0'..'\xff' -> operand lor 0x01 lsl (opcode lsr 5)
  in
  if opcode < 0x80 then (
    change_flag cpu ZeroFlag (result = 0);
    reset_flag cpu SubtractionFlag;
    change_flag cpu HalfCarryFlag (opcode >= 0x40)
  );
  match opcode land 0x07 with
  | _ when opcode land 0xc0 = 0x40 -> ()
  | 0x6 -> write_8 cpu memory (get_rr cpu 4) result
  | 0x7 -> cpu.registers.{8} <- result
  | n -> cpu.registers.{n} <- result

let jr cpu memory cond =
  let offset = signed_int8 (read_8 cpu memory cpu.program_ctr) in
  if cond then (
    cpu.program_ctr <- cpu.program_ctr + offset + 1;
    cpu.m_cycles <- cpu.m_cycles + 1
  ) else cpu.program_ctr <- cpu.program_ctr + 1

let ld_rr_u16 cpu memory r =
  set_rr cpu r (read_16_le cpu memory cpu.program_ctr);
  cpu.program_ctr <- cpu.program_ctr + 2

let add_rr_rr cpu rout rin =
  let result = get_rr cpu rout + get_rr cpu rin in
  set_rr cpu rout result;
  reset_flag cpu SubtractionFlag;
  change_flag cpu HalfCarryFlag (result >= 0x1000);
  change_flag cpu CarryFlag (result >= 0x10000);
  cpu.m_cycles <- cpu.m_cycles + 1

let incr_rr cpu r =
  set_rr cpu r (get_rr cpu r + 1);
  cpu.m_cycles <- cpu.m_cycles + 1

let decr_rr cpu r =
  set_rr cpu r (get_rr cpu r - 1);
  cpu.m_cycles <- cpu.m_cycles + 1

let incr_r cpu r =
  let result = cpu.registers.{r} + 1 in
  cpu.registers.{r} <- result;
  change_flag cpu ZeroFlag (result = 0);
  reset_flag cpu SubtractionFlag;
  change_flag cpu HalfCarryFlag (result >= 0x10)

let decr_r cpu r =
  let result = cpu.registers.{r} - 1 in
  cpu.registers.{r} <- result;
  change_flag cpu ZeroFlag (result = 0);
  set_flag cpu SubtractionFlag;
  change_flag cpu HalfCarryFlag (result >= 0x10)

let ld_r_u8 cpu memory r =
  cpu.registers.{r} <- read_8 cpu memory cpu.program_ctr;
  cpu.program_ctr <- cpu.program_ctr + 1

let push_rr cpu memory r =
  let addr = get_rr cpu 6 - 2 in
  write_16_le cpu memory addr (get_rr cpu r);
  set_rr cpu 6 addr

let pop_rr cpu memory r =
  let addr = get_rr cpu 6 in
  set_rr cpu r (read_16_le cpu memory addr);
  set_rr cpu 6 (addr + 2)

let push_pc cpu memory =
  let addr = get_rr cpu 6 - 2 in
  write_16_le cpu memory addr cpu.program_ctr;
  set_rr cpu 6 addr

let pop_pc cpu memory =
  let addr = get_rr cpu 6 in
  cpu.program_ctr <- read_16_le cpu memory addr;
  set_rr cpu 6 (addr + 2)

let execute cpu memory opcode =
  (* Printf.eprintf "Executing opcode 0x%02x at 0x%04x\n%!" opcode (cpu.program_ctr - 1); *)
  match Char.chr opcode with
  | '\xd3' | '\xdb' | '\xdd' | '\xe3'..'\xe4' | '\xeb'..'\xed' | '\xf4' | '\xfc'..'\xfd' ->
     Printf.eprintf "execute: illegal opcode 0x%02x at 0x%04x.\n%!" opcode (cpu.program_ctr - 1)
  | '\x00' -> ()
  | '\x08' ->
     write_16_le cpu memory (read_16_le cpu memory cpu.program_ctr) (get_rr cpu 6);
     cpu.program_ctr <- cpu.program_ctr + 2
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

  | '\x01' -> ld_rr_u16 cpu memory 0
  | '\x09' -> add_rr_rr cpu 4 0
  | '\x11' -> ld_rr_u16 cpu memory 2
  | '\x19' -> add_rr_rr cpu 4 2
  | '\x21' -> ld_rr_u16 cpu memory 4
  | '\x29' -> add_rr_rr cpu 4 4
  | '\x31' -> ld_rr_u16 cpu memory 6
  | '\x39' -> add_rr_rr cpu 4 6

  | '\x02' -> write_8 cpu memory (get_rr cpu 0) cpu.registers.{8}
  | '\x0a' -> cpu.registers.{8} <- read_8 cpu memory (get_rr cpu 0)
  | '\x12' -> write_8 cpu memory (get_rr cpu 2) cpu.registers.{8}
  | '\x1a' -> cpu.registers.{8} <- read_8 cpu memory (get_rr cpu 2)
  | '\x22' ->
     let addr = get_rr cpu 4 in
     write_8 cpu memory addr cpu.registers.{8};
     set_rr cpu 4 (addr + 1)
  | '\x2a' ->
     let addr = get_rr cpu 4 in
     cpu.registers.{8} <- read_8 cpu memory addr;
     set_rr cpu 4 (addr + 1)
  | '\x32' ->
     let addr = get_rr cpu 4 in
     write_8 cpu memory addr cpu.registers.{8};
     set_rr cpu 4 (addr - 1)
  | '\x3a' ->
     let addr = get_rr cpu 4 in
     cpu.registers.{8} <- read_8 cpu memory addr;
     set_rr cpu 4 (addr - 1)

  | '\x03' -> incr_rr cpu 0
  | '\x0b' -> decr_rr cpu 0
  | '\x13' -> incr_rr cpu 2
  | '\x1b' -> decr_rr cpu 2
  | '\x23' -> incr_rr cpu 4
  | '\x2b' -> decr_rr cpu 4
  | '\x33' -> incr_rr cpu 6
  | '\x3b' -> decr_rr cpu 6

  | '\x04' -> incr_r cpu 0
  | '\x0c' -> incr_r cpu 1
  | '\x14' -> incr_r cpu 2
  | '\x1c' -> incr_r cpu 3
  | '\x24' -> incr_r cpu 4
  | '\x2c' -> incr_r cpu 5
  | '\x34' ->
     let addr = get_rr cpu 4 in
     let result = read_8 cpu memory addr + 1 in
     write_8 cpu memory addr result;
     change_flag cpu ZeroFlag (result = 0);
     reset_flag cpu SubtractionFlag;
     change_flag cpu HalfCarryFlag (result >= 0x10)
  | '\x3c' -> incr_r cpu 8

  | '\x05' -> decr_r cpu 0
  | '\x0d' -> decr_r cpu 1
  | '\x15' -> decr_r cpu 2
  | '\x1d' -> decr_r cpu 3
  | '\x25' -> decr_r cpu 4
  | '\x2d' -> decr_r cpu 5
  | '\x35' ->
     let addr = get_rr cpu 4 in
     let result = read_8 cpu memory addr - 1 in
     write_8 cpu memory addr result;
     change_flag cpu ZeroFlag (result = 0);
     reset_flag cpu SubtractionFlag;
     change_flag cpu HalfCarryFlag (result >= 0x10)
  | '\x3d' -> decr_r cpu 8

  | '\x06' -> ld_r_u8 cpu memory 0
  | '\x0e' -> ld_r_u8 cpu memory 1
  | '\x16' -> ld_r_u8 cpu memory 2
  | '\x1e' -> ld_r_u8 cpu memory 3
  | '\x26' -> ld_r_u8 cpu memory 4
  | '\x2e' -> ld_r_u8 cpu memory 5
  | '\x36' ->
     write_8 cpu memory (get_rr cpu 4) (read_8 cpu memory cpu.program_ctr);
     cpu.program_ctr <- cpu.program_ctr + 1
  | '\x3e' -> ld_r_u8 cpu memory 8

  | '\x07' ->
     let c = if get_flag cpu CarryFlag then 0x01 else 0x00 in
     change_flag cpu CarryFlag (cpu.registers.{8} land 0x80 <> 0);
     cpu.registers.{8} <- cpu.registers.{8} lsl 1 lor c
  | '\x0f' ->
     let c = if get_flag cpu CarryFlag then 0x80 else 0x00 in
     change_flag cpu CarryFlag (cpu.registers.{8} land 0x01 <> 0);
     cpu.registers.{8} <- c lor cpu.registers.{8} lsr 1
  | '\x17' ->
     change_flag cpu CarryFlag (cpu.registers.{8} land 0x80 <> 0);
     cpu.registers.{8} <- cpu.registers.{8} lsl 1 lor cpu.registers.{8} lsr 7
  | '\x1f' ->
     change_flag cpu CarryFlag (cpu.registers.{8} land 0x01 <> 0);
     cpu.registers.{8} <- cpu.registers.{8} lsl 7 lor cpu.registers.{8} lsr 1
  | '\x27' -> Printf.eprintf "execute: unimplemented opcode 0x%02x at 0x%04x\n%!" opcode (cpu.program_ctr - 1) (* TODO: DAA *)
  | '\x2f' ->
     set_flag cpu SubtractionFlag;
     set_flag cpu HalfCarryFlag;
     cpu.registers.{8} <- lnot cpu.registers.{8}
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
       | 0x6 -> read_8 cpu memory (get_rr cpu 4)
       | 0x7 -> cpu.registers.{8}
       | n -> cpu.registers.{n}
     in
     begin match opcode lsr 3 land 0x7 with
     | 0x6 -> write_8 cpu memory (get_rr cpu 4) src_data
     | 0x7 -> cpu.registers.{8} <- src_data
     | n -> cpu.registers.{n} <- src_data
     end

  | '\x80'..'\xbf' (* 8-bit alu *) ->
     let operator, operand  = opcode lsr 3 land 0x7, opcode land 0x7 in
     let operand_data = match operand with
       | 0x6 -> read_8 cpu memory (get_rr cpu 4)
       | 0x7 -> cpu.registers.{8}
       | n -> cpu.registers.{n}
     in
     let result = match operator with
       | 0x0 -> cpu.registers.{8} + operand_data
       | 0x1 -> cpu.registers.{8} + operand_data + Bool.to_int (get_flag cpu CarryFlag)
       | 0x2 -> cpu.registers.{8} - operand_data
       | 0x3 -> cpu.registers.{8} - operand_data - Bool.to_int (get_flag cpu CarryFlag)
       | 0x4 -> cpu.registers.{8} land operand_data
       | 0x5 -> cpu.registers.{8} lxor operand_data
       | 0x6 -> cpu.registers.{8} lor operand_data
       | 0x7 -> cpu.registers.{8} - operand_data
       | _ -> assert false
     in
     if operator <> 0x7 then cpu.registers.{8} <- result;
     change_flag cpu ZeroFlag (result = 0);
     change_flag cpu SubtractionFlag (match operator with 0x2 | 0x3 | 0x7 -> true | _ -> false);
     change_flag cpu HalfCarryFlag (match operator with 0x4 -> true | 0x5 | 0x6 -> false | _ -> result >= 0x10);
     change_flag cpu CarryFlag (result >= 0x100 || result < 0)

  | '\xc0'..'\xff' (* various *) ->
     begin match opcode land 0x07 with
     | 0x0 ->
        if opcode lsr 5 land 0x1 = 0 then (
          let t = match opcode lsr 3 land 0x3 with
            | 0x0 -> not (get_flag cpu ZeroFlag)
            | 0x1 -> get_flag cpu ZeroFlag
            | 0x2 -> not (get_flag cpu CarryFlag)
            | 0x3 -> get_flag cpu CarryFlag
            | _ -> assert false
          in
          if t then (
            pop_pc cpu memory;
            cpu.m_cycles <- cpu.m_cycles + 2
          ) else cpu.m_cycles <- cpu.m_cycles + 1
        ) else
          let i = read_8 cpu memory cpu.program_ctr in
          cpu.program_ctr <- cpu.program_ctr + 1;
          begin match opcode lsr 3 land 0x3 with
          | 0x0 -> write_8 cpu memory (0xff00 + i) cpu.registers.{8}
          | 0x1 -> (* FIXME: increment machine cycles and modify flags. *)
             Printf.eprintf "execute: partially implemented opcode 0x%02x at 0x%04x.\n%!" opcode (cpu.program_ctr - 1);
             set_rr cpu 6 (get_rr cpu 6 + signed_int8 i)
          | 0x2 -> cpu.registers.{8} <- read_8 cpu memory (0xff00 + i)
          | 0x3 -> (* FIXME: same as above *)
             Printf.eprintf "execute: partially implemented opcode 0x%02x at 0x%04x.\n%!" opcode (cpu.program_ctr - 1);
             set_rr cpu 4 (get_rr cpu 6 + signed_int8 i)
          | _ -> assert false
          end
     | 0x1 ->
        begin match opcode with
        | 0xc9 ->
           pop_pc cpu memory;
           cpu.m_cycles <- cpu.m_cycles + 1
        | 0xd9 ->
           pop_pc cpu memory;
           cpu.int_master_enable <- true;
           cpu.m_cycles <- cpu.m_cycles + 1
        | 0xe9 -> cpu.program_ctr <- get_rr cpu 4
        | 0xf9 ->
           set_rr cpu 6 (get_rr cpu 4);
           cpu.m_cycles <- cpu.m_cycles + 1
        | 0xf1 -> pop_rr cpu memory 8
        | _ -> pop_rr cpu memory (opcode lsr 3 land 0x06)
        end
     | 0x2 ->
        if opcode lsr 5 land 0x1 = 0 then (
          let t = match opcode lsr 3 land 0x3 with
            | 0x0 -> not (get_flag cpu ZeroFlag)
            | 0x1 -> get_flag cpu ZeroFlag
            | 0x2 -> not (get_flag cpu CarryFlag)
            | 0x3 -> get_flag cpu CarryFlag
            | _ -> assert false
          in
          let addr = read_16_le cpu memory cpu.program_ctr in
          cpu.program_ctr <- cpu.program_ctr + 2;
          if t then (cpu.program_ctr <- addr; cpu.m_cycles <- cpu.m_cycles + 1)
        ) else
          begin match opcode lsr 3 land 0x3 with
          | 0x0 -> write_8 cpu memory (0xff00 + cpu.registers.{1}) cpu.registers.{8}
          | 0x1 ->
             write_8 cpu memory (read_16_le cpu memory cpu.program_ctr) cpu.registers.{8};
             cpu.program_ctr <- cpu.program_ctr + 2
          | 0x2 -> cpu.registers.{8} <- read_8 cpu memory (0xff00 + cpu.registers.{1})
          | 0x3 ->
             cpu.registers.{8} <- read_8 cpu memory (read_16_le cpu memory cpu.program_ctr);
             cpu.program_ctr <- cpu.program_ctr + 2
          | _ -> assert false
          end
     | 0x3 ->
        begin match opcode with
        | 0xc3 -> cpu.program_ctr <- read_16_le cpu memory cpu.program_ctr
        | 0xcb -> execute_cb_prefixed cpu memory
        | 0xf3 -> cpu.int_master_enable <- false
        | 0xfb -> cpu.int_master_enable <- true
        | _ -> assert false
        end
     | 0x4 ->
        let t = match opcode lsr 3 land 0x7 with
          | 0x0 -> not (get_flag cpu ZeroFlag)
          | 0x1 -> get_flag cpu ZeroFlag
          | 0x2 -> not (get_flag cpu CarryFlag)
          | 0x3 -> get_flag cpu CarryFlag
          | _ -> assert false
        in
        let addr = read_16_le cpu memory cpu.program_ctr in
        cpu.program_ctr <- cpu.program_ctr + 2;
        if t then (
          push_pc cpu memory;
          cpu.program_ctr <- addr;
          cpu.m_cycles <- cpu.m_cycles + 1
        )
     | 0x5 ->
        begin match opcode with
        | 0xcd ->
           let addr = read_16_le cpu memory cpu.program_ctr in
           cpu.program_ctr <- cpu.program_ctr + 2;
           push_pc cpu memory;
           cpu.program_ctr <- addr
        | 0xf5 -> push_rr cpu memory 8
        | _ when opcode land 0x08 = 0 -> push_rr cpu memory (opcode lsr 3 land 0x06)
        | _ -> assert false
        end;
        cpu.m_cycles <- cpu.m_cycles + 1
     | 0x6 ->
        let operator = opcode lsr 3 land 0x7 in
        let operand_data = read_8 cpu memory cpu.program_ctr in
        cpu.program_ctr <- cpu.program_ctr + 1;
        let result = match operator with
          | 0x0 -> cpu.registers.{8} + operand_data
          | 0x1 -> cpu.registers.{8} + operand_data + Bool.to_int (get_flag cpu CarryFlag)
          | 0x2 -> cpu.registers.{8} - operand_data
          | 0x3 -> cpu.registers.{8} - operand_data - Bool.to_int (get_flag cpu CarryFlag)
          | 0x4 -> cpu.registers.{8} land operand_data
          | 0x5 -> cpu.registers.{8} lxor operand_data
          | 0x6 -> cpu.registers.{8} lor operand_data
          | 0x7 -> cpu.registers.{8} - operand_data
          | _ -> assert false
        in
        if operator <> 0x7 then cpu.registers.{8} <- result;
        change_flag cpu ZeroFlag (result = 0);
        change_flag cpu SubtractionFlag (match operator with 0x2 | 0x3 | 0x7 -> true | _ -> false);
        change_flag cpu HalfCarryFlag (match operator with 0x4 -> true | 0x5 | 0x6 -> false | _ -> result >= 16);
        change_flag cpu CarryFlag (result >= 256 || result < 0)
     | 0x7 ->
        let stk = get_rr cpu 6 - 2 in
        set_rr cpu 6 stk;
        write_16_le cpu memory stk cpu.program_ctr;
        cpu.program_ctr <- opcode land 0x38
     | _ -> assert false
     end
