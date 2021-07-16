let record_gb_input (cpu : Cpu.t) (memory : Memory.t) key action =
  let b = match key with
    | GLFW.Right -> 0x01
    | Left -> 0x02
    | Up -> 0x04
    | Down -> 0x08
    | PageUp | Home -> 0x10
    | PageDown | End -> 0x20
    | Space -> 0x40
    | Enter -> 0x80
    | _ -> -1
  in
  match action with
  | _ when b < 0 -> false
  | GLFW.Press ->
     cpu.inputs <- cpu.inputs lor b;
     if memory.io_registers.{0x00} land 0x10 = 0 && b land 0x0f <> 0
        || memory.io_registers.{0x00} land 0x20 = 0 && b land 0xf0 <> 0
     then memory.io_registers.{0x0f} <- memory.io_registers.{0x0f} lor 0x10;
     true
  | Release -> cpu.inputs <- cpu.inputs land lnot b; true
  | Repeat -> false

let key_callback (cpu : Cpu.t) (memory : Memory.t) tiles_window current_window key _(*scancode*) action _(*modifiers*) =
  let open GLFW in
  match key, action with
  | _ when record_gb_input cpu memory key action -> ()
  | Escape, Press -> setWindowShouldClose current_window true
  | F2, Press when getWindowAttrib ~window:tiles_window ~attribute:Visible -> hideWindow tiles_window
  | F2, Press -> showWindow tiles_window
  | _ -> ()

let run_until_vblank (cpu : Cpu.t) (memory : Memory.t) (lcd : Lcd.t) =
  let next_vblank = cpu.m_cycles - (cpu.m_cycles + 1140) mod 17556 + 17556 in
  while cpu.m_cycles < next_vblank do
    (* Update LCD *)
    let lcd_y = cpu.m_cycles / 114 mod 154 in
    let lcd_mode = match cpu.m_cycles mod 114 with
      | _ when lcd_y >= 144 -> 1
      | c when c < 20 -> 2
      | c when c < 63 -> 3
      | _ -> 0
    in
    let stat = memory.io_registers.{0x41} in
    let stat =
      if stat land 0x03 <> lcd_mode then (
        if lcd_mode = 3 then
          Lcd.render_line lcd memory lcd_y
        else (
          if stat land 0x08 lsl lcd_mode <> 0 then
            memory.io_registers.{0x0f} <- memory.io_registers.{0x0f} lor 0x02;
          if lcd_mode = 1 then (
            memory.io_registers.{0x0f} <- memory.io_registers.{0x0f} lor 0x01;
            lcd.window_y_internal <- -1
          ) else if lcd_mode = 0 && memory.io_registers.{0x55} land 0x80 = 0 then (
            let src = memory.io_registers.{0x51} lsl 8 lor memory.io_registers.{0x52} in
            let dst = memory.io_registers.{0x53} lsl 8 lor memory.io_registers.{0x54} in
            Cpu.perform_dma_step memory src dst 16;
            for _ = 1 to 8 do Cpu.incr_m_cycles cpu memory done;
            let next_src = src + 16 in
            let next_dst = dst + 16 in
            memory.io_registers.{0x51} <- next_src lsr 8;
            memory.io_registers.{0x52} <- next_src;
            memory.io_registers.{0x53} <- next_dst lsr 8;
            memory.io_registers.{0x54} <- next_dst;
            memory.io_registers.{0x55} <- memory.io_registers.{0x55} - 1
          )
        );
        stat land lnot 0x03 lor lcd_mode
      ) else stat
    in
    let stat =
      if lcd_y = memory.io_registers.{0x45} then (
        if stat land 0x44 = 0x40 then
          memory.io_registers.{0x0f} <- memory.io_registers.{0x0f} lor 0x02;
        stat lor 0x04
      ) else stat land lnot 0x04
    in
    memory.io_registers.{0x41} <- stat;
    memory.io_registers.{0x44} <- lcd_y;
    (* Check for interrupts *)
    let requested_and_enabled = memory.ram_high.{0x7f} land memory.io_registers.{0x0f} land 0x1f in
    (* Even if IME is unset, when an enabled interrupt is requested, un-halt.*)
    if requested_and_enabled <> 0 then
      cpu.halted <- false;
    if requested_and_enabled <> 0 && cpu.interrupt_master_enable then (
      (* Service interrupt *)
      cpu.interrupt_master_enable <- false;
      cpu.interrupt_master_enable_pending <- false;
      Cpu.push_pc cpu memory;
      let i = ref 0 in
      while requested_and_enabled lsr !i land 0x01 = 0 do incr i done;
      memory.io_registers.{0x0f} <- memory.io_registers.{0x0f} lxor 0x01 lsl !i;
      cpu.program_ctr <- !i lsl 3 lor 0x40;
      Cpu.incr_m_cycles cpu memory;
      Cpu.incr_m_cycles cpu memory
    ) else if cpu.halted then (
      Cpu.incr_m_cycles cpu memory
    ) else ( (* Execute next instruction *)
      Cpu.(execute cpu memory (read_8_immediate cpu memory))
    )
  done

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s rom_file\n%!" Sys.argv.(0);
    exit 1
  );
  let cpu = Cpu.create () in
  let memory = Unix.handle_unix_error Memory.init_from_rom Sys.argv.(1) in
  Memory.print_rom_info memory;
  GLFW.init ();
  GLFW.windowHint GLFW.ClientApi GLFW.OpenGLESApi;
  GLFW.windowHint GLFW.ContextVersionMajor 2;
  GLFW.windowHint GLFW.ContextVersionMinor 0;
  GLFW.windowHint GLFW.Resizable false;
  let lcd_window = GLFW.createWindow 480 432 "cgb_emu" () in
  GLFW.makeContextCurrent (Some lcd_window);
  GLFW.windowHint GLFW.Visible false;
  let tiles_window = GLFW.createWindow ~width:512 ~height:384 ~title:"cgb_emu – tiles" ~share:lcd_window () in
  GLFW.setKeyCallback lcd_window (Some (key_callback cpu memory tiles_window)) |> ignore;
  GLFW.setWindowPos lcd_window 8 84;
  GLFW.setKeyCallback tiles_window (Some (key_callback cpu memory tiles_window)) |> ignore;
  GLFW.setWindowPos tiles_window 504 108;
  let lcd = Lcd.create () in
  let start_real_time = GLFW.getTime () in
  let start_cpu_time = Sys.time () in
  while not (GLFW.windowShouldClose lcd_window || GLFW.windowShouldClose tiles_window) do
    run_until_vblank cpu memory lcd;
    GLFW.pollEvents ();
    Lcd.render_frame lcd;
    GLFW.swapBuffers lcd_window;
    if GLFW.getWindowAttrib ~window:tiles_window ~attribute:GLFW.Visible then (
      GLFW.makeContextCurrent (Some tiles_window);
      Lcd.render_tiles lcd memory;
      GLFW.swapBuffers tiles_window;
      GLFW.makeContextCurrent (Some lcd_window)
    )
  done;
  let finish_real_time = GLFW.getTime () in
  let finish_cpu_time = Sys.time () in
  Printf.eprintf "Executed %d M-cycles in %.3f seconds (should be no more than %.3f; CPU time: %.3f seconds).\n%!"
    cpu.m_cycles (finish_real_time -. start_real_time)
    (float_of_int cpu.m_cycles /. float_of_int (17556 * 60))
    (finish_cpu_time -. start_cpu_time);
  GLFW.destroyWindow lcd_window;
  GLFW.destroyWindow tiles_window;
  GLFW.terminate ()
