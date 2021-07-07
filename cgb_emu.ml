let key_callback (cpu : Cpu.t) (memory : Memory.t) window key _(*scancode*) action _(*modifiers*) =
  let b = match key with
    | GLFW.Right -> 0x01
    | Left -> 0x02
    | Up -> 0x04
    | Down -> 0x08
    | PageUp -> 0x10
    | PageDown -> 0x20
    | Space -> 0x40
    | Enter -> 0x80
    | Escape when action = GLFW.Press -> GLFW.setWindowShouldClose window true; -1
    | F1 when action = GLFW.Press -> Utils.trace := not !Utils.trace; -1
    | _ -> -1
  in
  match action with
  | _ when b < 0 -> ()
  | GLFW.Press ->
     cpu.inputs <- cpu.inputs lor b;
     if memory.io_registers.{0x00} land 0x10 = 0 && b land 0x0f <> 0
        || memory.io_registers.{0x00} land 0x20 = 0 && b land 0xf0 <> 0
     then memory.io_registers.{0x0f} <- memory.io_registers.{0x0f} lor 0x10
  | Release -> cpu.inputs <- cpu.inputs land lnot b
  | Repeat -> ()

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
        if lcd_mode = 3
        then Lcd.render_line lcd memory lcd_y
        else (
          if stat land 0x08 lsl lcd_mode <> 0 then
            memory.io_registers.{0x0f} <- memory.io_registers.{0x0f} lor 0x02;
          if lcd_mode = 1 then
            memory.io_registers.{0x0f} <- memory.io_registers.{0x0f} lor 0x01
          else if lcd_mode = 0 && memory.io_registers.{0x55} land 0x80 = 0 then (
            let src = memory.io_registers.{0x51} lsl 8 lor memory.io_registers.{0x52} in
            let dst = memory.io_registers.{0x53} lsl 8 lor memory.io_registers.{0x54} in
            Cpu.perform_dma_step memory src dst 16;
            cpu.m_cycles <- cpu.m_cycles + 8;
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
    (* Update internal timers *)
    while cpu.divider_register_last_tick + 64 <= cpu.m_cycles do
      cpu.divider_register_last_tick <- cpu.divider_register_last_tick + 64;
      memory.io_registers.{0x04} <- memory.io_registers.{0x04} + 1
    done;
    if memory.io_registers.{0x07} land 0x04 <> 0 then (
      let tick_interval = match memory.io_registers.{0x07} land 0x03 with
        | 0x0 -> 256
        | 0x1 -> 4
        | 0x2 -> 16
        | 0x3 -> 64
        | _ -> assert false
      in
      while cpu.timer_counter_last_tick + tick_interval <= cpu.m_cycles do
        cpu.timer_counter_last_tick <- cpu.timer_counter_last_tick + tick_interval;
        memory.io_registers.{0x05} <- memory.io_registers.{0x05} + 1;
        if memory.io_registers.{0x05} = 0x00 then (
          memory.io_registers.{0x05} <- memory.io_registers.{0x06};
          memory.io_registers.{0x0f} <- memory.io_registers.{0x0f} lor 0x04
        )
      done
    );
    (* Check for interrupts *)
    let requested_and_enabled = memory.ram_high.{0x7f} land memory.io_registers.{0x0f} in
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
      cpu.m_cycles <- cpu.m_cycles + 1
    ) else if cpu.halted then (
      cpu.m_cycles <- cpu.m_cycles + 1
    ) else (
      (* Execute next instruction *)
      let open Cpu in
      if !Utils.trace then (
        Printf.eprintf "[%d M-cycles; %f s]\n" cpu.m_cycles (float_of_int cpu.m_cycles /. float_of_int (1 lsl 20));
        Printf.eprintf " B = 0x%02x    C = 0x%02x    D = 0x%02x    E = 0x%02x\n" cpu.%{B} cpu.%{C} cpu.%{D} cpu.%{E};
        Printf.eprintf " H = 0x%02x    L = 0x%02x    A = 0x%02x    F = %s\n" cpu.%{H} cpu.%{L} cpu.%{A} (string_of_flags cpu);
        Printf.eprintf "SP = 0x%04x  PC = 0x%04x\n%!" cpu.stack_ptr cpu.program_ctr
      );
      if cpu.interrupt_master_enable_pending then (
        cpu.interrupt_master_enable <- true;
        cpu.interrupt_master_enable_pending <- false;
      );
      let opcode = read_8 cpu memory cpu.program_ctr in
      cpu.program_ctr <- cpu.program_ctr + 1;
      execute cpu memory opcode
    )
  done

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s rom_file\n%!" Sys.argv.(0);
    exit 1
  );
  GLFW.init ();
  GLFW.windowHint GLFW.ClientApi GLFW.OpenGLESApi;
  GLFW.windowHint GLFW.ContextVersionMajor 2;
  GLFW.windowHint GLFW.ContextVersionMinor 0;
  GLFW.windowHint GLFW.Resizable false;
  (* let window = GLFW.createWindow 960 864 "cgb_emu" () in *)
  let window = GLFW.createWindow 480 432 "cgb_emu" () in
  GLFW.makeContextCurrent (Some window);
  let cpu = Cpu.create () in
  let memory = Unix.handle_unix_error Memory.init_from_rom Sys.argv.(1) in
  let lcd = Lcd.create () in
  GLFW.setKeyCallback window (Some (key_callback cpu memory)) |> ignore;
  let start_real_time = GLFW.getTime () in
  let start_cpu_time = Sys.time () in
  while not (GLFW.windowShouldClose window) do
    run_until_vblank cpu memory lcd;
    Lcd.render_frame lcd;
    GLFW.swapBuffers window;
    GLFW.pollEvents ()
  done;
  let finish_real_time = GLFW.getTime () in
  let finish_cpu_time = Sys.time () in
  Printf.eprintf "Executed %d M-cycles in %.3f seconds (should be no more than %.3f; CPU time: %.3f seconds).\n%!"
    cpu.m_cycles (finish_real_time -. start_real_time)
    (float_of_int cpu.m_cycles /. float_of_int (17556 * 60))
    (finish_cpu_time -. start_cpu_time);
  GLFW.destroyWindow window;
  GLFW.terminate ()
