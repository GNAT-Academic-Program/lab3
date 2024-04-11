pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__lab3.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__lab3.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;

package body ada_main is

   E065 : Short_Integer; pragma Import (Ada, E065, "ada__tags_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__strings__text_buffers_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "system__bb__timing_events_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "ada__exceptions_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "system__soft_links_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "system__exception_table_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "ada__streams_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "system__finalization_root_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "ada__finalization_E");
   E163 : Short_Integer; pragma Import (Ada, E163, "system__storage_pools_E");
   E161 : Short_Integer; pragma Import (Ada, E161, "system__finalization_masters_E");
   E178 : Short_Integer; pragma Import (Ada, E178, "ada__real_time_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "ada__strings__maps_E");
   E126 : Short_Integer; pragma Import (Ada, E126, "ada__strings__unbounded_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "system__pool_global_E");
   E219 : Short_Integer; pragma Import (Ada, E219, "system__tasking__protected_objects_E");
   E225 : Short_Integer; pragma Import (Ada, E225, "system__tasking__protected_objects__multiprocessors_E");
   E238 : Short_Integer; pragma Import (Ada, E238, "system__tasking__restricted__stages_E");
   E243 : Short_Integer; pragma Import (Ada, E243, "hal__audio_E");
   E159 : Short_Integer; pragma Import (Ada, E159, "hal__bitmap_E");
   E281 : Short_Integer; pragma Import (Ada, E281, "hal__framebuffer_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "hal__gpio_E");
   E215 : Short_Integer; pragma Import (Ada, E215, "hal__i2c_E");
   E247 : Short_Integer; pragma Import (Ada, E247, "hal__real_time_clock_E");
   E181 : Short_Integer; pragma Import (Ada, E181, "hal__spi_E");
   E297 : Short_Integer; pragma Import (Ada, E297, "hal__time_E");
   E304 : Short_Integer; pragma Import (Ada, E304, "hal__touch_panel_E");
   E262 : Short_Integer; pragma Import (Ada, E262, "hal__uart_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "bitmap_color_conversion_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "bmp_fonts_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "hershey_fonts_E");
   E169 : Short_Integer; pragma Import (Ada, E169, "bitmapped_drawing_E");
   E295 : Short_Integer; pragma Import (Ada, E295, "ili9341_E");
   E301 : Short_Integer; pragma Import (Ada, E301, "l3gd20_E");
   E299 : Short_Integer; pragma Import (Ada, E299, "ravenscar_time_E");
   E290 : Short_Integer; pragma Import (Ada, E290, "soft_drawing_bitmap_E");
   E288 : Short_Integer; pragma Import (Ada, E288, "memory_mapped_bitmap_E");
   E187 : Short_Integer; pragma Import (Ada, E187, "stm32__adc_E");
   E193 : Short_Integer; pragma Import (Ada, E193, "stm32__dac_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "stm32__dma__interrupts_E");
   E275 : Short_Integer; pragma Import (Ada, E275, "stm32__dma2d_E");
   E278 : Short_Integer; pragma Import (Ada, E278, "stm32__dma2d__interrupt_E");
   E280 : Short_Integer; pragma Import (Ada, E280, "stm32__dma2d__polling_E");
   E283 : Short_Integer; pragma Import (Ada, E283, "stm32__dma2d_bitmap_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "stm32__exti_E");
   E267 : Short_Integer; pragma Import (Ada, E267, "stm32__fmc_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "stm32__i2s_E");
   E249 : Short_Integer; pragma Import (Ada, E249, "stm32__power_control_E");
   E246 : Short_Integer; pragma Import (Ada, E246, "stm32__rtc_E");
   E265 : Short_Integer; pragma Import (Ada, E265, "stm32__sdram_E");
   E254 : Short_Integer; pragma Import (Ada, E254, "stm32__spi_E");
   E256 : Short_Integer; pragma Import (Ada, E256, "stm32__spi__dma_E");
   E199 : Short_Integer; pragma Import (Ada, E199, "stm32__gpio_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "stm32__i2c_E");
   E217 : Short_Integer; pragma Import (Ada, E217, "stm32__i2c__dma_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "stm32__syscfg_E");
   E260 : Short_Integer; pragma Import (Ada, E260, "stm32__usarts_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "stm32__device_E");
   E292 : Short_Integer; pragma Import (Ada, E292, "stm32__ltdc_E");
   E273 : Short_Integer; pragma Import (Ada, E273, "framebuffer_ltdc_E");
   E306 : Short_Integer; pragma Import (Ada, E306, "stmpe811_E");
   E271 : Short_Integer; pragma Import (Ada, E271, "framebuffer_ili9341_E");
   E303 : Short_Integer; pragma Import (Ada, E303, "touch_panel_stmpe811_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "stm32__board_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Exception_Tracebacks : Integer;
      pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
      Exception_Tracebacks_Symbolic : Integer;
      pragma Import (C, Exception_Tracebacks_Symbolic, "__gl_exception_tracebacks_symbolic");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := 0;
      WC_Encoding := 'b';
      Locking_Policy := 'C';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := 'F';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, True, True, False, False, False, False, True, 
           False, False, False, False, False, False, False, False, 
           True, True, True, False, False, False, False, False, 
           True, False, False, False, False, False, False, False, 
           False, False, True, True, False, False, True, True, 
           False, False, False, True, False, False, False, False, 
           True, False, True, True, False, False, False, False, 
           False, True, True, True, True, True, False, False, 
           True, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, True, False, False, False, False, False, False, 
           False, False, False, False, True, True, False, True, 
           False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (False, False, False, False, True, True, False, False, 
           False, False, False, True, True, True, True, False, 
           False, False, False, True, False, False, True, True, 
           False, True, True, False, True, True, True, True, 
           False, False, False, False, False, True, False, False, 
           True, False, True, False, True, True, False, False, 
           False, True, False, False, False, True, False, False, 
           False, False, False, False, False, False, False, False, 
           False, True, True, True, False, False, True, False, 
           True, True, True, False, True, True, False, False, 
           True, True, True, False, False, False, False, False, 
           False, False, False, True, False, False, False, False, 
           True, False),
         Count => (0, 0, 0, 1, 0, 0, 0, 0, 4, 0),
         Unknown => (False, False, False, False, False, False, False, False, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Exception_Tracebacks := 1;
      Exception_Tracebacks_Symbolic := 1;
      Detect_Blocking := 1;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Ada.Strings.Text_Buffers'Elab_Spec;
      E008 := E008 + 1;
      System.Bb.Timing_Events'Elab_Spec;
      E063 := E063 + 1;
      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      Ada.Tags'Elab_Body;
      E065 := E065 + 1;
      System.Exception_Table'Elab_Body;
      E100 := E100 + 1;
      E102 := E102 + 1;
      E012 := E012 + 1;
      Ada.Streams'Elab_Spec;
      E143 := E143 + 1;
      System.Finalization_Root'Elab_Spec;
      E146 := E146 + 1;
      Ada.Finalization'Elab_Spec;
      E141 := E141 + 1;
      System.Storage_Pools'Elab_Spec;
      E163 := E163 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E161 := E161 + 1;
      Ada.Real_Time'Elab_Body;
      E178 := E178 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E130 := E130 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E126 := E126 + 1;
      System.Pool_Global'Elab_Spec;
      E165 := E165 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E219 := E219 + 1;
      System.Tasking.Protected_Objects.Multiprocessors'Elab_Body;
      E225 := E225 + 1;
      System.Tasking.Restricted.Stages'Elab_Body;
      E238 := E238 + 1;
      HAL.AUDIO'ELAB_SPEC;
      E243 := E243 + 1;
      HAL.BITMAP'ELAB_SPEC;
      E159 := E159 + 1;
      HAL.FRAMEBUFFER'ELAB_SPEC;
      E281 := E281 + 1;
      HAL.GPIO'ELAB_SPEC;
      E210 := E210 + 1;
      HAL.I2C'ELAB_SPEC;
      E215 := E215 + 1;
      HAL.REAL_TIME_CLOCK'ELAB_SPEC;
      E247 := E247 + 1;
      HAL.SPI'ELAB_SPEC;
      E181 := E181 + 1;
      HAL.TIME'ELAB_SPEC;
      E297 := E297 + 1;
      HAL.TOUCH_PANEL'ELAB_SPEC;
      E304 := E304 + 1;
      HAL.UART'ELAB_SPEC;
      E262 := E262 + 1;
      E157 := E157 + 1;
      E171 := E171 + 1;
      E173 := E173 + 1;
      E169 := E169 + 1;
      ILI9341'ELAB_SPEC;
      ILI9341'ELAB_BODY;
      E295 := E295 + 1;
      L3GD20'ELAB_SPEC;
      L3GD20'ELAB_BODY;
      E301 := E301 + 1;
      Ravenscar_Time'Elab_Spec;
      Ravenscar_Time'Elab_Body;
      E299 := E299 + 1;
      Soft_Drawing_Bitmap'Elab_Spec;
      Soft_Drawing_Bitmap'Elab_Body;
      E290 := E290 + 1;
      Memory_Mapped_Bitmap'Elab_Spec;
      Memory_Mapped_Bitmap'Elab_Body;
      E288 := E288 + 1;
      STM32.ADC'ELAB_SPEC;
      E187 := E187 + 1;
      E193 := E193 + 1;
      E231 := E231 + 1;
      E275 := E275 + 1;
      STM32.DMA2D.INTERRUPT'ELAB_BODY;
      E278 := E278 + 1;
      E280 := E280 + 1;
      STM32.DMA2D_BITMAP'ELAB_SPEC;
      STM32.DMA2D_BITMAP'ELAB_BODY;
      E283 := E283 + 1;
      E206 := E206 + 1;
      E267 := E267 + 1;
      STM32.I2S'ELAB_SPEC;
      STM32.I2S'ELAB_BODY;
      E242 := E242 + 1;
      E249 := E249 + 1;
      STM32.RTC'ELAB_SPEC;
      STM32.RTC'ELAB_BODY;
      E246 := E246 + 1;
      E265 := E265 + 1;
      STM32.SPI'ELAB_SPEC;
      STM32.SPI'ELAB_BODY;
      E254 := E254 + 1;
      STM32.SPI.DMA'ELAB_SPEC;
      STM32.SPI.DMA'ELAB_BODY;
      E256 := E256 + 1;
      STM32.GPIO'ELAB_SPEC;
      STM32.I2C'ELAB_SPEC;
      STM32.I2C.DMA'ELAB_SPEC;
      E204 := E204 + 1;
      STM32.GPIO'ELAB_BODY;
      E199 := E199 + 1;
      STM32.USARTS'ELAB_SPEC;
      STM32.DEVICE'ELAB_SPEC;
      E183 := E183 + 1;
      STM32.I2C'ELAB_BODY;
      E212 := E212 + 1;
      STM32.I2C.DMA'ELAB_BODY;
      E217 := E217 + 1;
      STM32.USARTS'ELAB_BODY;
      E260 := E260 + 1;
      STM32.LTDC'ELAB_BODY;
      E292 := E292 + 1;
      Framebuffer_Ltdc'Elab_Spec;
      Framebuffer_Ltdc'Elab_Body;
      E273 := E273 + 1;
      STMPE811'ELAB_SPEC;
      STMPE811'ELAB_BODY;
      E306 := E306 + 1;
      Framebuffer_Ili9341'Elab_Spec;
      Touch_Panel_Stmpe811'Elab_Spec;
      STM32.BOARD'ELAB_SPEC;
      STM32.BOARD'ELAB_BODY;
      E176 := E176 + 1;
      Framebuffer_Ili9341'Elab_Body;
      E271 := E271 + 1;
      Touch_Panel_Stmpe811'Elab_Body;
      E303 := E303 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_lab3");

   procedure main is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
   end;

--  BEGIN Object file/option list
   --   /home/sabermaster/lab3/obj/development/lab3.o
   --   -L/home/sabermaster/lab3/obj/development/
   --   -L/home/sabermaster/lab3/obj/development/
   --   -L/home/sabermaster/lab3/alire/cache/dependencies/stm32_gui_0.1.0_e2e5c9d4/lib/
   --   -L/home/sabermaster/lab3/alire/cache/dependencies/adl_middleware_0.2.0_623c6913/lib/
   --   -L/home/sabermaster/lab3/alire/cache/dependencies/hal_0.3.0_095ae514/lib/
   --   -L/home/sabermaster/lab3/alire/cache/dependencies/stm32_hal_0.1.0_e9150be9/lib/
   --   -L/home/sabermaster/lab3/alire/cache/dependencies/cortex_m_0.5.0_39667d15/lib/
   --   -L/home/sabermaster/lab3/alire/cache/dependencies/stm32f429disco_0.1.0_3c54b8f0/lib/
   --   -L/home/sabermaster/lab3/alire/cache/dependencies/embedded_components_0.2.0_37c39b23/lib/
   --   -L/home/sabermaster/.config/alire/cache/dependencies/gnat_arm_elf_12.2.1_9be2ca0e/arm-eabi/lib/gnat/embedded-stm32f429disco/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
--  END Object file/option list   

end ada_main;
