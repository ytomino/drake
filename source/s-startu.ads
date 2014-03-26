pragma License (Unrestricted);
--  runtime unit
package System.Startup is
   pragma Preelaborate;

   --  command arguments (initialize.c)

   argc : Integer;
   pragma Export (C, argc, "gnat_argc");

   argv : Address;
   pragma Export (C, argv, "gnat_argv");

   envp : Address;
   pragma Export (C, envp, "gnat_envp");

   --  command status (exit.c)

   Exit_Status : Integer := 0;
   pragma Export (C, Exit_Status, "gnat_exit_status");

   --  initialize system (initialize.c)

   procedure Initialize (SEH : Address);
   pragma Export (C, Initialize, "__gnat_initialize");

   --  filled by gnatbind (init.c)

   Main_Priority : Integer := -1;
   pragma Export (C, Main_Priority, "__gl_main_priority");
   Main_CPU : Integer := -1;
   pragma Export (C, Main_CPU, "__gl_main_cpu");
   Time_Slice_Value : Integer := -1;
   pragma Export (C, Time_Slice_Value, "__gl_time_slice_val");
   WC_Encoding : Character := 'n';
   pragma Export (C, WC_Encoding, "__gl_wc_encoding");
   Locking_Policy : Character := ' ';
   pragma Export (C, Locking_Policy, "__gl_locking_policy");
   Queuing_Policy : Character := ' ';
   pragma Export (C, Queuing_Policy, "__gl_queuing_policy");
   Task_Dispatching_Policy : Character := ' ';
   pragma Export (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
   Priority_Specific_Dispatching : Address := Null_Address;
   pragma Export (C, Priority_Specific_Dispatching,
      "__gl_priority_specific_dispatching");
   Num_Specific_Dispatching : Integer := 0;
   pragma Export (C, Num_Specific_Dispatching,
      "__gl_num_specific_dispatching");
   Interrupt_States : access Character := null;
   pragma Export (C, Interrupt_States, "__gl_interrupt_states");
   Num_Interrupt_States : Integer := 0;
   pragma Export (C, Num_Interrupt_States, "__gl_num_interrupt_states");
   Unreserve_All_Interrupts : Integer := 0;
   pragma Export (C, Unreserve_All_Interrupts,
      "__gl_unreserve_all_interrupts");
   Zero_Cost_Exceptions : Integer := 0;
   pragma Export (C, Zero_Cost_Exceptions, "__gl_zero_cost_exceptions");
   Detect_Blocking : Integer := 0;
   pragma Export (C, Detect_Blocking, "__gl_detect_blocking");
   Default_Stack_Size : Integer := -1;
   pragma Export (C, Default_Stack_Size, "__gl_default_stack_size");
   Leap_Seconds_Support : Integer := 0;
   pragma Export (C, Leap_Seconds_Support, "__gl_leap_seconds_support");

   --  install handler (init.c)

   Handler_Installed : Integer := 0;
   pragma Export (C, Handler_Installed, "__gnat_handler_installed");

   procedure Install_Handler is null;
   pragma Export (C, Install_Handler, "__gnat_install_handler");

   --  breakpoint on start (s-stalib.adb)

   procedure Break_Start is null;
   pragma Export (C, Break_Start, "__gnat_break_start");

   --  finalize Ada runtime (s-stalib.adb)

   procedure AdaFinal is null;
   pragma Export (C, AdaFinal, "system__standard_library__adafinal");

   --  finalize system (final.c)

   procedure Finalize is null;
   pragma Export (C, Finalize, "__gnat_finalize");

   --  finalize library-level controlled objects (s-soflin.ads)

   type Finalize_Library_Objects_Handler is access procedure;
   pragma Suppress (Access_Check, Finalize_Library_Objects_Handler);

   Finalize_Library_Objects : Finalize_Library_Objects_Handler;
   pragma Suppress (Access_Check, Finalize_Library_Objects);
   pragma Export (Ada, Finalize_Library_Objects,
      "__gnat_finalize_library_objects");

end System.Startup;
