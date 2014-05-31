pragma License (Unrestricted);
--  runtime unit
with Ada.Unchecked_Deallocation;
with C.excpt;
with C.unwind;
with C.winnt;
package System.Unwind.Handling is
   pragma Preelaborate;

   --  (a-exexpr-gcc.adb)
   GNAT_Exception_Class : constant := 16#474e552d41646100#;

   Others_Value : aliased constant C.char := 'O';
   pragma Export (C, Others_Value, "__gnat_others_value");

   All_Others_Value : aliased constant C.char := 'A';
   pragma Export (C, All_Others_Value, "__gnat_all_others_value");

   Unhandled_Others_Value : aliased constant C.char := 'U'; -- SEH only
   pragma Export (C, Unhandled_Others_Value, "__gnat_unhandled_others_value");

   --  body of struct Unwind_Exception (a-exexpr-gcc.adb)
   type GNAT_GCC_Exception is record
      Header : aliased C.unwind.struct_Unwind_Exception;
      Occurrence : aliased Exception_Occurrence;
      Stack_Guard : Address; -- for skipping on stack overflow
      --  shortcut for phase2 (see exception.c in libobjc)
      landing_pad : C.unwind.Unwind_Ptr;
      ttype_filter : C.unwind.Unwind_Sword;
   end record;
   pragma Convention (C, GNAT_GCC_Exception);
   pragma Suppress_Initialization (GNAT_GCC_Exception);

   type GNAT_GCC_Exception_Access is access all GNAT_GCC_Exception;
   procedure Free is
      new Ada.Unchecked_Deallocation (
         GNAT_GCC_Exception,
         GNAT_GCC_Exception_Access);

   --  personality function (raise-gcc.c)
   function Personality (
      ABI_Version : C.signed_int;
      Phases : C.unwind.Unwind_Action;
      Exception_Class : C.unwind.Unwind_Exception_Class;
      Exception_Object : access C.unwind.struct_Unwind_Exception;
      Context : access C.unwind.struct_Unwind_Context)
      return C.unwind.Unwind_Reason_Code;
   pragma Export (C, Personality, "__gnat_personality_imp");
   pragma Compile_Time_Error (
      Personality'Access = C.unwind.Unwind_Personality_Fn'(null),
      "this expression is always false, for type check purpose");

   --  personality function (raise-gcc.c)
   function Personality_SEH (
      ms_exc : C.winnt.PEXCEPTION_RECORD;
      this_frame : C.void_ptr;
      ms_orig_context : C.winnt.PCONTEXT;
      ms_disp : C.winnt.PDISPATCHER_CONTEXT)
      return C.excpt.EXCEPTION_DISPOSITION;
   pragma Export (C, Personality_SEH, "__gnat_personality_seh0");

   --  by -fdump-tree-all, try ... exception be expanded below:
   --  try
   --    {
   --      ... user code ...
   --    }
   --  catch
   --    {
   --      ... .builtin_eh_filter ...
   --      catch (&exception_name ex. program_error)
   --        {
   --          {
   --            void * EXPTR = .builtin_eh_pointer (0);
   --            try
   --              {
   --                void * EXPTR = .builtin_eh_pointer (0);
   --                .gnat_begin_handler (EXPTR);
   --                --  system.soft_links.abort_undefer ();
   --                --  [gcc-4.7] abort_undefer is not called if zcx
   --                ... user code ...
   --              }
   --            finally
   --              {
   --                .gnat_end_handler (EXPTR);
   --                ... builtin_unwind_resume ...
   --              }
   --          }
   --        }
   --      ... builtin_unwind_resume ...
   --    }

end System.Unwind.Handling;
