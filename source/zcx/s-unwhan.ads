pragma License (Unrestricted);
--  runtime unit
with Ada.Unchecked_Deallocation;
with C.unwind;
package System.Unwind.Handling is
   pragma Preelaborate;

   --  (a-exexpr-gcc.adb)
   GNAT_Exception_Class : constant := 16#474e552d41646100#;

   Others_Value : aliased constant C.unwind.Unwind_Ptr := 16#7FFF#;
   pragma Export (C, Others_Value, "__gnat_others_value");

   All_Others_Value : aliased constant C.unwind.Unwind_Ptr := 16#7FFF#;
   pragma Export (C, All_Others_Value, "__gnat_all_others_value");

   --  body of struct Unwind_Exception (a-exexpr-gcc.adb)
   type GNAT_GCC_Exception is record
      Header : aliased C.unwind.struct_Unwind_Exception;
      Id : Standard_Library.Exception_Data_Ptr;
      N_Cleanups_To_Trigger : Integer;
      Next_Exception : Exception_Occurrence_Access;
      --  shortcut for phase2 (see exception.c in libobjc)
      landing_pad : C.unwind.Unwind_Ptr;
      ttype_filter : C.unwind.Unwind_Sword;
   end record;
   pragma Convention (C, GNAT_GCC_Exception);
   pragma Suppress_Initialization (GNAT_GCC_Exception);

   type GNAT_GCC_Exception_Access is access all GNAT_GCC_Exception;
   procedure Free is new Ada.Unchecked_Deallocation (
      GNAT_GCC_Exception,
      GNAT_GCC_Exception_Access);

   --  (a-exexpr-gcc.adb)
   procedure Save_Occurrence_And_Private (
      Target : out Exception_Occurrence;
      Source : Exception_Occurrence);

   --  hook for entering an exception handler (a-exexpr-gcc.adb)
   procedure Begin_Handler (GCC_Exception : GNAT_GCC_Exception_Access);
   pragma Export (C, Begin_Handler, "__gnat_begin_handler");

   --  hook for leaving an exception handler (a-exexpr-gcc.adb)
   procedure End_Handler (GCC_Exception : GNAT_GCC_Exception_Access);
   pragma Export (C, End_Handler, "__gnat_end_handler");

   --  personality function (raise-gcc.c)
   function Personality (
      ABI_Version : C.signed_int;
      Phases : C.unwind.Unwind_Action;
      Exception_Class : C.unwind.Unwind_Exception_Class;
      Exception_Object : access C.unwind.struct_Unwind_Exception;
      Context : access C.unwind.struct_Unwind_Context)
      return C.unwind.Unwind_Reason_Code;
   pragma Export (C, Personality, "__gnat_personality_v0");
   pragma Compile_Time_Error (
      Personality'Access = C.unwind.Unwind_Personality_Fn'(null),
      "this expression is always false, for type check purpose");

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
   --                system.soft_links.abort_undefer ();
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
