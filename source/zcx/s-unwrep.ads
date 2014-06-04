pragma License (Unrestricted);
--  runtime unit
with C.unwind;
package System.Unwind.Representation is
   pragma Preelaborate;

   --  (a-exexpr-gcc.adb)
   GNAT_Exception_Class : constant := 16#474e552d41646100#;

   --  equivalent to GNAT_GCC_Exception (a-excmac-gcc.adb)
   type Machine_Occurrence is record
      Header : aliased C.unwind.struct_Unwind_Exception;
      Occurrence : aliased Exception_Occurrence;
      Stack_Guard : Address; -- for skipping on stack overflow
      --  shortcut for phase2 (see exception.c in libobjc)
      landing_pad : C.unwind.Unwind_Ptr;
      ttype_filter : C.unwind.Unwind_Sword;
   end record;
   pragma Convention (C, Machine_Occurrence);
   pragma Suppress_Initialization (Machine_Occurrence);

   type Machine_Occurrence_Access is access all Machine_Occurrence;
   for Machine_Occurrence_Access'Storage_Size use 0;

   --  equivalent to Allocate_Occurrence (a-exexpr-gcc.adb)
   function New_Machine_Occurrence return not null Machine_Occurrence_Access;

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

end System.Unwind.Representation;
