pragma License (Unrestricted);
--  implementation unit specialized for Windows
with C;
package System.Native_Interrupts is
   pragma Preelaborate;

   subtype Interrupt_Id is C.signed_int;

   function Is_Blocked (Interrupt : Interrupt_Id) return Boolean is (False);

   procedure Block (Interrupt : Interrupt_Id)
      with Import, Convention => Ada, External_Name => "__drake_program_error";
      --  signal mask is not available in mingw-w64
   procedure Unblock (Interrupt : Interrupt_Id) is null;

   pragma Inline (Unblock); -- [gcc-7] can not skip calling null procedure

   procedure Raise_Interrupt (Interrupt : Interrupt_Id);

end System.Native_Interrupts;
