with Ada.Assertions; -- force to link exception data
pragma Unreferenced (Ada.Assertions);
with System.Unwind;
pragma Warnings (Off, System.Unwind); -- break "pure" rule
with System.Unwind.Raising;
pragma Warnings (Off, System.Unwind.Raising); -- break "pure" rule
package body System.Assertions is
   pragma Suppress (All_Checks);

   --  skip Ada.Exceptions
   Data : aliased constant Unwind.Exception_Data;
   pragma Import (Ada, Data, "assertion_error");

   --  implementation

   procedure Raise_Assert_Failure (Msg : String) is
   begin
      Unwind.Raising.Raise_Exception (
         Data'Access,
         Message => Msg);
   end Raise_Assert_Failure;

end System.Assertions;
