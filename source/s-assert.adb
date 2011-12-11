with Ada.Assertions; -- force to link exception data
pragma Unreferenced (Ada.Assertions);
with System.Standard_Library;
pragma Warnings (Off, System.Standard_Library); -- break "pure" rule
with System.Unwind.Raising;
pragma Warnings (Off, System.Unwind.Raising); -- break "pure" rule
package body System.Assertions is
   pragma Suppress (All_Checks);

   --  skip Ada.Exceptions
   Data : aliased constant Standard_Library.Exception_Data;
   pragma Import (Ada, Data, "assertion_error");

   procedure Raise_Assert_Failure (Msg : String) is
   begin
      Unwind.Raising.Raise_Exception (
         Data'Access,
         Message => Msg);
   end Raise_Assert_Failure;

end System.Assertions;
