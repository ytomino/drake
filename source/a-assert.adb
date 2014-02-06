with System.Unwind;
pragma Warnings (Off, System.Unwind); -- break "pure" rule
with System.Unwind.Raising;
pragma Warnings (Off, System.Unwind.Raising); -- break "pure" rule
package body Ada.Assertions is
   pragma Suppress (All_Checks);

   --  skip Ada.Exceptions
   Assertion_Error_Data : aliased constant System.Unwind.Exception_Data;
   pragma Import (Ada, Assertion_Error_Data, "assertion_error");

   --  implementation

   procedure Assert (
      Check : Boolean;
      Message : String := Debug.Source_Location) is
   begin
      if not Check then
         Raise_Assertion_Error (Message);
      end if;
   end Assert;

   procedure Raise_Assertion_Error (
      Message : String := Debug.Source_Location) is
   begin
      System.Unwind.Raising.Raise_Exception (
         Assertion_Error_Data'Access,
         Message => Message);
   end Raise_Assertion_Error;

end Ada.Assertions;
