with Ada.Exception_Identification;
package body Ada.Assertions is
   pragma Suppress (All_Checks);

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
      Exception_Identification.Raise_Exception (
         Assertion_Error'Identity,
         Message => Message);
   end Raise_Assertion_Error;

end Ada.Assertions;
