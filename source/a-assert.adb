with System.Assertions;
package body Ada.Assertions is
   pragma Suppress (All_Checks);

   procedure Assert (
      Check : Boolean;
      Message : String := Debug.Source_Location) is
   begin
      if not Check then
         System.Assertions.Raise_Assert_Failure (Message);
      end if;
   end Assert;

end Ada.Assertions;
