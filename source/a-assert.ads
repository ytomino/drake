pragma License (Unrestricted);
package Ada.Assertions is
   pragma Pure;

   Assertion_Error : exception;
   pragma Export (Ada, Assertion_Error, "assertion_error");

   --  modified
   --  Assert reports the source location if it's called without message.
--  procedure Assert (Check : Boolean);
--  procedure Assert (Check : Boolean; Message : String);
   procedure Assert (
      Check : Boolean;
      Message : String := Debug.Source_Location);

end Ada.Assertions;
