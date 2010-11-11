pragma License (Unrestricted);
package Ada.Assertions is
   pragma Pure;

   Assertion_Error : exception;
   pragma Export (Ada, Assertion_Error, "assertion_error");

--  procedure Assert (Check : Boolean);
--  procedure Assert (Check : Boolean; Message : String);

   --  extended
   procedure Assert (
      Check : Boolean;
      Message : String := Debug.Source_Location);

end Ada.Assertions;
