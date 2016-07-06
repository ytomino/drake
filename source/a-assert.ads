pragma License (Unrestricted);
package Ada.Assertions is
   pragma Pure;

   Assertion_Error : exception
      with Export, Convention => Ada, External_Name => "assertion_error";

   --  modified
   --  Assert reports the source location if it's called without a message.
--  procedure Assert (Check : Boolean);
--  procedure Assert (Check : Boolean; Message : String);
   procedure Assert (
      Check : Boolean;
      Message : String := Debug.Source_Location);

   --  extended
   procedure Raise_Assertion_Error (
      Message : String := Debug.Source_Location);
   pragma No_Return (Raise_Assertion_Error);

   --  Note: Raise_Assertion_Error is called by pragma Assert.

end Ada.Assertions;
