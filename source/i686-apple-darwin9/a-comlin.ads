pragma License (Unrestricted);
package Ada.Command_Line is
   pragma Preelaborate;

   function Argument_Count return Natural;

   function Argument (Number : Positive) return String;

   function Command_Name return String;

   type Exit_Status is new Integer; -- implementation-defined integer type

   Success : constant Exit_Status;
   Failure : constant Exit_Status;

   procedure Set_Exit_Status (Code : Exit_Status);

private

   Success : constant Exit_Status := 0;
   Failure : constant Exit_Status := 1;

end Ada.Command_Line;
