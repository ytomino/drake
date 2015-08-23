pragma License (Unrestricted);
with Ada.Iterator_Interfaces;
package Ada.Command_Line is
   pragma Preelaborate;

   function Argument_Count return Natural;
   pragma Inline (Argument_Count); -- renamed

   function Argument (Number : Positive) return String;

   --  extended from here
   --  The iterator.

   function Has_Element (Position : Natural) return Boolean;
   pragma Inline (Has_Element);

   package Iterator_Interfaces is
      new Ada.Iterator_Interfaces (Natural, Has_Element);

   function Iterate return Iterator_Interfaces.Reversible_Iterator'Class;
   function Iterate (First : Positive; Last : Natural)
      return Iterator_Interfaces.Reversible_Iterator'Class;

   --  to here

   function Command_Name return String;

   type Exit_Status is new Integer; -- implementation-defined integer type

   Success : constant Exit_Status;
   Failure : constant Exit_Status;

   procedure Set_Exit_Status (Code : Exit_Status);

private

   type Iterator is new Iterator_Interfaces.Reversible_Iterator with record
      First : Positive;
      Last : Natural;
   end record;

   overriding function First (Object : Iterator) return Natural;
   overriding function Next (Object : Iterator; Position : Natural)
      return Natural;
   overriding function Last (Object : Iterator) return Natural;
   overriding function Previous (Object : Iterator; Position : Natural)
      return Natural;

   Success : constant Exit_Status := 0;
   Failure : constant Exit_Status := 1;

end Ada.Command_Line;
