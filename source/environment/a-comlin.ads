pragma License (Unrestricted);
with Ada.Iterator_Interfaces;
private with System.Native_Command_Line;
package Ada.Command_Line is
   pragma Preelaborate;

   function Argument_Count return Natural;
   pragma Pure_Function (Argument_Count);
   pragma Inline (Argument_Count); -- renamed

   function Argument (Number : Positive) return String;
   pragma Inline (Argument);

   --  extended from here
   --  The iterator.

   function Has_Element (Position : Natural) return Boolean;
   pragma Inline (Has_Element);

   package Iterator_Interfaces is
      new Ada.Iterator_Interfaces (Natural, Has_Element);

   function Iterate return Iterator_Interfaces.Reversible_Iterator'Class;
   function Iterate (First : Positive; Last : Natural)
      return Iterator_Interfaces.Reversible_Iterator'Class;

   --  extended from here
   --  A non-abstract type of the above iterator.
   --  It's useful for some workarounds, and will be removed in the future.

   type Concrete_Iterator is
      limited new Iterator_Interfaces.Forward_Iterator with null record;

   --  to here

   function Command_Name return String;

   type Exit_Status is new Integer; -- implementation-defined integer type

   Success : constant Exit_Status;
   Failure : constant Exit_Status;

   procedure Set_Exit_Status (Code : Exit_Status);

private

   function Argument_Count return Natural
      renames System.Native_Command_Line.Argument_Count;

   overriding function First (Object : Concrete_Iterator) return Natural;
   overriding function Next (Object : Concrete_Iterator; Position : Natural)
      return Natural;

   type Iterator is new Iterator_Interfaces.Reversible_Iterator with record
      First : Natural; -- 0 means an empty range
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
