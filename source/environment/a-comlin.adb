with Ada.Command_Line.Inside;
with System.Startup;
package body Ada.Command_Line is
   pragma Suppress (All_Checks);

   --  implementation

   function Argument_Count return Natural
      renames Inside.Argument_Count;

   function Argument (Number : Positive) return String is
   begin
      if Number > Inside.Argument_Count then
         raise Constraint_Error;
      else
         return Inside.Argument (Number);
      end if;
   end Argument;

   function Has_Element (Position : Natural) return Boolean is
   begin
      return Position in 1 .. Argument_Count;
   end Has_Element;

   function Iterate return Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Iterate (1, Argument_Count);
   end Iterate;

   function Iterate (First : Positive; Last : Natural)
      return Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Iterator'(First, Last);
   end Iterate;

   function Command_Name return String is
   begin
      return Inside.Argument (0);
   end Command_Name;

   procedure Set_Exit_Status (Code : Exit_Status) is
   begin
      System.Startup.Exit_Status := Integer (Code);
   end Set_Exit_Status;

   --  implementation of the iterator

   overriding function First (Object : Iterator) return Natural is
   begin
      return Object.First;
   end First;

   overriding function Next (Object : Iterator; Position : Natural)
      return Natural is
   begin
      if Position >= Object.Last then
         return 0;
      else
         return Position + 1;
      end if;
   end Next;

   overriding function Last (Object : Iterator) return Natural is
   begin
      return Object.Last;
   end Last;

   overriding function Previous (Object : Iterator; Position : Natural)
      return Natural is
   begin
      if Position <= Object.First then
         return 0;
      else
         return Position - 1;
      end if;
   end Previous;

end Ada.Command_Line;
