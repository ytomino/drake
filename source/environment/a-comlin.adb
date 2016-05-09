with System.Startup;
package body Ada.Command_Line is

   --  implementation

   function Argument (Number : Positive) return String is
      pragma Check (Pre,
         Check => Number <= Argument_Count or else raise Constraint_Error);
   begin
      return System.Native_Command_Line.Argument (Number);
   end Argument;

   function Has_Element (Position : Natural) return Boolean is
      pragma Check (Pre,
         Check => Position <= Argument_Count or else raise Constraint_Error);
   begin
      return Position > 0;
   end Has_Element;

   function Iterate return Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Iterate (1, Argument_Count);
   end Iterate;

   function Iterate (First : Positive; Last : Natural)
      return Iterator_Interfaces.Reversible_Iterator'Class
   is
      pragma Check (Pre,
         Check => (First <= Argument_Count + 1 and then Last <= Argument_Count)
            or else raise Constraint_Error);
      Actual_First : Natural := First;
      Actual_Last : Natural := Last;
   begin
      if Actual_Last < Actual_First then
         Actual_First := 0;
         Actual_Last := 0;
      end if;
      return Iterator'(First => Actual_First, Last => Actual_Last);
   end Iterate;

   function Command_Name return String is
   begin
      return System.Native_Command_Line.Argument (0);
   end Command_Name;

   procedure Set_Exit_Status (Code : Exit_Status) is
   begin
      System.Startup.Exit_Status := Integer (Code);
   end Set_Exit_Status;

   --  implementation of the non-abstract iterator

   overriding function First (Object : Concrete_Iterator) return Natural is
      pragma Unreferenced (Object);
   begin
      if Argument_Count = 0 then
         return 0;
      else
         return 1;
      end if;
   end First;

   overriding function Next (Object : Concrete_Iterator; Position : Natural)
      return Natural
   is
      pragma Unreferenced (Object);
   begin
      if Position >= Argument_Count then
         return 0;
      else
         return Position + 1;
      end if;
   end Next;

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
