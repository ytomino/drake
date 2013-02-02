with Ada.Environment_Variables.Inside;
package body Ada.Environment_Variables is
   pragma Suppress (All_Checks);

   function "+" (Left : Cursor; Right : Integer) return Cursor
      renames Inside."+";

   --  implementation

   function Value (Name : String) return String
      renames Inside.Value;

   function Exists (Name : String) return Boolean
      renames Inside.Exists;

   procedure Set (Name : String; Value : String) is
      Error : Boolean;
   begin
      Inside.Set (Name, Value, Error => Error);
      if Error then
         raise Constraint_Error;
      end if;
   end Set;

   procedure Clear (Name : String) is
      Error : Boolean;
   begin
      Inside.Clear (Name, Error => Error);
      if Error then
         raise Constraint_Error;
      end if;
   end Clear;

   procedure Clear is
      Block : constant Cursor := Inside.First;
      I : Cursor := Block;
   begin
      while Inside.Has_Element (I) loop
         I := I + 1;
      end loop;
      while I /= Block loop
         I := I + (-1);
         Clear (Name (I));
      end loop;
   end Clear;

   procedure Iterate (
      Process : not null access procedure (Name, Value : String))
   is
      I : Cursor := Inside.First;
   begin
      while Has_Element (I) loop
         Process (Name (I), Value (I));
         I := I + 1;
      end loop;
   end Iterate;

   function Has_Element (Position : Cursor) return Boolean
      renames Inside.Has_Element;

   function Name (Position : Cursor) return String
      renames Inside.Name;

   function Value (Position : Cursor) return String
      renames Inside.Value;

   function Iterate return Iterator is
   begin
      return (null record);
   end Iterate;

   overriding function First (Object : Iterator) return Cursor is
      pragma Unreferenced (Object);
   begin
      return Inside.First;
   end First;

   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return Position + 1;
   end Next;

end Ada.Environment_Variables;
