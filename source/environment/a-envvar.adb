with Ada.Environment_Variables.Inside;
package body Ada.Environment_Variables is
   pragma Suppress (All_Checks);

   procedure Start (Object : out Iterator);
   procedure Start (Object : out Iterator) is
   begin
      Object.Block := Inside.Get_Block;
   end Start;

   --  implementation

   function Value (Name : String) return String
      renames Inside.Value;

   function Exists (Name : String) return Boolean
      renames Inside.Exists;

   procedure Set (Name : String; Value : String)
      renames Inside.Set;

   procedure Clear (Name : String)
      renames Inside.Clear;

   procedure Clear
      renames Inside.Clear;

   procedure Iterate (
      Process : not null access procedure (Name, Value : String))
   is
      Ite : Iterator; -- controlled
      I : Cursor;
   begin
      Start (Ite);
      I := First (Ite);
      while Has_Element (I) loop
         Process (Name (I), Value (I));
         I := Next (Ite, I);
      end loop;
   end Iterate;

   function Has_Element (Position : Cursor) return Boolean
      renames Inside.Has_Element;

   function Name (Position : Cursor) return String
      renames Inside.Name;

   function Value (Position : Cursor) return String
      renames Inside.Value;

   function Iterate return Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Result : Iterator do
         Start (Result);
      end return;
   end Iterate;

   overriding procedure Finalize (Object : in out Iterator) is
   begin
      Inside.Release_Block (Object.Block);
   end Finalize;

   overriding function First (Object : Iterator) return Cursor is
   begin
      return Inside.First (Object.Block);
   end First;

   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor is
   begin
      return Inside.Next (Object.Block, Position);
   end Next;

end Ada.Environment_Variables;
