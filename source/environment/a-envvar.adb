package body Ada.Environment_Variables is

   procedure Start (Object : out Iterator);
   procedure Start (Object : out Iterator) is
   begin
      Object.Block := System.Native_Environment_Variables.Get_Block;
   end Start;

   --  implementation

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

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return System.Native_Environment_Variables.Has_Element (
         System.Native_Environment_Variables.Cursor (Position));
   end Has_Element;

   function Name (Position : Cursor) return String is
   begin
      return System.Native_Environment_Variables.Name (
         System.Native_Environment_Variables.Cursor (Position));
   end Name;

   function Value (Position : Cursor) return String is
   begin
      return System.Native_Environment_Variables.Value (
         System.Native_Environment_Variables.Cursor (Position));
   end Value;

   function Iterate return Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Result : Iterator do
         Start (Result);
      end return;
   end Iterate;

   overriding procedure Finalize (Object : in out Iterator) is
   begin
      System.Native_Environment_Variables.Release_Block (Object.Block);
   end Finalize;

   overriding function First (Object : Iterator) return Cursor is
   begin
      return Cursor (System.Native_Environment_Variables.First (Object.Block));
   end First;

   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor is
   begin
      return Cursor (
         System.Native_Environment_Variables.Next (
            Object.Block,
            System.Native_Environment_Variables.Cursor (Position)));
   end Next;

end Ada.Environment_Variables;
