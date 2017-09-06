with Ada.References.Strings;
with System.Address_To_Constant_Access_Conversions;
with System.Startup;
with System.Storage_Elements;
package body Ada.Bind_Time_Variables is
   use type System.Storage_Elements.Storage_Offset;

   type Length_Type is mod 2 ** Character'Size;
   type Length_Access is access constant Length_Type;
   package LA_Conv is
      new System.Address_To_Constant_Access_Conversions (
         Length_Type,
         Length_Access);

   function Read_Length (Position : System.Address) return Length_Type
      with Convention => Intrinsic;
   pragma Inline_Always (Read_Length);

   function Read_Length (Position : System.Address) return Length_Type is
   begin
      return LA_Conv.To_Pointer (Position).all;
   end Read_Length;

   function First return Cursor;
   function Next (Position : Cursor) return Cursor;

   function First return Cursor is
      Result : constant System.Address := System.Startup.Bind_Env_Addr;
   begin
      if Read_Length (Result) = 0 then
         return Cursor (System.Null_Address);
      else
         return Cursor (Result);
      end if;
   end First;

   function Next (Position : Cursor) return Cursor is
      Name_Length : constant System.Storage_Elements.Storage_Offset :=
         System.Storage_Elements.Storage_Offset (
            Read_Length (System.Address (Position)));
      Value_Address : constant System.Address :=
         System.Address (Position) + 1 + Name_Length;
      Value_Length : constant System.Storage_Elements.Storage_Offset :=
         System.Storage_Elements.Storage_Offset (Read_Length (Value_Address));
      Result : constant System.Address := Value_Address + 1 + Value_Length;
   begin
      if Read_Length (Result) = 0 then
         return Cursor (System.Null_Address);
      else
         return Cursor (Result);
      end if;
   end Next;

   function Name_Reference (Position : Cursor)
      return References.Strings.Slicing.Constant_Reference_Type;
   function Value_Reference (Position : Cursor)
      return References.Strings.Slicing.Constant_Reference_Type;

   function Name_Reference (Position : Cursor)
      return References.Strings.Slicing.Constant_Reference_Type
   is
      type String_Constant_Access is access constant String;
      Name_Length : constant Natural :=
         Natural (Read_Length (System.Address (Position)));
      Name_All : aliased String (1 .. Name_Length);
      for Name_All'Address use System.Address (Position) + 1;
   begin
      return References.Strings.Slicing.Constant_Slice (
         String_Constant_Access'(Name_All'Unrestricted_Access).all,
         First => 1,
         Last => Name_Length);
   end Name_Reference;

   function Value_Reference (Position : Cursor)
      return References.Strings.Slicing.Constant_Reference_Type
   is
      type String_Constant_Access is access constant String;
      Name_Length : constant System.Storage_Elements.Storage_Offset :=
         System.Storage_Elements.Storage_Offset (
            Read_Length (System.Address (Position)));
      Value_Address : constant System.Address :=
         System.Address (Position) + 1 + Name_Length;
      Value_Length : constant Natural :=
         Natural (Read_Length (Value_Address));
      Value_All : aliased String (1 .. Value_Length);
      for Value_All'Address use Value_Address + 1;
   begin
      return References.Strings.Slicing.Constant_Slice (
         String_Constant_Access'(Value_All'Unrestricted_Access).all,
         First => 1,
         Last => Value_Length);
   end Value_Reference;

   --  implementation

   function Value (Name : String) return String is
      I : Cursor := First;
   begin
      while Has_Element (I) loop
         if Name_Reference (I).Element.all = Name then
            return Value (I);
         end if;
         I := Next (I);
      end loop;
      raise Constraint_Error;
   end Value;

   function Value (Name : String; Default : String) return String is
      I : Cursor := First;
   begin
      while Has_Element (I) loop
         if Name_Reference (I).Element.all = Name then
            return Value (I);
         end if;
         I := Next (I);
      end loop;
      return Default;
   end Value;

   function Exists (Name : String) return Boolean is
      I : Cursor := First;
   begin
      while Has_Element (I) loop
         if Name_Reference (I).Element.all = Name then
            return True;
         end if;
         I := Next (I);
      end loop;
      return False;
   end Exists;

   procedure Iterate (
      Process : not null access procedure (Name, Value : String))
   is
      I : Cursor := First;
   begin
      while Has_Element (I) loop
         Process (
            Name_Reference (I).Element.all,
            Value_Reference (I).Element.all);
         I := Next (I);
      end loop;
   end Iterate;

   --  implementation of iterators

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= Cursor (System.Null_Address);
   end Has_Element;

   function Name (Position : Cursor) return String is
   begin
      return Name_Reference (Position).Element.all;
   end Name;

   function Value (Position : Cursor) return String is
   begin
      return Value_Reference (Position).Element.all;
   end Value;

   function Iterate return Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Iterator'(null record);
   end Iterate;

   overriding function First (Object : Iterator) return Cursor is
      pragma Unreferenced (Object);
   begin
      return First;
   end First;

   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return Next (Position);
   end Next;

end Ada.Bind_Time_Variables;
