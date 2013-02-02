with Ada.Environment_Variables.Inside;
with System.Address_To_Named_Access_Conversions;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
package body Ada.Environment_Variables is
   pragma Suppress (All_Checks);
   use type Inside.Character_Access;

   package char_ptr_Conv is
      new System.Address_To_Named_Access_Conversions (
         Character,
         Inside.Character_Access);

   function "+" (Left : Cursor; Right : Integer) return Cursor
      renames Inside."+";

   function strlen (s : not null access Character)
      return System.Storage_Elements.Storage_Count;
   pragma Import (Intrinsic, strlen, "__builtin_strlen");

   --  implementation

   function Value (Name : String) return String is
      Result : constant Inside.Character_Access := Inside.Reference (Name);
   begin
      if Result = null then
         raise Constraint_Error;
      else
         return System.Zero_Terminated_Strings.Value (
            char_ptr_Conv.To_Address (Result));
      end if;
   end Value;

   function Exists (Name : String) return Boolean is
   begin
      return Inside.Reference (Name) /= null;
   end Exists;

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
      while Inside.Reference (I) /= null loop
         I := I + 1;
      end loop;
      while I /= Block loop
         I := I + (-1);
         Clear (Name (I).Element.all);
      end loop;
   end Clear;

   procedure Iterate (
      Process : not null access procedure (Name, Value : String))
   is
      I : Cursor := Inside.First;
   begin
      while Inside.Reference (I) /= null loop
         Process (
            Name (I).Element.all,
            Value (I).Element.all);
         I := I + 1;
      end loop;
   end Iterate;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Inside.Reference (Position)
         /= null;
   end Has_Element;

   function Name (Position : Cursor)
      return References.String.Slicing.Constant_Reference_Type
   is
      subtype Fixed_String is String (Positive);
      S : Fixed_String;
      for S'Address use char_ptr_Conv.To_Address (
         Inside.Reference (Position));
      I : Positive := 1;
   begin
      while S (I) /= '=' and then S (I) /= Character'Val (0) loop
         I := I + 1;
      end loop;
      return References.String.Slicing.Constant_Slice (
         S'Unrestricted_Access,
         1,
         I - 1);
   end Name;

   function Value (Position : Cursor)
      return References.String.Slicing.Constant_Reference_Type
   is
      subtype Fixed_String is String (Positive);
      S : Fixed_String;
      for S'Address use char_ptr_Conv.To_Address (
         Inside.Reference (Position));
      First : Positive;
      Last : Natural;
      I : Positive := 1;
   begin
      loop
         if S (I) = '=' then
            First := I + 1;
            Last := I + Natural (
               strlen (S (I + 1)'Unrestricted_Access));
            exit;
         elsif S (I) = Character'Val (0) then
            First := I;
            Last := I - 1;
            exit;
         end if;
         I := I + 1;
      end loop;
      return References.String.Slicing.Constant_Slice (
         S'Unrestricted_Access,
         First,
         Last);
   end Value;

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
