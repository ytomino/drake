with System.Address_To_Constant_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
with System.Environment_Block;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
with C.stdlib;
with C.string;
package body Ada.Environment_Variables.Inside is
   pragma Suppress (All_Checks);
   use type System.Storage_Elements.Storage_Offset;
   use type C.char_ptr;
   use type C.char_ptr_ptr;
   use type C.signed_int;
   use type C.ptrdiff_t;

   package char_const_ptr_Conv is
      new System.Address_To_Constant_Access_Conversions (
         C.char,
         C.char_const_ptr);

   package char_ptr_ptr_Conv is
      new System.Address_To_Named_Access_Conversions (
         C.char_ptr,
         C.char_ptr_ptr);

   function getenv (Name : String) return C.char_ptr;
   function getenv (Name : String) return C.char_ptr is
      Z_Name : String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
   begin
      return C.stdlib.getenv (C_Name (0)'Access);
   end getenv;

   procedure Do_Separate (
      Item : not null access constant C.char;
      Name_Length : out C.size_t;
      Value : out C.char_const_ptr);
   procedure Do_Separate (
      Item : not null access constant C.char;
      Name_Length : out C.size_t;
      Value : out C.char_const_ptr)
   is
      P : C.char_ptr;
   begin
      P := C.string.strchr (Item, C.char'Pos ('='));
      if P /= null then
         Name_Length := C.size_t (
            char_const_ptr_Conv.To_Address (C.char_const_ptr (P))
            - char_const_ptr_Conv.To_Address (C.char_const_ptr (Item)));
         Value := char_const_ptr_Conv.To_Pointer (
            char_const_ptr_Conv.To_Address (C.char_const_ptr (P)) + 1);
      else
         Name_Length := C.string.strlen (Item);
         Value := char_const_ptr_Conv.To_Pointer (
            char_const_ptr_Conv.To_Address (C.char_const_ptr (Item))
            + System.Storage_Elements.Storage_Offset (Name_Length));
      end if;
   end Do_Separate;

   --  implementation

   function Value (Name : String) return String is
      Result : constant C.char_ptr := getenv (Name);
   begin
      if Result = null then
         raise Constraint_Error;
      else
         return System.Zero_Terminated_Strings.Value (Result);
      end if;
   end Value;

   function Value (Name : String; Default : String) return String is
      Result : constant C.char_ptr := getenv (Name);
   begin
      if Result = null then
         return Default;
      else
         return System.Zero_Terminated_Strings.Value (Result);
      end if;
   end Value;

   function Exists (Name : String) return Boolean is
      Item : constant C.char_ptr := getenv (Name);
   begin
      return Item /= null;
   end Exists;

   procedure Set (Name : String; Value : String) is
      Z_Name : String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
      Z_Value : String := Value & Character'Val (0);
      C_Value : C.char_array (C.size_t);
      for C_Value'Address use Z_Value'Address;
   begin
      if C.stdlib.setenv (C_Name (0)'Access, C_Value (0)'Access, 1) < 0 then
         raise Constraint_Error;
      end if;
   end Set;

   procedure Clear (Name : String) is
      Z_Name : String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
   begin
      if C.stdlib.unsetenv (C_Name (0)'Access) < 0 then
         raise Constraint_Error;
      end if;
   end Clear;

   procedure Clear is
      Block : constant C.char_ptr_ptr := System.Environment_Block;
      I : C.char_ptr_ptr := Block;
   begin
      while I.all /= null loop
         I := char_ptr_ptr_Conv.To_Pointer (
            char_ptr_ptr_Conv.To_Address (I)
            + C.char_ptr'Size / Standard'Storage_Unit);
      end loop;
      while I /= Block loop
         I := char_ptr_ptr_Conv.To_Pointer (
            char_ptr_ptr_Conv.To_Address (I)
            - C.char_ptr'Size / Standard'Storage_Unit);
         declare
            Item : constant C.char_ptr := I.all;
            subtype Fixed_String is String (Positive);
            Name : Fixed_String;
            for Name'Address use
               char_const_ptr_Conv.To_Address (C.char_const_ptr (Item));
            Name_Length : C.size_t;
            Value : C.char_const_ptr;
         begin
            Do_Separate (Item, Name_Length, Value);
            Clear (Name (1 .. Natural (Name_Length)));
         end;
      end loop;
   end Clear;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return char_ptr_ptr_Conv.To_Pointer (System.Address (Position)).all /=
         null;
   end Has_Element;

   function Name (Position : Cursor) return String is
      Item : constant C.char_ptr :=
         char_ptr_ptr_Conv.To_Pointer (System.Address (Position)).all;
      subtype Fixed_String is String (Positive);
      Name : Fixed_String;
      for Name'Address use
         char_const_ptr_Conv.To_Address (C.char_const_ptr (Item));
      Name_Length : C.size_t;
      Value : C.char_const_ptr;
   begin
      Do_Separate (Item, Name_Length, Value);
      return Name (1 .. Natural (Name_Length));
   end Name;

   function Value (Position : Cursor) return String is
      Item : constant C.char_ptr :=
         char_ptr_ptr_Conv.To_Pointer (System.Address (Position)).all;
      Name_Length : C.size_t;
      Value : C.char_const_ptr;
   begin
      Do_Separate (Item, Name_Length, Value);
      return System.Zero_Terminated_Strings.Value (Value);
   end Value;

   function Get_Block return System.Address is
   begin
      return System.Null_Address;
   end Get_Block;

   function First (Block : System.Address) return Cursor is
      pragma Unreferenced (Block);
   begin
      return Cursor (char_ptr_ptr_Conv.To_Address (System.Environment_Block));
   end First;

   function Next (Block : System.Address; Position : Cursor) return Cursor is
      pragma Unreferenced (Block);
   begin
      return Cursor (
         System.Address (Position)
         + C.char_ptr'Size / Standard'Storage_Unit);
   end Next;

end Ada.Environment_Variables.Inside;
