pragma License (Unrestricted);
--  generic implementation of Interfaces.C.Strings
with Interfaces.C.Pointers;
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
   type Element_Array is array (size_t range <>) of aliased Character_Type;
   with package Pointers is new Interfaces.C.Pointers (
      Index => size_t,
      Element => Character_Type,
      Element_Array => Element_Array,
      Default_Terminator => Character_Type'Val (0));
package Interfaces.C.Zero_Terminated_Strings is
--  pragma Preelaborate;
   pragma Pure;

   subtype char_array is Element_Array; --  extended

   type char_array_access is access all char_array;
   for char_array_access'Storage_Size use 0; --  extended

--  type chars_ptr is private;
--  pragma Preelaborable_Initialization (chars_ptr);
   subtype chars_ptr is Pointers.Pointer; --  extended
   function "=" (Left, Right : chars_ptr) return Boolean renames Pointers."=";

   type chars_ptr_array is array (size_t range <>) of aliased chars_ptr;

   Null_Ptr : constant chars_ptr := null;

--  function To_Chars_Ptr (
--    Item : char_array_access;
--    Nul_Check : Boolean := False)
--    return chars_ptr;
   function To_Chars_Ptr (
      Item : not null access char_array;
      Nul_Check : Boolean := False)
      return not null chars_ptr;
   pragma Pure_Function (To_Chars_Ptr);
   pragma Inline_Always (To_Chars_Ptr);

--  function New_Char_Array (Chars : in char_array) return chars_ptr;
   function New_Char_Array (Chars : char_array) return not null chars_ptr;

--  function New_String (Str : String) return chars_ptr;
   function New_String (Str : String_Type) return not null chars_ptr;

   procedure Free (Item : in out chars_ptr);

--  Dereference_Error : exception;

--  function Value (Item : chars_ptr) return char_array;
   function Value (Item : not null access constant Character_Type)
      return char_array;

--  function Value (Item : chars_ptr; Length : size_t)
--    return char_array;
   function Value (Item : access constant Character_Type; Length : size_t)
      return char_array;

--  function Value (Item : chars_ptr) return String;
   function Value (Item : not null access constant Character_Type)
      return String_Type;

--  function Value (Item : chars_ptr; Length : in size_t) return String;
   function Value (Item : access constant Character_Type; Length : size_t)
      return String_Type;

--  function Strlen (Item : chars_ptr) return size_t;
   function Strlen (Item : not null access constant Character_Type)
      return size_t;

--  procedure Update (
--    Item : chars_ptr;
--    Offset : size_t;
--    Chars : char_array;
--    Check : Boolean := True);
   procedure Update (
      Item : not null access Character_Type;
      Offset : size_t;
      Chars : char_array;
      Check : Boolean := True);

--  procedure Update (
--    Item : chars_ptr;
--    Offset : size_t;
--    Str : String;
--    Check : Boolean := True);
   procedure Update (
      Item : not null access Character_Type;
      Offset : size_t;
      Str : String_Type;
      Check : Boolean := True);

--  Update_Error : exception;

   --  extended

   subtype Ada_Character is Character_Type;
   subtype Ada_String is String_Type;

   subtype const_chars_ptr is Pointers.Constant_Pointer;
   function "=" (Left, Right : const_chars_ptr) return Boolean
      renames Pointers."=";

   function To_Chars_Ptr (Item : not null access String_Type)
      return not null chars_ptr;
   pragma Pure_Function (To_Chars_Ptr);
   pragma Inline_Always (To_Chars_Ptr);

   function To_Const_Chars_Ptr (Item : not null access constant String_Type)
      return not null const_chars_ptr;
   pragma Pure_Function (To_Const_Chars_Ptr);
   pragma Inline_Always (To_Const_Chars_Ptr);

   procedure Update (
      Item : not null access Character_Type;
      Source : not null access constant Character_Type;
      Length : size_t);

   function New_Chars_Ptr (
      Item : not null access constant Character_Type;
      Length : size_t)
      return not null chars_ptr;

   function New_Chars_Ptr (Item : not null access constant Character_Type)
      return not null chars_ptr;

end Interfaces.C.Zero_Terminated_Strings;
