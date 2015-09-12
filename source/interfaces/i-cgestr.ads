pragma License (Unrestricted);
--  generalized unit of Interfaces.C.Strings
with Interfaces.C.Pointers;
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
   type Element is new Character_Type;
   type Element_Array is array (size_t range <>) of aliased Element;
   with package Pointers is
      new C.Pointers (
         Index => size_t,
         Element => Element,
         Element_Array => Element_Array,
         Default_Terminator => Element'Val (0));
   with function To_C (
      Item : String_Type;
      Append_Nul : Boolean := True;
      Substitute : Element_Array)
      return Element_Array;
   with function To_Ada (
      Item : Element_Array;
      Trim_Nul : Boolean := True;
      Substitute : String_Type)
      return String_Type;
package Interfaces.C.Generic_Strings is
   pragma Preelaborate;

--  type char_array_access is access all char_array;
   type char_array_access is access all Element_Array;

--  type chars_ptr is private;
--  pragma Preelaborable_Initialization (chars_ptr);
   subtype chars_ptr is Pointers.Pointer;
   function "=" (Left, Right : chars_ptr) return Boolean
      renames Pointers."=";

   type chars_ptr_array is array (size_t range <>) of aliased chars_ptr;

   --  extended
   subtype const_chars_ptr is Pointers.Constant_Pointer;
   function "=" (Left, Right : const_chars_ptr) return Boolean
      renames Pointers."=";
   type const_chars_ptr_array is
      array (size_t range <>) of aliased const_chars_ptr;
   type const_chars_ptr_With_Length is record
      ptr : const_chars_ptr;
      Length : size_t;
   end record;
   type const_chars_ptr_With_Length_array is
      array (size_t range <>) of aliased const_chars_ptr_With_Length;

   Null_Ptr : constant chars_ptr := null;

--  function To_Chars_Ptr (
--    Item : char_array_access;
--    Nul_Check : Boolean := False)
--    return chars_ptr;
   function To_Chars_Ptr (
      Item : access Element_Array; -- CXB3009 requires null
      Nul_Check : Boolean := False)
      return chars_ptr;
   pragma Pure_Function (To_Chars_Ptr);
   pragma Inline (To_Chars_Ptr);

   --  extended
   function To_Const_Chars_Ptr (Item : not null access constant Element_Array)
      return not null const_chars_ptr;
   pragma Pure_Function (To_Const_Chars_Ptr);
   pragma Inline (To_Const_Chars_Ptr);

--  function New_Char_Array (Chars : char_array) return chars_ptr;
   function New_Char_Array (Chars : Element_Array) return not null chars_ptr;

--  function New_String (Str : String) return chars_ptr;
   function New_String (
      Str : String_Type;
      Substitute : Element_Array := (0 => Element'Val (Character'Pos ('?'))))
      return not null chars_ptr;

   --  extended
   function New_Chars_Ptr (Length : size_t) return not null chars_ptr;
   function New_Chars_Ptr (
      Item : not null access constant Element;
      Length : size_t)
      return not null chars_ptr;
   function New_Chars_Ptr (Item : not null access constant Element)
      return not null chars_ptr;

   --  extended
   function New_Strcat (Items : const_chars_ptr_array)
      return not null chars_ptr;
   function New_Strcat (Items : const_chars_ptr_With_Length_array)
      return not null chars_ptr;

   procedure Free (Item : in out chars_ptr);

   Dereference_Error : exception
      renames C.Dereference_Error;

--  function Value (Item : chars_ptr) return char_array;
   function Value (Item : access constant Element) -- CXB3010 requires null
      return Element_Array;

--  function Value (Item : chars_ptr; Length : size_t)
--    return char_array;
   function Value (
      Item : access constant Element;
      Length : size_t;
      Append_Nul : Boolean := False) -- additional
      return Element_Array;

   --  Note: Value (no Append_Nul, default) produces an unterminated result
   --    if there is no nul in the first Length elements of Item.
   --  This behavior is danger similar to strncpy.
   --  Insert the parameter Append_Nul => True.

--  function Value (Item : chars_ptr) return String;
   function Value (
      Item : access constant Element; -- CXB3011 requires null
      Substitute : String_Type :=
         (1 => Character_Type'Val (Character'Pos ('?'))))
      return String_Type;

--  function Value (Item : chars_ptr; Length : size_t) return String;
   function Value (
      Item : access constant Element;
      Length : size_t;
      Substitute : String_Type :=
         (1 => Character_Type'Val (Character'Pos ('?'))))
      return String_Type;

--  function Strlen (Item : chars_ptr) return size_t;
   function Strlen (Item : access constant Element) -- CXB3011 requires null
      return size_t;

   --  extended
   --  This overloaded version Strlen gets the length of Item
   --    less than or equal to Limit.
   function Strlen (Item : not null access constant Element; Limit : size_t)
      return size_t;

--  procedure Update (
--    Item : chars_ptr;
--    Offset : size_t;
--    Chars : char_array;
--    Check : Boolean := True);
   procedure Update (
      Item : access Element; -- CXB3012 requires null
      Offset : size_t;
      Chars : Element_Array;
      Check : Boolean := True);

--  procedure Update (
--    Item : chars_ptr;
--    Offset : size_t;
--    Str : String;
--    Check : Boolean := True);
   procedure Update (
      Item : access Element; -- CXB3012 requires null
      Offset : size_t;
      Str : String_Type;
      Check : Boolean := True;
      Substitute : Element_Array :=
         (0 => Element'Val (Character'Pos ('?'))));

   --  Note: Update (.., Str) is danger in drake,
   --    because Str may be encoded and its length could be changed.

   --  extended
   procedure Update (
      Item : not null access Element;
      Offset : size_t;
      Source : not null access constant Element;
      Length : size_t);

   Update_Error : exception
      renames C.Update_Error;

end Interfaces.C.Generic_Strings;
