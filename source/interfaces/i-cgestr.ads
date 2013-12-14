pragma License (Unrestricted);
--  generic implementation of Interfaces.C.Strings
with Ada.References;
with Interfaces.C.Pointers;
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
   type Element is new Character_Type;
   type Element_Array is array (size_t range <>) of aliased Element;
   with package Pointers is new Interfaces.C.Pointers (
      Index => size_t,
      Element => Element,
      Element_Array => Element_Array,
      Default_Terminator => Element'Val (0));
   with package Slicing is new Ada.References.Generic_Slicing (
      Positive,
      Character_Type,
      String_Type);
package Interfaces.C.Generic_Strings is
   pragma Preelaborate;

--  type char_array_access is access all char_array;
   type char_array_access is access all Element_Array;
   for char_array_access'Storage_Size use 0; -- additional

--  type chars_ptr is private;
--  pragma Preelaborable_Initialization (chars_ptr);
   subtype chars_ptr is Pointers.Pointer;
   function "=" (Left, Right : chars_ptr) return Boolean
      renames Pointers."=";

   type chars_ptr_array is array (size_t range <>) of aliased chars_ptr;

   Null_Ptr : constant chars_ptr := null;

--  function To_Chars_Ptr (
--    Item : char_array_access;
--    Nul_Check : Boolean := False)
--    return chars_ptr;
   function To_Chars_Ptr (
      Item : not null access Element_Array;
      Nul_Check : Boolean := False)
      return not null chars_ptr;
   pragma Pure_Function (To_Chars_Ptr);
   pragma Inline (To_Chars_Ptr);

--  function New_Char_Array (Chars : char_array) return chars_ptr;
   function New_Char_Array (Chars : Element_Array) return not null chars_ptr;

--  function New_String (Str : String) return chars_ptr;
   function New_String (Str : String_Type) return not null chars_ptr;

   procedure Free (Item : in out chars_ptr);

--  Dereference_Error : exception;

--  function Value (Item : chars_ptr) return char_array;
   function Value (Item : not null access constant Element)
      return Element_Array;

--  function Value (Item : chars_ptr; Length : size_t)
--    return char_array;
   function Value (Item : access constant Element; Length : size_t)
      return Element_Array;

--  function Value (Item : chars_ptr) return String;
   function Value (Item : not null access constant Element)
      return String_Type;

--  function Value (Item : chars_ptr; Length : size_t) return String;
   function Value (Item : access constant Element; Length : size_t)
      return String_Type;

--  function Strlen (Item : chars_ptr) return size_t;
   function Strlen (Item : not null access constant Element)
      return size_t;

--  procedure Update (
--    Item : chars_ptr;
--    Offset : size_t;
--    Chars : char_array;
--    Check : Boolean := True);
   procedure Update (
      Item : not null access Element;
      Offset : size_t;
      Chars : Element_Array;
      Check : Boolean := True);

--  procedure Update (
--    Item : chars_ptr;
--    Offset : size_t;
--    Str : String;
--    Check : Boolean := True);
   procedure Update (
      Item : not null access Element;
      Offset : size_t;
      Str : String_Type;
      Check : Boolean := True);

--  Update_Error : exception;

   --  extended from here

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

   function To_Chars_Ptr (Item : not null access String_Type)
      return not null chars_ptr;
   pragma Pure_Function (To_Chars_Ptr);
   pragma Inline (To_Chars_Ptr);

   function To_Const_Chars_Ptr (Item : not null access constant Element_Array)
      return not null const_chars_ptr;
   pragma Pure_Function (To_Const_Chars_Ptr);
   pragma Inline (To_Const_Chars_Ptr);

   function To_Const_Chars_Ptr (Item : not null access constant String_Type)
      return not null const_chars_ptr;
   pragma Pure_Function (To_Const_Chars_Ptr);
   pragma Inline (To_Const_Chars_Ptr);

   function New_Chars_Ptr (Length : size_t) return not null chars_ptr;

   function New_Chars_Ptr (
      Item : not null access constant Element;
      Length : size_t)
      return not null chars_ptr;

   function New_Chars_Ptr (Item : not null access constant Element)
      return not null chars_ptr;

   function New_Strcat (Items : const_chars_ptr_array)
      return not null chars_ptr;

   function New_Strcat (Items : const_chars_ptr_With_Length_array)
      return not null chars_ptr;

   procedure Update (
      Item : not null access Element;
      Offset : size_t;
      Source : not null access constant Element;
      Length : size_t);

   procedure Update (
      Item : not null access Element;
      Offset : size_t;
      Source : not null access constant Element);

   function Reference (
      Item : not null access Element;
      Length : size_t)
      return Slicing.Reference_Type;

   function Constant_Reference (
      Item : not null access constant Element;
      Length : size_t)
      return Slicing.Constant_Reference_Type;

   subtype char_t is Element;
   subtype char_array_t is Element_Array;
   subtype char_t_Character is Character_Type;
   subtype char_t_String is String_Type;

end Interfaces.C.Generic_Strings;
