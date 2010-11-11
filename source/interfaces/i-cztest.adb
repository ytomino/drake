with Ada.Unchecked_Conversion;
package body Interfaces.C.Zero_Terminated_Strings is
   pragma Suppress (All_Checks);

   package libc is

      function strlen (Item : not null access constant Character_Type)
         return size_t;
      pragma Import (Intrinsic, strlen, "__builtin_strlen");

      function wcslen (Item : not null access constant Character_Type)
         return size_t;
      pragma Import (C, wcslen);

      procedure memcpy (
         s1 : not null access Character_Type;
         s2 : not null access constant Character_Type;
         n : size_t);
      pragma Import (Intrinsic, memcpy, "__builtin_memcpy");

      procedure memmove (
         s1 : not null access Character_Type;
         s2 : not null access constant Character_Type;
         n : size_t);
      pragma Import (Intrinsic, memmove, "__builtin_memmove");

      function malloc (size : size_t) return chars_ptr;
      pragma Import (Intrinsic, malloc, "__builtin_malloc");

      procedure free (ptr : chars_ptr);
      pragma Import (Intrinsic, free, "__builtin_free");

   end libc;

   procedure Free (Item : in out chars_ptr) is
   begin
      libc.free (Item);
      Item := null;
   end Free;

   function New_Char_Array (Chars : Element_Array)
      return not null chars_ptr
   is
      Source : constant const_chars_ptr :=
         Chars (Chars'First)'Unchecked_Access;
      Length : constant size_t := size_t'Min (Chars'Length, Strlen (Source));
   begin
      return New_Chars_Ptr (Source, Length);
   end New_Char_Array;

   function New_Chars_Ptr (Item : not null access constant Character_Type)
      return not null chars_ptr is
   begin
      return New_Chars_Ptr (Item, Strlen (Item));
   end New_Chars_Ptr;

   function New_Chars_Ptr (
      Item : not null access constant Character_Type;
      Length : size_t)
      return not null chars_ptr
   is
      function N is new Ada.Unchecked_Conversion (chars_ptr, size_t);
      function P is new Ada.Unchecked_Conversion (size_t, chars_ptr);
      Size : constant size_t := Length *
         (Character_Type'Size / Standard'Storage_Unit);
      Result : constant chars_ptr := libc.malloc (Size + 1);
   begin
      if Result = null then
         raise Storage_Error;
      end if;
      libc.memmove (Result, Item, Size);
      P (N (Result) + Size).all := Character_Type'Val (0);
      return Result;
   end New_Chars_Ptr;

   function New_String (Str : String_Type) return not null chars_ptr is
   begin
      return New_Chars_Ptr (Str (Str'First)'Unrestricted_Access, Str'Length);
   end New_String;

   function Strlen (Item : not null access constant Character_Type)
      return size_t is
   begin
      if Character_Type'Size = char'Size then
         return libc.strlen (Item);
      elsif Character_Type'Size = wchar_t'Size then
         return libc.wcslen (Item);
      else
         declare
            function N is
               new Ada.Unchecked_Conversion (const_chars_ptr, size_t);
            function P is
               new Ada.Unchecked_Conversion (size_t, const_chars_ptr);
            S : const_chars_ptr := const_chars_ptr (Item);
            Length : size_t := 0;
         begin
            while S.all /= Character_Type'Val (0) loop
               Length := Length + 1;
               S := P (N (S) + Character_Type'Size / Standard'Storage_Unit);
            end loop;
            return Length;
         end;
      end if;
   end Strlen;

   function To_Chars_Ptr (
      Item : not null access Element_Array;
      Nul_Check : Boolean := False)
      return not null chars_ptr
   is
      pragma Unreferenced (Nul_Check);
   begin
      return Item.all (Item.all'First)'Access;
   end To_Chars_Ptr;

   function To_Chars_Ptr (Item : not null access String_Type)
      return not null chars_ptr is
   begin
      return Item.all (Item.all'First)'Unrestricted_Access;
   end To_Chars_Ptr;

   function To_Const_Chars_Ptr (Item : not null access constant String_Type)
      return not null const_chars_ptr is
   begin
      return Item.all (Item.all'First)'Unrestricted_Access;
   end To_Const_Chars_Ptr;

   procedure Update (
      Item : not null access Character_Type;
      Offset : size_t;
      Chars : Element_Array;
      Check : Boolean := True)
   is
      pragma Unreferenced (Check);
      function N is new Ada.Unchecked_Conversion (chars_ptr, size_t);
      function P is new Ada.Unchecked_Conversion (size_t, chars_ptr);
      Offset_Size : constant size_t := Offset *
         (Character_Type'Size / Standard'Storage_Unit);
   begin
      Update (
         P (N (chars_ptr (Item)) + Offset_Size),
         Chars (Chars'First)'Access,
         Strlen (Chars (Chars'First)'Access));
   end Update;

   procedure Update (
      Item : not null access Character_Type;
      Offset : size_t;
      Str : String_Type;
      Check : Boolean := True)
   is
      pragma Unreferenced (Check);
      function N is new Ada.Unchecked_Conversion (chars_ptr, size_t);
      function P is new Ada.Unchecked_Conversion (size_t, chars_ptr);
      Offset_Size : constant size_t := Offset *
         (Character_Type'Size / Standard'Storage_Unit);
   begin
      Update (
         P (N (chars_ptr (Item)) + Offset_Size),
         Str (Str'First)'Unrestricted_Access,
         Str'Length);
   end Update;

   procedure Update (
      Item : not null access Character_Type;
      Source : not null access constant Character_Type;
      Length : size_t)
   is
      function N is new Ada.Unchecked_Conversion (chars_ptr, size_t);
      function P is new Ada.Unchecked_Conversion (size_t, chars_ptr);
      Size : constant size_t := Length *
         (Character_Type'Size / Standard'Storage_Unit);
   begin
      libc.memcpy (Item, Source, Size);
      P (N (chars_ptr (Item)) + Size).all := Character_Type'Val (0);
   end Update;

   function Value (Item : not null access constant Character_Type)
      return Element_Array is
   begin
      return Value (Item, Strlen (Item));
   end Value;

   function Value (
      Item : access constant Character_Type;
      Length : size_t)
      return Element_Array
   is
      Source : Element_Array (1 .. Length);
      for Source'Address use Item.all'Address;
   begin
      return Source;
   end Value;

   function Value (Item : not null access constant Character_Type)
      return String_Type is
   begin
      return Value (Item, Strlen (Item));
   end Value;

   function Value (
      Item : access constant Character_Type;
      Length : size_t)
      return String_Type
   is
      Source : String_Type (1 .. Natural (Length));
      for Source'Address use Item.all'Address;
   begin
      return Source;
   end Value;

end Interfaces.C.Zero_Terminated_Strings;
