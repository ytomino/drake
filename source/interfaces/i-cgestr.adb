with System.Address_To_Named_Access_Conversions;
with System.Address_To_Constant_Access_Conversions;
with System.Storage_Elements;
package body Interfaces.C.Generic_Strings is
   pragma Suppress (All_Checks);
   use type System.Storage_Elements.Storage_Offset;

   package libc is

      function strlen (Item : not null access constant Element)
         return size_t;
      pragma Import (Intrinsic, strlen, "__builtin_strlen");

      function wcslen (Item : not null access constant Element)
         return size_t;
      pragma Import (C, wcslen);

      procedure memcpy (
         s1 : not null access Element;
         s2 : not null access constant Element;
         n : size_t);
      pragma Import (Intrinsic, memcpy, "__builtin_memcpy");

      procedure memmove (
         s1 : not null access Element;
         s2 : not null access constant Element;
         n : size_t);
      pragma Import (Intrinsic, memmove, "__builtin_memmove");

      function malloc (size : size_t) return chars_ptr;
      pragma Import (Intrinsic, malloc, "__builtin_malloc");

      procedure free (ptr : chars_ptr);
      pragma Import (Intrinsic, free, "__builtin_free");

   end libc;

   package Conv is new System.Address_To_Named_Access_Conversions (
      Element,
      chars_ptr);
   package const_Conv is new System.Address_To_Constant_Access_Conversions (
      Element,
      const_chars_ptr);

   --  implementation

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
      return Conv.To_Pointer (Item.all'Address);
   end To_Chars_Ptr;

   function To_Const_Chars_Ptr (Item : not null access constant Element_Array)
      return not null const_chars_ptr is
   begin
      return Item.all (Item.all'First)'Access;
   end To_Const_Chars_Ptr;

   function To_Const_Chars_Ptr (Item : not null access constant String_Type)
      return not null const_chars_ptr is
   begin
      return const_Conv.To_Pointer (Item.all'Address);
   end To_Const_Chars_Ptr;

   function New_Char_Array (Chars : Element_Array)
      return not null chars_ptr
   is
      Source : constant const_chars_ptr :=
         const_Conv.To_Pointer (Chars'Address);
      Length : constant size_t := size_t'Min (Chars'Length, Strlen (Source));
   begin
      return New_Chars_Ptr (Source, Length);
   end New_Char_Array;

   function New_String (Str : String_Type) return not null chars_ptr is
   begin
      return New_Chars_Ptr (Conv.To_Pointer (Str'Address), Str'Length);
   end New_String;

   function New_Chars_Ptr (Length : size_t) return not null chars_ptr is
      Size : constant System.Storage_Elements.Storage_Count :=
         System.Storage_Elements.Storage_Count (Length)
            * (Element'Size / Standard'Storage_Unit);
      Result : constant chars_ptr := libc.malloc (
         C.size_t (Size + Element'Size / Standard'Storage_Unit));
   begin
      if Result = null then
         raise Storage_Error;
      end if;
      Result.all := Element'Val (0);
      return Result;
   end New_Chars_Ptr;

   function New_Chars_Ptr (
      Item : not null access constant Element;
      Length : size_t)
      return not null chars_ptr
   is
      Result : constant chars_ptr := New_Chars_Ptr (Length);
      Size : constant System.Storage_Elements.Storage_Count :=
         System.Storage_Elements.Storage_Count (Length)
            * (Element'Size / Standard'Storage_Unit);
   begin
      libc.memcpy (Result, Item, C.size_t (Size));
      Conv.To_Pointer (Conv.To_Address (Result) + Size).all := Element'Val (0);
      return Result;
   end New_Chars_Ptr;

   function New_Chars_Ptr (Item : not null access constant Element)
      return not null chars_ptr is
   begin
      return New_Chars_Ptr (Item, Strlen (Item));
   end New_Chars_Ptr;

   function New_Strcat (Items : const_chars_ptr_array)
      return not null chars_ptr
   is
      Lengths : array (Items'Range) of size_t;
      Total_Length : size_t;
      Offset : size_t;
      Result : chars_ptr;
   begin
      --  get length
      Total_Length := 0;
      for I in Items'Range loop
         Lengths (I) := Strlen (Items (I));
         Total_Length := Total_Length + Lengths (I);
      end loop;
      --  allocate
      Result := New_Chars_Ptr (Total_Length);
      --  copy
      Offset := 0;
      for I in Items'Range loop
         Update (Result, Offset, Items (I), Lengths (I));
         Offset := Offset + Lengths (I);
      end loop;
      return Result;
   end New_Strcat;

   function New_Strcat (Items : const_chars_ptr_With_Length_array)
      return not null chars_ptr
   is
      Total_Length : size_t;
      Offset : size_t;
      Result : chars_ptr;
   begin
      --  get length
      Total_Length := 0;
      for I in Items'Range loop
         Total_Length := Total_Length + Items (I).Length;
      end loop;
      --  allocate
      Result := New_Chars_Ptr (Total_Length);
      --  copy
      Offset := 0;
      for I in Items'Range loop
         Update (Result, Offset, Items (I).ptr, Items (I).Length);
         Offset := Offset + Items (I).Length;
      end loop;
      return Result;
   end New_Strcat;

   procedure Free (Item : in out chars_ptr) is
   begin
      libc.free (Item);
      Item := null;
   end Free;

   function Value (Item : not null access constant Element)
      return Element_Array is
   begin
      return Value (Item, Strlen (Item));
   end Value;

   function Value (
      Item : access constant Element;
      Length : size_t)
      return Element_Array
   is
      Source : Element_Array (1 .. Length);
      for Source'Address use Item.all'Address;
   begin
      return Source;
   end Value;

   function Value (Item : not null access constant Element)
      return String_Type is
   begin
      return Value (Item, Strlen (Item));
   end Value;

   function Value (
      Item : access constant Element;
      Length : size_t)
      return String_Type
   is
      Source : String_Type (1 .. Natural (Length));
      for Source'Address use Item.all'Address;
   begin
      return Source;
   end Value;

   function Strlen (Item : not null access constant Element)
      return size_t is
   begin
      if Element'Size = char'Size then
         return libc.strlen (Item);
      elsif Element'Size = wchar_t'Size then
         return libc.wcslen (Item);
      else
         declare
            S : const_chars_ptr := const_chars_ptr (Item);
            Length : size_t := 0;
         begin
            while S.all /= Element'Val (0) loop
               Length := Length + 1;
               S := const_Conv.To_Pointer (
                  const_Conv.To_Address (S)
                  + Element'Size / Standard'Storage_Unit);
            end loop;
            return Length;
         end;
      end if;
   end Strlen;

   procedure Update (
      Item : not null access Element;
      Offset : size_t;
      Chars : Element_Array;
      Check : Boolean := True)
   is
      pragma Unreferenced (Check);
      Source : constant const_chars_ptr :=
         const_Conv.To_Pointer (Chars'Address);
   begin
      Update (
         Item,
         Offset,
         Source,
         size_t'Min (Chars'Length, Strlen (Source)));
   end Update;

   procedure Update (
      Item : not null access Element;
      Offset : size_t;
      Str : String_Type;
      Check : Boolean := True)
   is
      pragma Unreferenced (Check);
      Source : constant const_chars_ptr :=
         const_Conv.To_Pointer (Str'Address);
   begin
      Update (
         Item,
         Offset,
         Source,
         Str'Length);
   end Update;

   procedure Update (
      Item : not null access Element;
      Offset : size_t;
      Source : not null access constant Element;
      Length : size_t)
   is
      Offset_Size : constant System.Storage_Elements.Storage_Count :=
         System.Storage_Elements.Storage_Count (Offset)
            * (Element'Size / Standard'Storage_Unit);
      Offsetted_Item : constant chars_ptr :=
         Conv.To_Pointer (Conv.To_Address (chars_ptr (Item)) + Offset_Size);
      Size : constant System.Storage_Elements.Storage_Count :=
         System.Storage_Elements.Storage_Count (Length)
            * (Element'Size / Standard'Storage_Unit);
   begin
      libc.memmove (Offsetted_Item, Source, C.size_t (Size));
      Conv.To_Pointer (Conv.To_Address (Offsetted_Item) + Size).all :=
         Element'Val (0);
   end Update;

   procedure Update (
      Item : not null access Element;
      Offset : size_t;
      Source : not null access constant Element) is
   begin
      Update (
         Item,
         Offset,
         Source,
         Strlen (Source));
   end Update;

   function Reference (
      Item : not null access Element;
      Length : size_t)
      return Slicing.Reference_Type
   is
      Source : aliased String_Type (1 .. Natural (Length));
      for Source'Address use Item.all'Address;
   begin
      return Slicing.Slice (
         Source'Unrestricted_Access,
         Source'First,
         Source'Last);
   end Reference;

   function Constant_Reference (
      Item : not null access constant Element;
      Length : size_t)
      return Slicing.Constant_Reference_Type
   is
      Source : aliased String_Type (1 .. Natural (Length));
      for Source'Address use Item.all'Address;
   begin
      return Slicing.Constant_Slice (
         Source'Unrestricted_Access,
         Source'First,
         Source'Last);
   end Constant_Reference;

end Interfaces.C.Generic_Strings;
