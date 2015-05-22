with System.Address_To_Named_Access_Conversions;
with System.Address_To_Constant_Access_Conversions;
with System.Storage_Elements;
package body Interfaces.C.Generic_Strings is
   pragma Suppress (All_Checks);
   use type System.Storage_Elements.Storage_Offset;

   package libc is

      function malloc (size : size_t) return chars_ptr
         with Import,
            Convention => Intrinsic, External_Name => "__builtin_malloc";

      procedure free (ptr : chars_ptr)
         with Import,
            Convention => Intrinsic, External_Name => "__builtin_free";

   end libc;

   package Conv is
      new System.Address_To_Named_Access_Conversions (Element, chars_ptr);
   package const_Conv is
      new System.Address_To_Constant_Access_Conversions (
         Element,
         const_chars_ptr);

   --  implementation

   function To_Chars_Ptr (
      Item : access Element_Array;
      Nul_Check : Boolean := False)
      return chars_ptr is
   begin
      if Item = null then
         return null;
      else
         if Nul_Check then
            --  raise Terminator_Error when Item contains no nul
            if Element'Size = char'Size
               and then Element_Array'Component_Size =
                  char_array'Component_Size
               --  'Scalar_Storage_Order is unrelated since searching 0
            then
               declare
                  ca_Item : char_array (Item'Range);
                  for ca_Item'Address use Item.all'Address;
                  Dummy : constant size_t := Length (ca_Item);
                  pragma Unreferenced (Dummy);
               begin
                  null;
               end;
            elsif Element'Size = wchar_t'Size
               and then Element_Array'Component_Size =
                  wchar_array'Component_Size
            then
               declare
                  wa_Item : wchar_array (Item'Range);
                  for wa_Item'Address use Item.all'Address;
                  Dummy : constant size_t := Length (wa_Item);
                  pragma Unreferenced (Dummy);
               begin
                  null;
               end;
            else
               declare
                  I : size_t := Item'First;
               begin
                  loop
                     if I <= Item'Last then
                        raise Terminator_Error;
                     end if;
                     exit when Item (I) = Element'Val (0);
                     I := I + 1;
                  end loop;
               end;
            end if;
         end if;
         return Item.all (Item.all'First)'Access;
      end if;
   end To_Chars_Ptr;

   function To_Const_Chars_Ptr (Item : not null access constant Element_Array)
      return not null const_chars_ptr is
   begin
      return Item.all (Item.all'First)'Access;
   end To_Const_Chars_Ptr;

   function New_Char_Array (Chars : Element_Array)
      return not null chars_ptr is
   begin
      return New_Chars_Ptr (
         const_Conv.To_Pointer (Chars'Address),
         Chars'Length); -- CXB3009, accept non-nul terminated
   end New_Char_Array;

   function New_String (
      Str : String_Type;
      Substitute : Element := Element'Val (Character'Pos ('?')))
      return not null chars_ptr
   is
      C : constant Element_Array :=
         To_C (Str, Append_Nul => False, Substitute => Substitute);
   begin
      return New_Chars_Ptr (C (C'First)'Access, C'Length);
   end New_String;

   function New_Chars_Ptr (Length : size_t) return not null chars_ptr is
      Size : constant System.Storage_Elements.Storage_Count :=
         (System.Storage_Elements.Storage_Count (Length) + 1) -- appending nul
         * (Element_Array'Component_Size / Standard'Storage_Unit);
      Result : constant chars_ptr := libc.malloc (C.size_t (Size));
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
         * (Element_Array'Component_Size / Standard'Storage_Unit);
   begin
      Pointers.Copy_Array (
         Source => Item,
         Target => Result,
         Length => ptrdiff_t (Length));
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

   function Value (Item : access constant Element)
      return Element_Array is
   begin
      if const_chars_ptr (Item) = null then
         raise Dereference_Error; -- CXB3010
      end if;
      declare
         Length : constant size_t := Strlen (Item);
      begin
         return Pointers.Value (
            Item,
            ptrdiff_t (Length + 1)); -- CXB3009, including nul
      end;
   end Value;

   function Value (
      Item : access constant Element;
      Length : size_t;
      Append_Nul : Boolean := False)
      return Element_Array
   is
      Actual_Length : size_t;
   begin
      if not Append_Nul and then Length = 0 then
         raise Constraint_Error; -- CXB3010
      end if;
      if const_chars_ptr (Item) = null then
         if Length > 0 then
            raise Dereference_Error; -- CXB3010
         end if;
         Actual_Length := 0;
      else
         if Element'Size = char'Size then
            declare
               function memchr (
                  s : not null access constant Element;
                  c : int;
                  n : size_t)
                  return const_chars_ptr
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_memchr";
               P : constant const_chars_ptr := memchr (Item, 0, Length);
            begin
               if P = null then
                  Actual_Length := Length;
               else
                  Actual_Length := C.size_t (
                     const_Conv.To_Address (P) - const_Conv.To_Address (Item));
               end if;
            end;
         elsif Element'Size = wchar_t'Size then
            declare
               function wmemchr (
                  ws : not null access constant Element;
                  wc : int;
                  n : size_t)
                  return const_chars_ptr
                  with Import, Convention => C;
               P : constant const_chars_ptr := wmemchr (Item, 0, Length);
            begin
               if P = null then
                  Actual_Length := Length;
               else
                  Actual_Length := C.size_t (
                     (const_Conv.To_Address (P) - const_Conv.To_Address (Item))
                     / (wchar_t'Size / Standard'Storage_Unit));
               end if;
            end;
         else
            declare
               Source : Element_Array (0 .. Length - 1);
               for Source'Address use Conv.To_Address (Item);
            begin
               Actual_Length := 0;
               while Actual_Length < Length loop
                  exit when Source (Actual_Length) = Element'Val (0);
                  Actual_Length := Actual_Length + 1;
               end loop;
            end;
         end if;
         Actual_Length := Actual_Length + 1; -- including nul
      end if;
      if Append_Nul and then Length < Actual_Length then
         declare
            Source : Element_Array (0 .. Actual_Length - 1);
            for Source'Address use Conv.To_Address (Item);
         begin
            return Source (0 .. Length - 1) & Element'Val (0);
         end;
      else
         return Pointers.Value (
            Item,
            ptrdiff_t (size_t'Min (Actual_Length, Length)));
         --  CXB3010, not appending nul
      end if;
   end Value;

   function Value (
      Item : access constant Element;
      Substitute : Character_Type := Character_Type'Val (Character'Pos ('?')))
      return String_Type
   is
      C : constant Element_Array := Value (Item);
   begin
      return To_Ada (
         C (C'First .. C'Last - 1),
         Trim_Nul => False,
         Substitute => Substitute);
   end Value;

   function Value (
      Item : access constant Element;
      Length : size_t;
      Substitute : Character_Type := Character_Type'Val (Character'Pos ('?')))
      return String_Type
   is
      C : constant Element_Array := Value (Item, Length, Append_Nul => True);
   begin
      return To_Ada (
         C (C'First .. C'Last - 1),
         Trim_Nul => False,
         Substitute => Substitute);
   end Value;

   function Strlen (Item : access constant Element)
      return size_t is
   begin
      if const_chars_ptr (Item) = null then
         raise Dereference_Error; -- CXB3011
      end if;
      if Element'Size = char'Size
         and then Element_Array'Component_Size = char_array'Component_Size
         --  'Scalar_Storage_Order is unrelated since searching 0
      then
         declare
            function strlen (Item : not null access constant Element)
               return size_t
               with Import,
                  Convention => Intrinsic, External_Name => "__builtin_strlen";
         begin
            return strlen (Item);
         end;
      elsif Element'Size = wchar_t'Size
         and then Element_Array'Component_Size = wchar_array'Component_Size
      then
         declare
            function wcslen (Item : not null access constant Element)
               return size_t
               with Import, Convention => C;
         begin
            return wcslen (Item);
         end;
      else
         declare
            S : const_chars_ptr := const_chars_ptr (Item);
            Length : size_t := 0;
         begin
            while S.all /= Element'Val (0) loop
               Length := Length + 1;
               S := const_Conv.To_Pointer (
                  const_Conv.To_Address (S)
                  + Element_Array'Component_Size / Standard'Storage_Unit);
            end loop;
            return Length;
         end;
      end if;
   end Strlen;

   procedure Update (
      Item : access Element;
      Offset : size_t;
      Chars : Element_Array;
      Check : Boolean := True) is
   begin
      if chars_ptr (Item) = null then
         raise Dereference_Error; -- CXB3011
      end if;
      if Check and then Offset + Chars'Length > Strlen (Item) then
         raise Update_Error;
      end if;
      Update (
         Item,
         Offset,
         const_Conv.To_Pointer (Chars'Address),
         Chars'Length);
   end Update;

   procedure Update (
      Item : access Element;
      Offset : size_t;
      Str : String_Type;
      Check : Boolean := True;
      Substitute : Element := Element'Val (Character'Pos ('?'))) is
   begin
      Update (
         Item,
         Offset,
         To_C (Str, Append_Nul => False, Substitute => Substitute),
         Check);
   end Update;

   procedure Update (
      Item : not null access Element;
      Offset : size_t;
      Source : not null access constant Element;
      Length : size_t)
   is
      Offset_Size : constant System.Storage_Elements.Storage_Count :=
         System.Storage_Elements.Storage_Count (Offset)
         * (Element_Array'Component_Size / Standard'Storage_Unit);
      Offsetted_Item : constant chars_ptr :=
         Conv.To_Pointer (Conv.To_Address (chars_ptr (Item)) + Offset_Size);
   begin
      Pointers.Copy_Array (
         Source => Source,
         Target => Offsetted_Item,
         Length => ptrdiff_t (Length));
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

end Interfaces.C.Generic_Strings;
