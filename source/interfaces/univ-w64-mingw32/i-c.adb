with Ada.Exceptions;
pragma Warnings (Off, Ada.Exceptions); -- break "pure" rule
with Ada.Unchecked_Conversion;
with System;
with C.winnls;
pragma Warnings (Off, C.winnls); -- break "pure" rule
with C.winnt;
pragma Warnings (Off, C.winnt); -- break "pure" rule
package body Interfaces.C is
   pragma Suppress (All_Checks);
   use type Standard.C.size_t;

   generic
      type Element is private;
      type Element_Array is array (size_t range <>) of aliased Element;
      type Element_Access is access constant Element;
   function Pointer_Add (Left : not null Element_Access; Right : ptrdiff_t)
      return not null Element_Access;

   function Pointer_Add (Left : not null Element_Access; Right : ptrdiff_t)
      return not null Element_Access
   is
      function To_ptrdiff_t is
         new Ada.Unchecked_Conversion (Element_Access, ptrdiff_t);
      function To_Pointer is
         new Ada.Unchecked_Conversion (ptrdiff_t, Element_Access);
   begin
      return To_Pointer (
         To_ptrdiff_t (Left)
         + Right * (Element_Array'Component_Size / Standard'Storage_Unit));
   end Pointer_Add;

   generic
      type Element is private;
      type Element_Array is array (size_t range <>) of aliased Element;
      type Element_Access is access constant Element;
   function Pointer_Sub (Left, Right : not null Element_Access)
      return ptrdiff_t;

   function Pointer_Sub (Left, Right : not null Element_Access)
      return ptrdiff_t
   is
      function To_ptrdiff_t is
         new Ada.Unchecked_Conversion (Element_Access, ptrdiff_t);
   begin
      return (To_ptrdiff_t (Left) - To_ptrdiff_t (Right))
         / (Element_Array'Component_Size / Standard'Storage_Unit);
   end Pointer_Sub;

   generic
      type Character_Type is (<>);
      type String_Type is array (Positive range <>) of Character_Type;
      type Element is (<>);
      type Element_Array is array (size_t range <>) of aliased Element;
      type Element_ptr is access constant Element;
      with function Find_nul (s : not null Element_ptr; n : size_t)
         return Element_ptr;
   package Simple_Conversions is

      pragma Compile_Time_Error (
         String_Type'Component_Size /= Element_Array'Component_Size,
         "size mismatch!");

      function Is_Nul_Terminated (Item : Element_Array) return Boolean;

      function Length (Item : Element_Array) return size_t;

      function To_Nul_Terminated (Item : String_Type)
         return Element_Array;
      function To_Non_Nul_Terminated (Item : String_Type)
         return Element_Array;

      function From_Nul_Terminated (Item : Element_Array)
         return String_Type;
      function From_Non_Nul_Terminated (Item : Element_Array)
         return String_Type;

      procedure To_Nul_Terminated (
         Item : String_Type;
         Target : out Element_Array;
         Count : out size_t);
      procedure To_Non_Nul_Terminated (
         Item : String_Type;
         Target : out Element_Array;
         Count : out size_t);

      procedure From_Nul_Terminated (
         Item : Element_Array;
         Target : out String_Type;
         Count : out Natural);
      procedure From_Non_Nul_Terminated (
         Item : Element_Array;
         Target : out String_Type;
         Count : out Natural);

   end Simple_Conversions;

   package body Simple_Conversions is

      function Is_Nul_Terminated (Item : Element_Array) return Boolean is
         nul_Pos : constant Element_ptr :=
            Find_nul (
               Item (Item'First)'Unchecked_Access,
               Item'Length);
      begin
         return nul_Pos /= null;
      end Is_Nul_Terminated;

      function Length (Item : Element_Array) return size_t is
         function "-" is new Pointer_Sub (Element, Element_Array, Element_ptr);
         Item_ptr : constant Element_ptr := Item (Item'First)'Unchecked_Access;
         nul_Pos : constant Element_ptr := Find_nul (Item_ptr, Item'Length);
      begin
         if nul_Pos = null then
            Ada.Exceptions.Raise_Exception_From_Here (
               Terminator_Error'Identity); -- CXB3005
         end if;
         return size_t (nul_Pos - Item_ptr);
      end Length;

      function To_Nul_Terminated (Item : String_Type)
         return Element_Array
      is
         C_Item : Element_Array (0 .. Item'Length - 1);
         for C_Item'Address use Item'Address;
      begin
         return C_Item & Element'Val (0);
      end To_Nul_Terminated;

      function To_Non_Nul_Terminated (Item : String_Type)
         return Element_Array
      is
         C_Item : Element_Array (0 .. Item'Length - 1);
         for C_Item'Address use Item'Address;
      begin
         return C_Item;
      end To_Non_Nul_Terminated;

      function From_Nul_Terminated (Item : Element_Array)
         return String_Type
      is
         Ada_Item : String_Type (1 .. Item'Length);
         for Ada_Item'Address use Item'Address;
      begin
         return Ada_Item (1 .. Natural (Length (Item)));
      end From_Nul_Terminated;

      function From_Non_Nul_Terminated (Item : Element_Array)
         return String_Type
      is
         Ada_Item : String_Type (1 .. Item'Length);
         for Ada_Item'Address use Item'Address;
      begin
         return Ada_Item;
      end From_Non_Nul_Terminated;

      procedure To_Nul_Terminated (
         Item : String_Type;
         Target : out Element_Array;
         Count : out size_t)
      is
         C_Item : Element_Array (0 .. Item'Length - 1);
         for C_Item'Address use Item'Address;
      begin
         Count := C_Item'Length + 1;
         if Count > Target'Length then
            raise Constraint_Error;
         end if;
         Target (Target'First .. Target'First + C_Item'Length - 1) := C_Item;
         Target (Target'First + C_Item'Length) := Element'Val (0);
      end To_Nul_Terminated;

      procedure To_Non_Nul_Terminated (
         Item : String_Type;
         Target : out Element_Array;
         Count : out size_t)
      is
         C_Item : Element_Array (0 .. Item'Length - 1);
         for C_Item'Address use Item'Address;
      begin
         Count := C_Item'Length;
         if Count > Target'Length then
            raise Constraint_Error;
         end if;
         Target (Target'First .. Target'First + C_Item'Length - 1) := C_Item;
      end To_Non_Nul_Terminated;

      procedure From_Nul_Terminated (
         Item : Element_Array;
         Target : out String_Type;
         Count : out Natural)
      is
         Ada_Item : String_Type (1 .. Item'Length);
         for Ada_Item'Address use Item'Address;
      begin
         Count := Natural (Length (Item));
         if Count > Target'Length then
            raise Constraint_Error;
         end if;
         Target (Target'First .. Target'First + Count - 1) :=
            Ada_Item (1 .. Count);
      end From_Nul_Terminated;

      procedure From_Non_Nul_Terminated (
         Item : Element_Array;
         Target : out String_Type;
         Count : out Natural)
      is
         Ada_Item : String_Type (1 .. Item'Length);
         for Ada_Item'Address use Item'Address;
      begin
         Count := Item'Length;
         if Count > Target'Length then
            raise Constraint_Error;
         end if;
         Target (Target'First .. Target'First + Count - 1) :=
            Ada_Item (1 .. Count);
      end From_Non_Nul_Terminated;

   end Simple_Conversions;

   --  char

   type char_const_ptr is access constant char;
   for char_const_ptr'Storage_Size use 0;

   function Find_nul (s : not null char_const_ptr; n : size_t)
      return char_const_ptr;
   function Find_nul (s : not null char_const_ptr; n : size_t)
      return char_const_ptr
   is
      function memchr (
         s : not null char_const_ptr;
         c : int;
         n : size_t)
         return char_const_ptr;
      pragma Import (Intrinsic, memchr, "__builtin_memchr");
   begin
      return memchr (s, 0, n);
   end Find_nul;

   package char_Conv is
      new Simple_Conversions (
         Character,
         String,
         char,
         char_array,
         char_const_ptr,
         Find_nul);

   procedure To_Non_Nul_Terminated (
      Item : String;
      Target : out char_array;
      Count : out size_t;
      Substitute : char);
   procedure To_Non_Nul_Terminated (
      Item : String;
      Target : out char_array;
      Count : out size_t;
      Substitute : char) is
   begin
      if Item'Length = 0 then
         Count := 0;
      else
         declare
            function To_LPSTR is
               new Ada.Unchecked_Conversion (
                  System.Address,
                  Standard.C.winnt.LPSTR);
            W_Item : Standard.C.winnt.WCHAR_array (0 .. Item'Length - 1);
            W_Length : size_t;
            Sub : aliased constant Standard.C.char_array (0 .. 1) := (
               Standard.C.char'Val (char'Pos (Substitute)),
               Standard.C.char'Val (0));
         begin
            W_Length := size_t (Standard.C.winnls.MultiByteToWideChar (
               Standard.C.winnls.CP_UTF8,
               0,
               To_LPSTR (Item'Address),
               Item'Length,
               W_Item (0)'Access,
               W_Item'Length));
            if W_Length = 0 then
               raise Constraint_Error;
            end if;
            Count := size_t (Standard.C.winnls.WideCharToMultiByte (
               Standard.C.winnls.CP_ACP,
               0,
               W_Item (0)'Access,
               Standard.C.signed_int (W_Length),
               To_LPSTR (Target'Address),
               Target'Length,
               Sub (0)'Access,
               null));
            if Count = 0 then
               raise Constraint_Error;
            end if;
         end;
      end if;
   end To_Non_Nul_Terminated;

   procedure From_Non_Nul_Terminated (
      Item : char_array;
      Target : out String;
      Count : out Natural);
   procedure From_Non_Nul_Terminated (
      Item : char_array;
      Target : out String;
      Count : out Natural) is
   begin
      if Item'Length = 0 then
         Count := 0;
      else
         declare
            function To_LPSTR is
               new Ada.Unchecked_Conversion (
                  System.Address,
                  Standard.C.winnt.LPSTR);
            W_Item : Standard.C.winnt.WCHAR_array (0 .. Item'Length - 1);
            W_Length : size_t;
         begin
            W_Length := size_t (Standard.C.winnls.MultiByteToWideChar (
               Standard.C.winnls.CP_ACP,
               0,
               To_LPSTR (Item'Address),
               Item'Length,
               W_Item (0)'Access,
               W_Item'Length));
            if W_Length = 0 then
               raise Constraint_Error;
            end if;
            Count := Natural (Standard.C.winnls.WideCharToMultiByte (
               Standard.C.winnls.CP_UTF8,
               0,
               W_Item (0)'Access,
               Standard.C.signed_int (W_Length),
               To_LPSTR (Target'Address),
               Target'Length,
               null,
               null));
            if Count = 0 then
               raise Constraint_Error;
            end if;
         end;
      end if;
   end From_Non_Nul_Terminated;

   --  wchar_t

   type wchar_t_const_ptr is access constant wchar_t;
   for wchar_t_const_ptr'Storage_Size use 0;

   function Find_nul (s : not null wchar_t_const_ptr; n : size_t)
      return wchar_t_const_ptr;
   function Find_nul (s : not null wchar_t_const_ptr; n : size_t)
      return wchar_t_const_ptr
   is
      function wmemchr (
         ws : not null wchar_t_const_ptr;
         wc : int;
         n : size_t)
         return wchar_t_const_ptr;
      pragma Import (C, wmemchr);
   begin
      return wmemchr (s, 0, n);
   end Find_nul;

   package wchar_Conv is
      new Simple_Conversions (
         wchar_Character,
         wchar_String,
         wchar_t,
         wchar_array,
         wchar_t_const_ptr,
         Find_nul);

   --  char16_t

   type char16_t_const_ptr is access constant char16_t;
   for char16_t_const_ptr'Storage_Size use 0;

   function Find_nul (s : not null char16_t_const_ptr; n : size_t)
      return char16_t_const_ptr;
   function Find_nul (s : not null char16_t_const_ptr; n : size_t)
      return char16_t_const_ptr
   is
      function "+" is
         new  Pointer_Add (char16_t, char16_array, char16_t_const_ptr);
      p : not null char16_t_const_ptr := s;
      r : size_t := n;
   begin
      while r > 0 loop
         if p.all = char16_nul then
            return p;
         end if;
         p := p + 1;
         r := r - 1;
      end loop;
      return null;
   end Find_nul;

   package char16_Conv is
      new Simple_Conversions (
         Wide_Character,
         Wide_String,
         char16_t,
         char16_array,
         char16_t_const_ptr,
         Find_nul);

   --  char32_t

   type char32_t_const_ptr is access constant char32_t;
   for char32_t_const_ptr'Storage_Size use 0;

   function Find_nul (s : not null char32_t_const_ptr; n : size_t)
      return char32_t_const_ptr;
   function Find_nul (s : not null char32_t_const_ptr; n : size_t)
      return char32_t_const_ptr
   is
      function "+" is
         new  Pointer_Add (char32_t, char32_array, char32_t_const_ptr);
      p : not null char32_t_const_ptr := s;
      r : size_t := n;
   begin
      while r > 0 loop
         if p.all = char32_nul then
            return p;
         end if;
         p := p + 1;
         r := r - 1;
      end loop;
      return null;
   end Find_nul;

   package char32_Conv is
      new Simple_Conversions (
         Wide_Wide_Character,
         Wide_Wide_String,
         char32_t,
         char32_array,
         char32_t_const_ptr,
         Find_nul);

   --  implementation of Characters and Strings

   --  Character (UTF-8) from/to char (MBCS)

   function To_C (Item : Character) return char is
   begin
      if Character'Pos (Item) > 16#7f# then
         raise Constraint_Error;
      end if;
      return char (Item);
   end To_C;

   function To_Ada (Item : char) return Character is
   begin
      if char'Pos (Item) > 16#7f# then
         raise Constraint_Error;
      end if;
      return Character (Item);
   end To_Ada;

   function Is_Nul_Terminated (Item : char_array) return Boolean
      renames char_Conv.Is_Nul_Terminated;

   function Length (Item : char_array) return size_t
      renames char_Conv.Length;

   function To_C (Item : String; Append_Nul : Boolean := True)
      return char_array is
   begin
      return To_C (Item, Append_Nul => Append_Nul, Substitute => '?');
   end To_C;

   function To_C (Item : String; Append_Nul : Boolean; Substitute : char)
      return char_array
   is
      Result : char_array (0 .. Item'Length * 2); -- +1 for nul
      Count : size_t;
   begin
      To_Non_Nul_Terminated (Item, Result, Count, Substitute);
      if Append_Nul then
         Result (Count) := nul;
         Count := Count + 1;
      end if;
      return Result (0 .. Count - 1);
   end To_C;

   function To_Ada (Item : char_array; Trim_Nul : Boolean := True)
      return String
   is
      Item_Last : size_t;
   begin
      if Trim_Nul then
         Item_Last := Item'First + Length (Item) - 1;
      else
         Item_Last := Item'Last;
      end if;
      declare
         Result : String (1 .. Item'Length * 3); -- Expanding_From_16_To_8
         Count : Natural;
      begin
         From_Non_Nul_Terminated (
            Item (Item'First .. Item_Last),
            Result,
            Count);
         return Result (1 .. Count);
      end;
   end To_Ada;

   procedure To_C (
      Item : String;
      Target : out char_array;
      Count : out size_t;
      Append_Nul : Boolean := True;
      Substitute : char := '?') is
   begin
      To_Non_Nul_Terminated (Item, Target, Count, Substitute);
      if Append_Nul then
         Count := Count + 1;
         if Count > Target'Length then
            raise Constraint_Error;
         end if;
         Target (Target'First + Count - 1) := nul;
      end if;
   end To_C;

   procedure To_Ada (
      Item : char_array;
      Target : out String;
      Count : out Natural;
      Trim_Nul : Boolean := True)
   is
      Item_Last : size_t;
   begin
      if Trim_Nul then
         Item_Last := Item'First + Length (Item) - 1;
      else
         Item_Last := Item'Last;
      end if;
      From_Non_Nul_Terminated (
         Item (Item'First .. Item_Last),
         Target,
         Count);
   end To_Ada;

   --  implementation of Wide Character and Wide String

   --  Wide_Character (UTF-16) from/to wchar_t (UTF-16)

   function To_C (Item : Wide_Character) return wchar_t is
   begin
      return wchar_t'Val (Wide_Character'Pos (Item));
   end To_C;

   function To_Ada (Item : wchar_t) return Wide_Character is
   begin
      return Wide_Character'Val (wchar_t'Pos (Item));
   end To_Ada;

   function Is_Nul_Terminated (Item : wchar_array) return Boolean
      renames wchar_Conv.Is_Nul_Terminated;

   function Length (Item : wchar_array) return size_t
      renames wchar_Conv.Length;

   function To_C (Item : Wide_String; Append_Nul : Boolean := True)
      return wchar_array is
   begin
      if Append_Nul then
         return wchar_Conv.To_Nul_Terminated (Item);
      else
         return wchar_Conv.To_Non_Nul_Terminated (Item);
      end if;
   end To_C;

   function To_Ada (Item : wchar_array; Trim_Nul : Boolean := True)
      return Wide_String is
   begin
      if Trim_Nul then
         return wchar_Conv.From_Nul_Terminated (Item);
      else
         return wchar_Conv.From_Non_Nul_Terminated (Item);
      end if;
   end To_Ada;

   procedure To_C (
      Item : Wide_String;
      Target : out wchar_array;
      Count : out size_t;
      Append_Nul : Boolean := True) is
   begin
      if Append_Nul then
         wchar_Conv.To_Nul_Terminated (Item, Target, Count);
      else
         wchar_Conv.To_Non_Nul_Terminated (Item, Target, Count);
      end if;
   end To_C;

   procedure To_Ada (
      Item : wchar_array;
      Target : out Wide_String;
      Count : out Natural;
      Trim_Nul : Boolean := True) is
   begin
      if Trim_Nul then
         wchar_Conv.From_Nul_Terminated (Item, Target, Count);
      else
         wchar_Conv.From_Non_Nul_Terminated (Item, Target, Count);
      end if;
   end To_Ada;

   --  implementation of
   --    ISO/IEC 10646:2003 compatible types defined by ISO/IEC TR 19769:2004.

   --  Wide_Character (UTF-16) from/to char16_t (UTF-16)

   function To_C (Item : Wide_Character) return char16_t is
   begin
      return char16_t (Item);
   end To_C;

   function To_Ada (Item : char16_t) return Wide_Character is
   begin
      return Wide_Character (Item);
   end To_Ada;

   function Is_Nul_Terminated (Item : char16_array) return Boolean
      renames char16_Conv.Is_Nul_Terminated;

   function To_C (Item : Wide_String; Append_Nul : Boolean := True)
      return char16_array is
   begin
      if Append_Nul then
         return char16_Conv.To_Nul_Terminated (Item);
      else
         return char16_Conv.To_Non_Nul_Terminated (Item);
      end if;
   end To_C;

   function To_Ada (Item : char16_array; Trim_Nul : Boolean := True)
      return Wide_String is
   begin
      if Trim_Nul then
         return char16_Conv.From_Nul_Terminated (Item);
      else
         return char16_Conv.From_Non_Nul_Terminated (Item);
      end if;
   end To_Ada;

   procedure To_C (
      Item : Wide_String;
      Target : out char16_array;
      Count : out size_t;
      Append_Nul : Boolean := True) is
   begin
      if Append_Nul then
         char16_Conv.To_Nul_Terminated (Item, Target, Count);
      else
         char16_Conv.To_Non_Nul_Terminated (Item, Target, Count);
      end if;
   end To_C;

   procedure To_Ada (
      Item : char16_array;
      Target : out Wide_String;
      Count : out Natural;
      Trim_Nul : Boolean := True) is
   begin
      if Trim_Nul then
         char16_Conv.From_Nul_Terminated (Item, Target, Count);
      else
         char16_Conv.From_Non_Nul_Terminated (Item, Target, Count);
      end if;
   end To_Ada;

   --  Wide_Wide_Character (UTF-32) from/to char32_t (UTF-32)

   function To_C (Item : Wide_Wide_Character) return char32_t is
   begin
      return char32_t (Item);
   end To_C;

   function To_Ada (Item : char32_t) return Wide_Wide_Character is
   begin
      return Wide_Wide_Character (Item);
   end To_Ada;

   function Is_Nul_Terminated (Item : char32_array) return Boolean
      renames char32_Conv.Is_Nul_Terminated;

   function To_C (Item : Wide_Wide_String; Append_Nul : Boolean := True)
      return char32_array is
   begin
      if Append_Nul then
         return char32_Conv.To_Nul_Terminated (Item);
      else
         return char32_Conv.To_Non_Nul_Terminated (Item);
      end if;
   end To_C;

   function To_Ada (Item : char32_array; Trim_Nul : Boolean := True)
      return Wide_Wide_String is
   begin
      if Trim_Nul then
         return char32_Conv.From_Nul_Terminated (Item);
      else
         return char32_Conv.From_Non_Nul_Terminated (Item);
      end if;
   end To_Ada;

   procedure To_C (
      Item : Wide_Wide_String;
      Target : out char32_array;
      Count : out size_t;
      Append_Nul : Boolean := True) is
   begin
      if Append_Nul then
         char32_Conv.To_Nul_Terminated (Item, Target, Count);
      else
         char32_Conv.To_Non_Nul_Terminated (Item, Target, Count);
      end if;
   end To_C;

   procedure To_Ada (
      Item : char32_array;
      Target : out Wide_Wide_String;
      Count : out Natural;
      Trim_Nul : Boolean := True) is
   begin
      if Trim_Nul then
         char32_Conv.From_Nul_Terminated (Item, Target, Count);
      else
         char32_Conv.From_Non_Nul_Terminated (Item, Target, Count);
      end if;
   end To_Ada;

end Interfaces.C;
