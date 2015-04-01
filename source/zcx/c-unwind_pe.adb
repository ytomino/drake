with Ada.Unchecked_Conversion;
with System.Address_To_Constant_Access_Conversions;
with System.Storage_Elements;
with C.stdint;
package body C.unwind_pe is
   pragma Suppress (All_Checks);
   use type System.Storage_Elements.Storage_Offset;

   procedure unreachable
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_unreachable";

   pragma No_Return (unreachable);

   package unsigned_char_const_ptr_Conv is
      new System.Address_To_Constant_Access_Conversions (
         C.unsigned_char,
         C.unsigned_char_const_ptr);

   function "+" (Left : C.unsigned_char_const_ptr; Right : C.ptrdiff_t)
      return C.unsigned_char_const_ptr;
   function "+" (Left : C.unsigned_char_const_ptr; Right : C.ptrdiff_t)
      return C.unsigned_char_const_ptr is
   begin
      return unsigned_char_const_ptr_Conv.To_Pointer (
         unsigned_char_const_ptr_Conv.To_Address (Left)
         + System.Storage_Elements.Storage_Offset (Right));
   end "+";

   function size_of_encoded_value (encoding : unsigned_char)
      return unsigned_int is
   begin
      if encoding = DW_EH_PE_omit then
         return 0;
      end if;
      case encoding and 16#07# is
         when DW_EH_PE_absptr =>
            return void_ptr'Size / Standard'Storage_Unit;
         when DW_EH_PE_udata2 =>
            return 2;
         when DW_EH_PE_udata4 =>
            return 4;
         when DW_EH_PE_udata8 =>
            return 8;
         when others =>
            null;
      end case;
      gxx_abort;
   end size_of_encoded_value;

   function base_of_encoded_value (
      encoding : unsigned_char;
      context : access unwind.struct_Unwind_Context)
      return unwind.Unwind_Ptr is
   begin
      if encoding = DW_EH_PE_omit then
         return 0;
      end if;
      case encoding and 16#70# is
         when DW_EH_PE_absptr
            | DW_EH_PE_pcrel
            | DW_EH_PE_aligned =>
            return 0;
         when DW_EH_PE_textrel =>
            return unwind.Unwind_GetTextRelBase (context);
         when DW_EH_PE_datarel =>
            return unwind.Unwind_GetDataRelBase (context);
         when DW_EH_PE_funcrel =>
            return unwind.Unwind_GetRegionStart (context);
         when others =>
            null;
      end case;
      gxx_abort;
   end base_of_encoded_value;

   function read_uleb128 (
      p : access constant unsigned_char;
      val : access unwind.uleb128_t)
      return unsigned_char_const_ptr
   is
      Mutable_p : unsigned_char_const_ptr := unsigned_char_const_ptr (p);
      shift : unsigned_int := 0;
      byte : unsigned_char;
      result : unwind.uleb128_t;
   begin
      result := 0;
      loop
         byte := Mutable_p.all;
         Mutable_p := Mutable_p + 1;
         if shift >= unwind.uleb128_t'Size then
            unreachable;
         end if;
         result := result or
            Shift_Left (unwind.uleb128_t (byte) and 16#7f#, Natural (shift));
         shift := shift + 7;
         exit when not ((byte and 16#80#) /= 0);
      end loop;
      val.all := result;
      return Mutable_p;
   end read_uleb128;

   function read_sleb128 (
      p : access constant unsigned_char;
      val : access unwind.sleb128_t)
      return unsigned_char_const_ptr
   is
      Mutable_p : unsigned_char_const_ptr := unsigned_char_const_ptr (p);
      shift : unsigned_int := 0;
      byte : unsigned_char;
      result : unwind.uleb128_t;
   begin
      result := 0;
      loop
         byte := Mutable_p.all;
         Mutable_p := Mutable_p + 1;
         if shift >= unwind.uleb128_t'Size then
            unreachable;
         end if;
         result := result or
            Shift_Left (unwind.uleb128_t (byte) and 16#7f#, Natural (shift));
         shift := shift + 7;
         exit when not ((byte and 16#80#) /= 0);
      end loop;
      if shift < result'Size and then (byte and 16#40#) /= 0 then
         result := result or (-Shift_Left (1, Natural (shift)));
      end if;
      val.all := unwind.sleb128_t (result);
      return Mutable_p;
   end read_sleb128;

   function read_encoded_value_with_base (
      encoding : unsigned_char;
      base : unwind.Unwind_Ptr;
      p : access constant unsigned_char;
      val : access unwind.Unwind_Ptr)
      return unsigned_char_const_ptr
   is
      function Cast is
         new Ada.Unchecked_Conversion (void_ptr, unwind.Unwind_Internal_Ptr);
      type Unwind_Internal_Ptr_ptr is access all unwind.Unwind_Internal_Ptr;
      function Cast is
         new Ada.Unchecked_Conversion (
            unwind.Unwind_Internal_Ptr,
            Unwind_Internal_Ptr_ptr);
      Mutable_p : unsigned_char_const_ptr := unsigned_char_const_ptr (p);
      type unaligned (unchecked_tag : unsigned_int := 0) is record
         case unchecked_tag is
            when 0 =>
               ptr : void_ptr;
            when DW_EH_PE_udata2 =>
               u2 : stdint.uint16_t;
            when DW_EH_PE_udata4 =>
               u4 : stdint.uint32_t;
            when DW_EH_PE_udata8 =>
               u8 : stdint.uint64_t;
            when DW_EH_PE_sdata2 =>
               s2 : stdint.int16_t;
            when DW_EH_PE_sdata4 =>
               s4 : stdint.int32_t;
            when others => -- DW_EH_PE_sdata8
               s8 : stdint.int64_t;
         end case;
      end record;
      pragma Unchecked_Union (unaligned);
      type unaligned_ptr is access all unaligned;
      function Cast is
         new Ada.Unchecked_Conversion (unsigned_char_const_ptr, unaligned_ptr);
      function Cast is
         new Ada.Unchecked_Conversion (
            unaligned_ptr,
            unwind.Unwind_Internal_Ptr);
      u : constant unaligned_ptr := Cast (Mutable_p);
      result : unwind.Unwind_Internal_Ptr;
   begin
      if encoding = DW_EH_PE_aligned then
         declare
            a : unwind.Unwind_Internal_Ptr :=
               unwind.Unwind_Internal_Ptr (
                  System.Storage_Elements.To_Integer (
                     unsigned_char_const_ptr_Conv.To_Address (Mutable_p)));
         begin
            a := (a + void_ptr'Size / Standard'Storage_Unit - 1) and
               -(void_ptr'Size / Standard'Storage_Unit);
            result := Cast (a).all;
            Mutable_p := unsigned_char_const_ptr_Conv.To_Pointer (
               System'To_Address (
                  System.Storage_Elements.Integer_Address (
                     a + void_ptr'Size / Standard'Storage_Unit)));
         end;
      else
         case encoding and 16#0f# is
            when DW_EH_PE_absptr =>
               result := Cast (u.ptr);
               Mutable_p := Mutable_p + void_ptr'Size / Standard'Storage_Unit;
            when DW_EH_PE_uleb128 =>
               declare
                  tmp : aliased unwind.uleb128_t;
               begin
                  Mutable_p := read_uleb128 (Mutable_p, tmp'Access);
                  result := unwind.Unwind_Internal_Ptr (tmp);
               end;
            when DW_EH_PE_sleb128 =>
               declare
                  tmp : aliased unwind.sleb128_t;
               begin
                  Mutable_p := read_sleb128 (Mutable_p, tmp'Access);
                  result := unwind.Unwind_Internal_Ptr (tmp);
               end;
            when DW_EH_PE_udata2 =>
               result := unwind.Unwind_Internal_Ptr (u.u2);
               Mutable_p := Mutable_p + 2;
            when DW_EH_PE_udata4 =>
               result := unwind.Unwind_Internal_Ptr (u.u4);
               Mutable_p := Mutable_p + 4;
            when DW_EH_PE_udata8 =>
               result := unwind.Unwind_Internal_Ptr (u.u8);
               Mutable_p := Mutable_p + 8;
            when DW_EH_PE_sdata2 =>
               result := unwind.Unwind_Internal_Ptr (u.s2);
               Mutable_p := Mutable_p + 2;
            when DW_EH_PE_sdata4 =>
               result := unwind.Unwind_Internal_Ptr (u.s4);
               Mutable_p := Mutable_p + 4;
            when DW_EH_PE_sdata8 =>
               result := unwind.Unwind_Internal_Ptr (u.s8);
               Mutable_p := Mutable_p + 8;
            when others =>
               gxx_abort;
         end case;
         if result /= 0 then
            if (encoding and 16#70#) = DW_EH_PE_pcrel then
               result := result + Cast (u);
            else
               result := result + base;
            end if;
            if (encoding and DW_EH_PE_indirect) /= 0 then
               result := Cast (result).all;
            end if;
         end if;
      end if;
      val.all := result;
      return Mutable_p;
   end read_encoded_value_with_base;

   function read_encoded_value (
      context : access unwind.struct_Unwind_Context;
      encoding : unsigned_char;
      p : access constant unsigned_char;
      val : access unwind.Unwind_Ptr)
      return unsigned_char_const_ptr is
   begin
      return read_encoded_value_with_base (
         encoding,
         base_of_encoded_value (encoding, context),
         p,
         val);
   end read_encoded_value;

end C.unwind_pe;
