--  unwind-pe.h is a part of gcc, not in runtime libraries.
pragma License (GPL); --  and "GCC Runtime Library Exception"
with C.stdlib;
with C.unwind;
package C.unwind_pe is
   pragma Preelaborate;

   subtype sleb128_t is signed_long;
   subtype uleb128_t is unsigned_long;

   procedure gxx_abort renames stdlib.C_abort;

   DW_EH_PE_absptr : constant := 16#00#;
   DW_EH_PE_omit : constant := 16#ff#;

   DW_EH_PE_uleb128 : constant := 16#01#;
   DW_EH_PE_udata2 : constant := 16#02#;
   DW_EH_PE_udata4 : constant := 16#03#;
   DW_EH_PE_udata8 : constant := 16#04#;
   DW_EH_PE_sleb128 : constant := 16#09#;
   DW_EH_PE_sdata2 : constant := 16#0A#;
   DW_EH_PE_sdata4 : constant := 16#0B#;
   DW_EH_PE_sdata8 : constant := 16#0C#;
   DW_EH_PE_signed : constant := 16#08#;

   DW_EH_PE_pcrel : constant := 16#10#;
   DW_EH_PE_textrel : constant := 16#20#;
   DW_EH_PE_datarel : constant := 16#30#;
   DW_EH_PE_funcrel : constant := 16#40#;
   DW_EH_PE_aligned : constant := 16#50#;

   DW_EH_PE_indirect : constant := 16#80#;

   function size_of_encoded_value (encoding : unsigned_char)
      return unsigned_int;

   function base_of_encoded_value (
      encoding : unsigned_char;
      context : access unwind.struct_Unwind_Context)
      return unwind.Unwind_Ptr;

   function read_uleb128 (
      p : access constant unsigned_char;
      val : access uleb128_t)
      return unsigned_char_const_ptr;

   function read_sleb128 (
      p : access constant unsigned_char;
      val : access sleb128_t)
      return unsigned_char_const_ptr;

   function read_encoded_value_with_base (
      encoding : unsigned_char;
      base : unwind.Unwind_Ptr;
      p : access constant unsigned_char;
      val : access unwind.Unwind_Ptr)
      return unsigned_char_const_ptr;

   function read_encoded_value (
      context : access unwind.struct_Unwind_Context;
      encoding : unsigned_char;
      p : access constant unsigned_char;
      val : access unwind.Unwind_Ptr)
      return unsigned_char_const_ptr;

end C.unwind_pe;
