--  reference:
--  http://www.opensource.apple.com/
--     source/boot/boot-132/i386/libsaio/hfs_compare.c
with Ada.Characters.Normalization;
with System.Address_To_Named_Access_Conversions;
with System.Address_To_Constant_Access_Conversions;
with System.Memory;
with System.Storage_Elements;
with System.Once;
with C.hfs_casetables;
package body Ada.Directories.Inside.File_Names is
   use type System.Storage_Elements.Storage_Offset;
   use type C.signed_int;
   use type C.size_t;
   use type C.unsigned_char;
   use type C.unsigned_short;
   use type C.unsigned_short_ptr;

   Flag : aliased System.Once.Flag := 0;

   --  equivalent to UncompressStructure (hfs_compare.c)
   function UncompressStructure (
      bp : not null C.hfs_casetables.compressed_block_const_ptr;
      count : C.signed_int;
      size : C.signed_int)
      return C.unsigned_short_ptr;
   function UncompressStructure (
      bp : not null C.hfs_casetables.compressed_block_const_ptr;
      count : C.signed_int;
      size : C.signed_int)
      return C.unsigned_short_ptr
   is
      package unsigned_short_ptr_Conv is
         new System.Address_To_Named_Access_Conversions (
            C.unsigned_short,
            C.unsigned_short_ptr);
      package compressed_block_const_ptr_Conv is
         new System.Address_To_Constant_Access_Conversions (
            C.hfs_casetables.compressed_block,
            C.hfs_casetables.compressed_block_const_ptr);
      m_bp : not null C.hfs_casetables.compressed_block_const_ptr := bp;
      l_out : constant C.unsigned_short_ptr :=
         unsigned_short_ptr_Conv.To_Pointer (System.Memory.Allocate (
            System.Storage_Elements.Storage_Offset (size)));
      op : C.unsigned_short_ptr := l_out;
      data : C.unsigned_short;
   begin
      for i in 0 .. count - 1 loop
         data := m_bp.data;
         for j in 0 .. m_bp.count - 1 loop
            op.all := data;
            op := unsigned_short_ptr_Conv.To_Pointer (
               unsigned_short_ptr_Conv.To_Address (op)
               + C.unsigned_short'Size / Standard'Storage_Unit);
            if m_bp.f_type = C.hfs_casetables.kTypeAscending then
               data := data + 1;
            elsif m_bp.f_type = C.hfs_casetables.kTypeAscending256 then
               data := data + 256;
            end if;
         end loop;
         m_bp := compressed_block_const_ptr_Conv.To_Pointer (
            compressed_block_const_ptr_Conv.To_Address (m_bp)
            + C.hfs_casetables.compressed_block'Size / Standard'Storage_Unit);
      end loop;
      return l_out;
   end UncompressStructure;

   --  equivalent to InitCompareTables (hfs_compare.c)
   procedure InitCompareTables;
   procedure InitCompareTables is
   begin
--    C.hfs_casetables.gCompareTable := UncompressStructure (
--       C.hfs_casetables.gCompareTableCompressed (0)'Access,
--       C.hfs_casetables.kCompareTableNBlocks,
--       C.hfs_casetables.kCompareTableDataSize);
      C.hfs_casetables.gLowerCaseTable := UncompressStructure (
         C.hfs_casetables.gLowerCaseTableCompressed (0)'Access,
         C.hfs_casetables.kLowerCaseTableNBlocks,
         C.hfs_casetables.kLowerCaseTableDataSize);
   end InitCompareTables;

   function To_Lower (Item : Wide_Wide_Character) return Wide_Wide_Character;
   function To_Lower (Item : Wide_Wide_Character) return Wide_Wide_Character is
   begin
      if Wide_Wide_Character'Pos (Item) > 16#ffff# then
         return Item; -- out of gCompareTable
      else
         declare
            subtype Fixed_Table is C.unsigned_short_array (C.size_t);
            Table : Fixed_Table;
            for Table'Address use C.hfs_casetables.gLowerCaseTable.all'Address;
            cn : constant C.unsigned_short := Wide_Wide_Character'Pos (Item);
            temp : constant C.unsigned_short := Table (C.size_t (cn / 256));
         begin
            if temp /= 0 then
               return Wide_Wide_Character'Val (
                  Table (C.size_t (temp + cn rem 256)));
            else
               return Item;
            end if;
         end;
      end if;
   end To_Lower;

   --  equivalent to FastUnicodeCompare (hfs_compare.c)
   function FastUnicodeCompare (Left, Right : Wide_Wide_String)
      return Integer;
   function FastUnicodeCompare (Left, Right : Wide_Wide_String)
      return Integer
   is
      I : Positive := Left'First;
      J : Positive := Right'First;
   begin
      loop
         if I > Left'Last then
            if J > Right'Last then
               return 0;
            else
               return -1; -- Left'Length < Right'Length
            end if;
         elsif J > Right'Last then
            return 1; -- Left'Length > Right'Length
         end if;
         declare
            C1 : constant Wide_Wide_Character := To_Lower (Left (I));
            C2 : constant Wide_Wide_Character := To_Lower (Right (J));
         begin
            if C1 /= C2 then
               if C1 < C2 then
                  return -1;
               else
                  return 1;
               end if;
            end if;
         end;
         I := I + 1;
         J := J + 1;
      end loop;
   end FastUnicodeCompare;

   function Equal_Case_Insensitive (Left, Right : Wide_Wide_String)
      return Boolean;
   function Equal_Case_Insensitive (Left, Right : Wide_Wide_String)
      return Boolean is
   begin
      return FastUnicodeCompare (Left, Right) = 0;
   end Equal_Case_Insensitive;

   function Less_Case_Insensitive (Left, Right : Wide_Wide_String)
      return Boolean;
   function Less_Case_Insensitive (Left, Right : Wide_Wide_String)
      return Boolean is
   begin
      return FastUnicodeCompare (Left, Right) < 0;
   end Less_Case_Insensitive;

   --  implementation

   function Equal_File_Names (Left, Right : String) return Boolean is
   begin
      System.Once.Initialize (Flag'Access, InitCompareTables'Access);
      return Characters.Normalization.Equal (
         Left,
         Right,
         Equal_Case_Insensitive'Access);
   end Equal_File_Names;

   function Less_File_Names (Left, Right : String) return Boolean is
   begin
      System.Once.Initialize (Flag'Access, InitCompareTables'Access);
      return Characters.Normalization.Less (
         Left,
         Right,
         Less_Case_Insensitive'Access);
   end Less_File_Names;

end Ada.Directories.Inside.File_Names;
