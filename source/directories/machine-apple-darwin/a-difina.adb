--  reference:
--  http://www.opensource.apple.com/
--     source/boot/boot-132/i386/libsaio/hfs_compare.c
--  http://www.opensource.apple.com/
--     source/xnu/xnu-1504.15.3/bsd/vfs/vfs_utfconv.c
with System.Address_To_Constant_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
with System.Formatting;
with System.Once;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.UTF_Conversions;
with C.hfs_casetables;
with C.vfs_utfconvdata;
package body Ada.Directories.File_Names is
   use type System.Formatting.Digit;
   use type System.Storage_Elements.Integer_Address;
   use type System.Storage_Elements.Storage_Offset;
   use type System.UTF_Conversions.From_Status_Type;
   use type System.UTF_Conversions.UCS_4;
   use type C.signed_int;
   use type C.size_t;
   use type C.unsigned_char; -- implies u_int8_t
   use type C.unsigned_short; -- implies u_int16_t
   use type C.unsigned_short_ptr;
   use type C.unsigned_int;

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
         unsigned_short_ptr_Conv.To_Pointer (
            System.Standard_Allocators.Allocate (
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

   --  a part of FastUnicodeCompare (hfs_compare.c)
   function To_Lower (Item : System.UTF_Conversions.UCS_4)
      return System.UTF_Conversions.UCS_4;
   function To_Lower (Item : System.UTF_Conversions.UCS_4)
      return System.UTF_Conversions.UCS_4 is
   begin
      if Item > 16#ffff# then
         return Item; -- out of gCompareTable
      else
         declare
            subtype Fixed_Table is C.unsigned_short_array (C.size_t);
            Table : Fixed_Table;
            for Table'Address use C.hfs_casetables.gLowerCaseTable.all'Address;
            cn : constant C.unsigned_short := C.unsigned_short (Item);
            temp : constant C.unsigned_short := Table (C.size_t (cn / 256));
         begin
            if temp /= 0 then
               return System.UTF_Conversions.UCS_4 (
                  Table (C.size_t (temp + cn rem 256)));
            else
               return Item;
            end if;
         end;
      end if;
   end To_Lower;

   --  equivalent to get_combining_class (vfs_utfconv.c)
   function get_combining_class (character : C.vfs_utfconvdata.u_int16_t)
      return C.vfs_utfconvdata.u_int8_t;
   function get_combining_class (character : C.vfs_utfconvdata.u_int16_t)
      return C.vfs_utfconvdata.u_int8_t
   is
      bitmap : C.vfs_utfconvdata.u_int8_t_array
         renames C.vfs_utfconvdata.CFUniCharCombiningPropertyBitmap;
      value : constant C.vfs_utfconvdata.u_int8_t :=
         bitmap (C.size_t (C.Shift_Right (character, 8)));
   begin
      if value /= 0 then
         return bitmap (
            C.size_t (value) * 256
            + C.size_t (character and 16#00FF#));
      end if;
      return 0;
   end get_combining_class;

   --  equivalent to unicode_decomposeable (vfs_utfconv.c)
   function unicode_decomposeable (character : C.vfs_utfconvdata.u_int16_t)
      return Boolean;
   function unicode_decomposeable (character : C.vfs_utfconvdata.u_int16_t)
      return Boolean
   is
      bitmap : C.vfs_utfconvdata.u_int8_t_array
         renames C.vfs_utfconvdata.CFUniCharDecomposableBitmap;
      Offset : C.size_t;
      value : C.vfs_utfconvdata.u_int8_t;
   begin
      if character < 16#00C0# then
         return False;
      end if;
      value := bitmap (C.size_t (C.Shift_Right (character, 8) and 16#FF#));
      if value = 16#FF# then
         return True;
      elsif value /= 0 then
         Offset := (C.size_t (value) - 1) * 32 + 256;
         return (bitmap (Offset + C.size_t ((character and 16#FF#) / 8))
            and C.Shift_Left (1, Natural (character rem 8))) /= 0;
      end if;
      return False;
   end unicode_decomposeable;

   RECURSIVE_DECOMPOSITION : constant := 2 ** 15;

   function EXTRACT_COUNT (value : C.vfs_utfconvdata.u_int16_t)
      return C.size_t;
   function EXTRACT_COUNT (value : C.vfs_utfconvdata.u_int16_t)
      return C.size_t is
   begin
      return C.size_t (
         C.Shift_Right (value, 12) and 16#0007#);
   end EXTRACT_COUNT;

   type unicode_mappings16 is record
      key : C.vfs_utfconvdata.u_int16_t;
      value : C.vfs_utfconvdata.u_int16_t;
   end record;
   pragma Suppress_Initialization (unicode_mappings16);

   --  equivalent to getmappedvalue16 (vfs_utfconv.c)
   function getmappedvalue16 (
      theTable : System.Address;
      numElem : C.size_t;
      character : C.vfs_utfconvdata.u_int16_t)
      return C.vfs_utfconvdata.u_int16_t;
   function getmappedvalue16 (
      theTable : System.Address;
      numElem : C.size_t;
      character : C.vfs_utfconvdata.u_int16_t)
      return C.vfs_utfconvdata.u_int16_t
   is
      type unicode_mappings16_array is array (C.size_t) of unicode_mappings16;
      pragma Suppress_Initialization (unicode_mappings16_array);
      table : unicode_mappings16_array;
      for table'Address use theTable;
      p, q, divider : C.size_t;
   begin
      if character < table (0).key
         or else character > table (numElem - 1).key
      then
         return 0;
      end if;
      p := 0;
      q := numElem - 1;
      while p <= q loop
         divider := p + (q - p) / 2; -- divide by 2
         if character < table (divider).key then
            q := divider - 1;
         elsif character > table (divider).key then
            p := divider + 1;
         else
            return table (divider).value;
         end if;
      end loop;
      return 0;
   end getmappedvalue16;

   Expanding : constant := 4; -- max decomposed length of one code point

   subtype decompose_buffer is
      C.vfs_utfconvdata.u_int16_t_array (0 .. Expanding - 1);

   type u_int16_t_ptr is access constant C.vfs_utfconvdata.u_int16_t;

   package u_int16_t_ptr_Conv is
      new System.Address_To_Constant_Access_Conversions (
         C.vfs_utfconvdata.u_int16_t,
         u_int16_t_ptr);

   --  equivalent to unicode_recursive_decompose (vfs_utfconv.c)
   procedure unicode_recursive_decompose (
      character : C.vfs_utfconvdata.u_int16_t;
      convertedChars : out decompose_buffer;
      usedLength : out C.size_t);
   procedure unicode_recursive_decompose (
      character : C.vfs_utfconvdata.u_int16_t;
      convertedChars : out decompose_buffer;
      usedLength : out C.size_t)
   is
      value : C.vfs_utfconvdata.u_int16_t;
      length : C.size_t;
      firstChar : C.vfs_utfconvdata.u_int16_t;
      theChar : aliased C.vfs_utfconvdata.u_int16_t;
      bmpMappings : u_int16_t_ptr;
   begin
      value := getmappedvalue16 (
         C.vfs_utfconvdata.CFUniCharDecompositionTable'Address,
         C.vfs_utfconvdata.CFUniCharDecompositionTable'Length / 2,
         character);
      length := EXTRACT_COUNT (value);
      firstChar := value and 16#0FFF#;
      theChar := firstChar;
      if length = 1 then
         bmpMappings := theChar'Unchecked_Access;
      else
         bmpMappings := u_int16_t_ptr_Conv.To_Pointer (
            C.vfs_utfconvdata.CFUniCharMultipleDecompositionTable'Address
            + System.Storage_Elements.Storage_Offset (firstChar)
               * (C.vfs_utfconvdata.u_int16_t'Size / Standard'Storage_Unit));
      end if;
      usedLength := 0;
      if (value and RECURSIVE_DECOMPOSITION) /= 0 then
         unicode_recursive_decompose (
            bmpMappings.all,
            convertedChars,
            usedLength);
         length := length - 1; -- Decrement for the first char
         if usedLength = 0 then
            return;
         end if;
         bmpMappings := u_int16_t_ptr_Conv.To_Pointer (
            u_int16_t_ptr_Conv.To_Address (bmpMappings)
            + (C.vfs_utfconvdata.u_int16_t'Size / Standard'Storage_Unit));
      end if;
      for I in 0 .. length - 1 loop
         convertedChars (usedLength + I) := bmpMappings.all;
         bmpMappings := u_int16_t_ptr_Conv.To_Pointer (
            u_int16_t_ptr_Conv.To_Address (bmpMappings)
            + (C.vfs_utfconvdata.u_int16_t'Size / Standard'Storage_Unit));
      end loop;
      usedLength := usedLength + length;
   end unicode_recursive_decompose;

   HANGUL_SBASE : constant := 16#AC00#;
   HANGUL_LBASE : constant := 16#1100#;
   HANGUL_VBASE : constant := 16#1161#;
   HANGUL_TBASE : constant := 16#11A7#;

   HANGUL_SCOUNT : constant := 11172;
--  HANGUL_LCOUNT : constant := 19;
   HANGUL_VCOUNT : constant := 21;
   HANGUL_TCOUNT : constant := 28;
   HANGUL_NCOUNT : constant := HANGUL_VCOUNT * HANGUL_TCOUNT;

   --  equivalent to unicode_decompose (vfs_utfconv.c)
   procedure unicode_decompose (
      character : C.vfs_utfconvdata.u_int16_t;
      convertedChars : out decompose_buffer;
      length : out C.size_t);
   procedure unicode_decompose (
      character : C.vfs_utfconvdata.u_int16_t;
      convertedChars : out decompose_buffer;
      length : out C.size_t) is
   begin
      if character >= HANGUL_SBASE
         and then character <= HANGUL_SBASE + HANGUL_SCOUNT
      then
         declare
            sindex : constant C.vfs_utfconvdata.u_int16_t :=
               character - HANGUL_SBASE;
         begin
            if sindex rem HANGUL_TCOUNT /= 0 then
               length := 3;
            else
               length := 2;
            end if;
            convertedChars (0) :=
               sindex / HANGUL_NCOUNT + HANGUL_LBASE;
            convertedChars (1) :=
               (sindex rem HANGUL_NCOUNT) / HANGUL_TCOUNT + HANGUL_VBASE;
            if length > 2 then
               convertedChars (2) :=
                  (sindex rem HANGUL_TCOUNT) + HANGUL_TBASE;
            end if;
         end;
      else
         unicode_recursive_decompose (character, convertedChars, length);
      end if;
   end unicode_decompose;

   type Stack_Type is
      array (Positive range <>) of System.UTF_Conversions.UCS_4;
   pragma Suppress_Initialization (Stack_Type);

   --  a part of utf8_decodestr (vfs_utfconv.c)
   procedure Push (
      S : String;
      Index : in out Positive;
      Stack : in out Stack_Type;
      Top : in out Natural);
   procedure Push (
      S : String;
      Index : in out Positive;
      Stack : in out Stack_Type;
      Top : in out Natural)
   is
      Last : Natural;
      Code : System.UTF_Conversions.UCS_4;
      From_Status : System.UTF_Conversions.From_Status_Type;
   begin
      System.UTF_Conversions.From_UTF_8 (
         S (Index .. S'Last),
         Last,
         Code,
         From_Status);
      if From_Status /= System.UTF_Conversions.Success then
         --  an illegal sequence is replaced to %XX at HFS
         declare
            Illegal : Character;
            D : Character;
         begin
            Illegal := S (Index);
            Stack (3) := Wide_Wide_Character'Pos ('%');
            System.Formatting.Image (
               Character'Pos (Illegal) / 16,
               D,
               Set => System.Formatting.Upper_Case);
            Stack (2) := Character'Pos (D);
            System.Formatting.Image (
               Character'Pos (Illegal) rem 16,
               D,
               Set => System.Formatting.Upper_Case);
            Stack (1) := Character'Pos (D);
            Top := 3;
         end;
         Index := Index + 1;
      else
         Top := 0;
         --  push trailing composable characters
         while Last < S'Last loop
            declare
               Trailing_Last : Natural;
               Trailing_Code : System.UTF_Conversions.UCS_4;
               Trailing_From_Status : System.UTF_Conversions.From_Status_Type;
            begin
               System.UTF_Conversions.From_UTF_8 (
                  S (Last + 1 .. S'Last),
                  Trailing_Last,
                  Trailing_Code,
                  Trailing_From_Status);
               if Trailing_From_Status = System.UTF_Conversions.Success
                  and then Trailing_Code < 16#FFFF#
                  and then get_combining_class (
                     C.vfs_utfconvdata.u_int16_t (Trailing_Code)) /= 0
               then
                  Top := Top + 1;
                  Stack (Top) := Trailing_Code;
                  Last := Trailing_Last;
               else
                  exit;
               end if;
            end;
         end loop;
         --  reverse trailing composable characters
         for I in 0 .. Top / 2 - 1 loop
            declare
               Temp : constant System.UTF_Conversions.UCS_4 :=
                  Stack (Stack'First + I);
            begin
               Stack (Stack'First + I) := Stack (Top - I);
               Stack (Top - I) := Temp;
            end;
         end loop;
         --  push taken code
         if Code < 16#FFFF#
            and then unicode_decomposeable (C.vfs_utfconvdata.u_int16_t (Code))
         then
            declare
               sequence : decompose_buffer;
               count : C.size_t;
               ucs_ch : C.vfs_utfconvdata.u_int16_t;
            begin
               unicode_decompose (
                  C.vfs_utfconvdata.u_int16_t (Code),
                  sequence,
                  count);
               --  push decomposed sequence
               for i in reverse 0 .. count - 1 loop
                  ucs_ch := sequence (i);
                  Top := Top + 1;
                  Stack (Top) := System.UTF_Conversions.UCS_4 (ucs_ch);
               end loop;
            end;
         else
            Top := Top + 1;
            Stack (Top) := Code;
         end if;
         --  sort combining characters in Stack (1 .. Top - 1)
         --  equivalent to priortysort (vfs_utfconv.c) except reverse order
         for I in reverse 1 .. Top - 2 loop
            declare
               Item : constant System.UTF_Conversions.UCS_4 := Stack (I);
               Item_Class : constant C.vfs_utfconvdata.u_int8_t :=
                  get_combining_class (
                     C.vfs_utfconvdata.u_int16_t (Stack (I)));
            begin
               for J in I + 1 .. Top - 1 loop
                  exit when get_combining_class (
                     C.vfs_utfconvdata.u_int16_t (Stack (J))) <= Item_Class;
                  Stack (J - 1) := Stack (J);
                  Stack (J) := Item;
               end loop;
            end;
         end loop;
         Index := Last + 1;
      end if;
   end Push;

   function HFS_Compare (Left, Right : String) return Integer;
   function HFS_Compare (Left, Right : String) return Integer is
      L_Stack : Stack_Type (1 .. Left'Length + Expanding - 1);
      R_Stack : Stack_Type (1 .. Right'Length + Expanding - 1);
      L_Top : Natural := 0;
      R_Top : Natural := 0;
      L_Index : Positive := Left'First;
      R_Index : Positive := Right'First;
   begin
      loop
         if L_Top = 0 then
            if L_Index > Left'Last then
               if R_Index > Right'Last and then R_Top = 0 then
                  return 0;
               else
                  return -1; -- Left'Length < Right'Length
               end if;
            end if;
            Push (Left, L_Index, L_Stack, L_Top);
         end if;
         if R_Top = 0 then
            if R_Index > Right'Last then
               return 1; -- Left'Length > Right'Length
            end if;
            Push (Right, R_Index, R_Stack, R_Top);
         end if;
         declare
            L_Code : System.UTF_Conversions.UCS_4;
            R_Code : System.UTF_Conversions.UCS_4;
         begin
            L_Code := To_Lower (L_Stack (L_Top));
            R_Code := To_Lower (R_Stack (R_Top));
            if L_Code /= R_Code then
               if L_Code < R_Code then
                  return -1;
               else
                  return 1;
               end if;
            end if;
         end;
         L_Top := L_Top - 1;
         R_Top := R_Top - 1;
      end loop;
   end HFS_Compare;

   --  implementation

   function Equal_File_Names (
      FS : Volumes.File_System;
      Left, Right : String)
      return Boolean is
   begin
      if Volumes.Is_HFS (FS) then
         System.Once.Initialize (Flag'Access, InitCompareTables'Access);
         return HFS_Compare (Left, Right) = 0;
      else
         return Left = Right;
      end if;
   end Equal_File_Names;

   function Less_File_Names (
      FS : Volumes.File_System;
      Left, Right : String)
      return Boolean is
   begin
      if Volumes.Is_HFS (FS) then
         System.Once.Initialize (Flag'Access, InitCompareTables'Access);
         return HFS_Compare (Left, Right) < 0;
      else
         return Left < Right;
      end if;
   end Less_File_Names;

end Ada.Directories.File_Names;
