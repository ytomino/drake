pragma Check_Policy (Validate, Off);
with Ada.Exception_Identification.From_Here;
with System.Storage_Elements;
with System.UTF_Conversions;
package body Ada.Strings.UTF_Encoding.Conversions is
   use Exception_Identification.From_Here;
   use type System.Storage_Elements.Storage_Offset;
   use type System.UTF_Conversions.From_Status_Type;
   use type System.UTF_Conversions.To_Status_Type;
   use type System.UTF_Conversions.UCS_4;

   --  binary to Wide_String, Wide_Wide_String

   function To_UTF_16_Wide_String (Item : System.Address; Length : Natural)
      return UTF_16_Wide_String;
   function To_UTF_16_Wide_String (Item : System.Address; Length : Natural)
      return UTF_16_Wide_String
   is
      pragma Suppress (Alignment_Check); -- Item'Alignment = 2
      pragma Check (Validate, Length rem 2 = 0);
      pragma Check (Validate, Item mod 2 = 0); -- stack may be aligned
      Result : UTF_16_Wide_String (1 .. Length / 2);
      for Result'Address use Item;
   begin
      return Result;
   end To_UTF_16_Wide_String;

   function To_UTF_32_Wide_Wide_String (
      Item : System.Address;
      Length : Natural)
      return UTF_32_Wide_Wide_String;
   function To_UTF_32_Wide_Wide_String (
      Item : System.Address;
      Length : Natural)
      return UTF_32_Wide_Wide_String
   is
      pragma Suppress (Alignment_Check); -- Item'Alignment = 4
      pragma Check (Validate, Length rem 4 = 0);
      pragma Check (Validate, Item mod 4 = 0); -- stack may be aligned
      Result : UTF_32_Wide_Wide_String (1 .. Length / 4);
      for Result'Address use Item;
   begin
      return Result;
   end To_UTF_32_Wide_Wide_String;

   --  binary version subprograms of System.UTF_Conversions

   procedure To_UTF_8 (
      Code : System.UTF_Conversions.UCS_4;
      Result : out UTF_String;
      Last : out Natural;
      Status : out System.UTF_Conversions.To_Status_Type)
      renames System.UTF_Conversions.To_UTF_8;

   procedure From_UTF_8 (
      Data : UTF_String;
      Last : out Natural;
      Result : out System.UTF_Conversions.UCS_4;
      Status : out System.UTF_Conversions.From_Status_Type)
      renames System.UTF_Conversions.From_UTF_8;

   procedure To_UTF_16BE (
      Code : System.UTF_Conversions.UCS_4;
      Result : out UTF_String;
      Last : out Natural;
      Status : out System.UTF_Conversions.To_Status_Type);
   procedure To_UTF_16BE (
      Code : System.UTF_Conversions.UCS_4;
      Result : out UTF_String;
      Last : out Natural;
      Status : out System.UTF_Conversions.To_Status_Type)
   is
      W_Result : Wide_String (1 .. System.UTF_Conversions.UTF_16_Max_Length);
      W_Last : Natural;
   begin
      System.UTF_Conversions.To_UTF_16 (Code, W_Result, W_Last, Status);
      Last := Result'First - 1;
      for I in 1 .. W_Last loop
         declare
            type U16 is mod 2 ** 16;
            E : constant U16 := Wide_Character'Pos (W_Result (I));
         begin
            Last := Last + 1;
            Result (Last) := Character'Val (E / 256);
            Last := Last + 1;
            Result (Last) := Character'Val (E rem 256);
         end;
      end loop;
   end To_UTF_16BE;

   procedure From_UTF_16BE (
      Data : UTF_String;
      Last : out Natural;
      Result : out System.UTF_Conversions.UCS_4;
      Status : out System.UTF_Conversions.From_Status_Type);
   procedure From_UTF_16BE (
      Data : UTF_String;
      Last : out Natural;
      Result : out System.UTF_Conversions.UCS_4;
      Status : out System.UTF_Conversions.From_Status_Type) is
   begin
      if Data'Length < 2 then
         Last := Data'Last;
         Status := System.UTF_Conversions.Truncated;
      else
         declare
            Leading : constant Wide_Character := Wide_Character'Val (
               Character'Pos (Data (Data'First)) * 256
               + Character'Pos (Data (Data'First + 1)));
            Length : Natural;
         begin
            Last := Data'First + 1;
            System.UTF_Conversions.UTF_16_Sequence (Leading, Length, Status);
            if Status = System.UTF_Conversions.Success then
               if Length = 2 then
                  if Data'Length < 4 then
                     Last := Data'Last;
                     Status := System.UTF_Conversions.Truncated;
                  else
                     declare
                        Trailing : constant Wide_Character :=
                           Wide_Character'Val (
                              Character'Pos (Data (Data'First + 2)) * 256
                              + Character'Pos (Data (Data'First + 3)));
                        W_Data : constant Wide_String (
                           1 ..
                           System.UTF_Conversions.UTF_16_Max_Length) :=
                           (Leading, Trailing);
                        W_Last : Natural;
                     begin
                        Last := Data'First + 3;
                        System.UTF_Conversions.From_UTF_16 (
                           W_Data,
                           W_Last,
                           Result,
                           Status);
                     end;
                  end if;
               else
                  pragma Check (Validate, Length = 1);
                  Result := Wide_Character'Pos (Leading);
               end if;
            end if;
         end;
      end if;
   end From_UTF_16BE;

   procedure To_UTF_16LE (
      Code : System.UTF_Conversions.UCS_4;
      Result : out UTF_String;
      Last : out Natural;
      Status : out System.UTF_Conversions.To_Status_Type);
   procedure To_UTF_16LE (
      Code : System.UTF_Conversions.UCS_4;
      Result : out UTF_String;
      Last : out Natural;
      Status : out System.UTF_Conversions.To_Status_Type)
   is
      W_Result : Wide_String (1 .. System.UTF_Conversions.UTF_16_Max_Length);
      W_Last : Natural;
   begin
      System.UTF_Conversions.To_UTF_16 (Code, W_Result, W_Last, Status);
      Last := Result'First - 1;
      for I in 1 .. W_Last loop
         declare
            type U16 is mod 2 ** 16;
            E : constant U16 := Wide_Character'Pos (W_Result (I));
         begin
            Last := Last + 1;
            Result (Last) := Character'Val (E rem 256);
            Last := Last + 1;
            Result (Last) := Character'Val (E / 256);
         end;
      end loop;
   end To_UTF_16LE;

   procedure From_UTF_16LE (
      Data : UTF_String;
      Last : out Natural;
      Result : out System.UTF_Conversions.UCS_4;
      Status : out System.UTF_Conversions.From_Status_Type);
   procedure From_UTF_16LE (
      Data : UTF_String;
      Last : out Natural;
      Result : out System.UTF_Conversions.UCS_4;
      Status : out System.UTF_Conversions.From_Status_Type) is
   begin
      if Data'Length < 2 then
         Last := Data'Last;
         Status := System.UTF_Conversions.Truncated;
      else
         declare
            Leading : constant Wide_Character := Wide_Character'Val (
               Character'Pos (Data (Data'First))
               + Character'Pos (Data (Data'First + 1)) * 256);
            Length : Natural;
         begin
            Last := Data'First + 1;
            System.UTF_Conversions.UTF_16_Sequence (Leading, Length, Status);
            if Status = System.UTF_Conversions.Success then
               if Length = 2 then
                  if Data'Length < 4 then
                     Last := Data'Last;
                     Status := System.UTF_Conversions.Truncated;
                  else
                     declare
                        Trailing : constant Wide_Character :=
                           Wide_Character'Val (
                              Character'Pos (Data (Data'First + 2))
                              + Character'Pos (Data (Data'First + 3)) * 256);
                        W_Data : constant Wide_String (
                           1 ..
                           System.UTF_Conversions.UTF_16_Max_Length) :=
                           (Leading, Trailing);
                        W_Last : Natural;
                     begin
                        Last := Data'First + 3;
                        System.UTF_Conversions.From_UTF_16 (
                           W_Data,
                           W_Last,
                           Result,
                           Status);
                     end;
                  end if;
               else
                  pragma Check (Validate, Length = 1);
                  Result := Wide_Character'Pos (Leading);
               end if;
            end if;
         end;
      end if;
   end From_UTF_16LE;

   procedure To_UTF_32BE (
      Code : System.UTF_Conversions.UCS_4;
      Result : out UTF_String;
      Last : out Natural;
      Status : out System.UTF_Conversions.To_Status_Type);
   procedure To_UTF_32BE (
      Code : System.UTF_Conversions.UCS_4;
      Result : out UTF_String;
      Last : out Natural;
      Status : out System.UTF_Conversions.To_Status_Type) is
   begin
      Last := Result'First;
      Result (Last) := Character'Val (Code / 16#1000000#);
      Last := Last + 1;
      Result (Last) := Character'Val (Code / 16#10000# rem 16#100#);
      Last := Last + 1;
      Result (Last) := Character'Val (Code / 16#100# rem 16#100#);
      Last := Last + 1;
      Result (Last) := Character'Val (Code rem 16#100#);
      Status := System.UTF_Conversions.Success;
   end To_UTF_32BE;

   procedure From_UTF_32BE (
      Data : UTF_String;
      Last : out Natural;
      Result : out System.UTF_Conversions.UCS_4;
      Status : out System.UTF_Conversions.From_Status_Type);
   procedure From_UTF_32BE (
      Data : UTF_String;
      Last : out Natural;
      Result : out System.UTF_Conversions.UCS_4;
      Status : out System.UTF_Conversions.From_Status_Type) is
   begin
      if Data'Length < 4 then
         Last := Data'Last;
         Status := System.UTF_Conversions.Truncated;
      else
         declare
            type U32 is mod 2 ** 32; -- Wide_Wide_Character'Size = 31(?)
            Leading : constant U32 :=
               Character'Pos (Data (Data'First)) * 16#1000000#
               + Character'Pos (Data (Data'First + 1)) * 16#10000#
               + Character'Pos (Data (Data'First + 2)) * 16#100#
               + Character'Pos (Data (Data'First + 3));
            Length : Natural;
         begin
            Last := Data'First + 3;
            if Leading > U32 (System.UTF_Conversions.UCS_4'Last) then
               Status := System.UTF_Conversions.Illegal_Sequence;
            else
               Result := System.UTF_Conversions.UCS_4 (Leading);
               System.UTF_Conversions.UTF_32_Sequence (
                  Wide_Wide_Character'Val (Leading),
                  Length,
                  Status); -- checking surrogate pair
            end if;
         end;
      end if;
   end From_UTF_32BE;

   procedure To_UTF_32LE (
      Code : System.UTF_Conversions.UCS_4;
      Result : out UTF_String;
      Last : out Natural;
      Status : out System.UTF_Conversions.To_Status_Type);
   procedure To_UTF_32LE (
      Code : System.UTF_Conversions.UCS_4;
      Result : out UTF_String;
      Last : out Natural;
      Status : out System.UTF_Conversions.To_Status_Type) is
   begin
      Last := Result'First;
      Result (Last) := Character'Val (Code rem 16#100#);
      Last := Last + 1;
      Result (Last) := Character'Val (Code / 16#100# rem 16#100#);
      Last := Last + 1;
      Result (Last) := Character'Val (Code / 16#10000# rem 16#100#);
      Last := Last + 1;
      Result (Last) := Character'Val (Code / 16#1000000#);
      Status := System.UTF_Conversions.Success;
   end To_UTF_32LE;

   procedure From_UTF_32LE (
      Data : UTF_String;
      Last : out Natural;
      Result : out System.UTF_Conversions.UCS_4;
      Status : out System.UTF_Conversions.From_Status_Type);
   procedure From_UTF_32LE (
      Data : UTF_String;
      Last : out Natural;
      Result : out System.UTF_Conversions.UCS_4;
      Status : out System.UTF_Conversions.From_Status_Type) is
   begin
      if Data'Length < 4 then
         Last := Data'Last;
         Status := System.UTF_Conversions.Truncated;
      else
         declare
            type U32 is mod 2 ** 32; -- Wide_Wide_Character'Size = 31(?)
            Leading : constant U32 :=
               Character'Pos (Data (Data'First))
               + Character'Pos (Data (Data'First + 1)) * 16#100#
               + Character'Pos (Data (Data'First + 2)) * 16#10000#
               + Character'Pos (Data (Data'First + 3)) * 16#1000000#;
            Length : Natural;
         begin
            Last := Data'First + 3;
            if Leading > U32 (System.UTF_Conversions.UCS_4'Last) then
               Status := System.UTF_Conversions.Illegal_Sequence;
            else
               Result := System.UTF_Conversions.UCS_4 (Leading);
               System.UTF_Conversions.UTF_32_Sequence (
                  Wide_Wide_Character'Val (Leading),
                  Length,
                  Status); -- checking surrogate pair
            end if;
         end;
      end if;
   end From_UTF_32LE;

   To_UTF : constant array (Encoding_Scheme) of
      not null access procedure (
         Code : System.UTF_Conversions.UCS_4;
         Result : out UTF_String;
         Last : out Natural;
         Status : out System.UTF_Conversions.To_Status_Type) := (
      UTF_8 => To_UTF_8'Access,
      UTF_16BE => To_UTF_16BE'Access,
      UTF_16LE => To_UTF_16LE'Access,
      UTF_32BE => To_UTF_32BE'Access,
      UTF_32LE => To_UTF_32LE'Access);

   From_UTF : constant array (Encoding_Scheme) of
      not null access procedure (
         Data : UTF_String;
         Last : out Natural;
         Result : out System.UTF_Conversions.UCS_4;
         Status : out System.UTF_Conversions.From_Status_Type) := (
      UTF_8 => From_UTF_8'Access,
      UTF_16BE => From_UTF_16BE'Access,
      UTF_16LE => From_UTF_16LE'Access,
      UTF_32BE => From_UTF_32BE'Access,
      UTF_32LE => From_UTF_32LE'Access);

   --  conversions between various encoding schemes

   procedure Do_Convert (
      Item : UTF_String;
      Input_Scheme : Encoding_Scheme;
      Output_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False;
      Result : out UTF_String;
      Last : out Natural);
   procedure Do_Convert (
      Item : UTF_String;
      Input_Scheme : Encoding_Scheme;
      Output_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False;
      Result : out UTF_String;
      Last : out Natural)
   is
      In_BOM : constant not null access constant UTF_String :=
         BOM_Table (Input_Scheme);
      Out_BOM : constant not null access constant UTF_String :=
         BOM_Table (Output_Scheme);
      Item_Last : Natural := Item'First - 1;
   begin
      if Item'Length >= In_BOM.all'Length
         and then Item (Item_Last + 1 .. Item_Last + In_BOM.all'Length) =
            In_BOM.all
      then
         Item_Last := Item_Last + In_BOM.all'Length;
      end if;
      Last := Result'First - 1;
      if Output_BOM then
         Last := Last + Out_BOM.all'Length;
         Result (Result'First .. Last) := Out_BOM.all;
      end if;
      while Item_Last < Item'Last loop
         declare
            Code : System.UTF_Conversions.UCS_4;
            From_Status : System.UTF_Conversions.From_Status_Type;
            To_Status : System.UTF_Conversions.To_Status_Type;
         begin
            From_UTF (Input_Scheme) (
               Item (Item_Last + 1 .. Item'Last),
               Item_Last,
               Code,
               From_Status);
            if From_Status /= System.UTF_Conversions.Success then
               Raise_Exception (Encoding_Error'Identity);
            end if;
            To_UTF (Output_Scheme) (
               Code,
               Result (Last + 1 .. Result'Last),
               Last,
               To_Status);
            if To_Status /= System.UTF_Conversions.Success then
               Raise_Exception (Encoding_Error'Identity);
            end if;
         end;
      end loop;
   end Do_Convert;

   --  implementation

   function Convert (
      Item : UTF_String;
      Input_Scheme : Encoding_Scheme;
      Output_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False)
      return UTF_String
   is
      Result : UTF_String (1 .. 4 * Item'Length + 4);
      --  from 8 to 8 : Item'Length + 3
      --  from 16 to 8 : 3 * Item'Length / 2 + 3 = 3/2 * Item'Length + 3
      --  from 32 to 8 : 6 * Item'Length / 4 + 3 = 2 * Item'Length + 3
      --  from 8 to 16 : (Item'Length + 1) * 2 = 2 * Item'Length + 2
      --  from 16 to 16 : (Item'Length / 2 + 1) * 2 = Item'Length + 2
      --  from 32 to 16 : (2 * Item'Length / 4 + 1) * 2 = Item'Length + 2
      --  from 8 to 32 : (Item'Length + 1) * 4 = 4 * Item'Length + 4 (max)
      --  from 16 to 32 : (Item'Length / 2 + 1) * 4 = 2 * Item'Length + 4
      --  from 32 to 32 : (Item'Length / 4 + 1) * 4 = Item'Length + 4
      Last : Natural;
   begin
      Do_Convert (
         Item,
         Input_Scheme,
         Output_Scheme,
         Output_BOM,
         Result,
         Last);
      return Result (1 .. Last);
   end Convert;

   function Convert (
      Item : UTF_String;
      Input_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False)
      return UTF_16_Wide_String
   is
      --  from 8 to 16 : (Item'Length + 1) * 2 = 2 * Item'Length + 2 (max)
      --  from 16 to 16 : (Item'Length / 2 + 1) * 2 = Item'Length + 2
      --  from 32 to 16 : (2 * Item'Length / 4 + 1) * 2 = Item'Length + 2
      Result : UTF_String (1 .. 2 * Item'Length + 2);
      for Result'Alignment use 16 / Standard'Storage_Unit;
      Last : Natural;
   begin
      Do_Convert (
         Item,
         Input_Scheme,
         UTF_16_Wide_String_Scheme,
         Output_BOM,
         Result,
         Last);
      return To_UTF_16_Wide_String (Result'Address, Last);
   end Convert;

   function Convert (
      Item : UTF_String;
      Input_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False)
      return UTF_32_Wide_Wide_String
   is
      --  from 8 to 32 : (Item'Length + 1) * 4 = 4 * Item'Length + 4 (max)
      --  from 16 to 32 : (Item'Length / 2 + 1) * 4 = 2 * Item'Length + 4
      --  from 32 to 32 : (Item'Length / 4 + 1) * 4 = Item'Length + 4
      Result : UTF_String (1 .. 4 * Item'Length + 4);
      for Result'Alignment use 32 / Standard'Storage_Unit;
      Last : Natural;
   begin
      Do_Convert (
         Item,
         Input_Scheme,
         UTF_32_Wide_Wide_String_Scheme,
         Output_BOM,
         Result,
         Last);
      return To_UTF_32_Wide_Wide_String (Result'Address, Last);
   end Convert;

   function Convert (
      Item : UTF_8_String;
      Output_BOM : Boolean := False)
      return UTF_16_Wide_String
   is
      Result : UTF_String (
         1 ..
         (Item'Length * System.UTF_Conversions.Expanding_From_8_To_16 + 1)
            * 2);
      for Result'Alignment use 16 / Standard'Storage_Unit;
      Last : Natural;
   begin
      --  it should be specialized version ?
      Do_Convert (
         Item,
         UTF_8,
         UTF_16_Wide_String_Scheme,
         Output_BOM,
         Result,
         Last);
      return To_UTF_16_Wide_String (Result'Address, Last);
   end Convert;

   function Convert (
      Item : UTF_8_String;
      Output_BOM : Boolean := False)
      return UTF_32_Wide_Wide_String
   is
      Result : UTF_String (
         1 ..
         (Item'Length * System.UTF_Conversions.Expanding_From_8_To_32 + 1)
            * 4);
      for Result'Alignment use 32 / Standard'Storage_Unit;
      Last : Natural;
   begin
      --  it should be specialized version ?
      Do_Convert (
         Item,
         UTF_8,
         UTF_32_Wide_Wide_String_Scheme,
         Output_BOM,
         Result,
         Last);
      return To_UTF_32_Wide_Wide_String (Result'Address, Last);
   end Convert;

   function Convert (
      Item : UTF_16_Wide_String;
      Output_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False)
      return UTF_String
   is
      Item_A : UTF_String (1 .. Item'Length * 2);
      for Item_A'Address use Item'Address;
      --  from 16 to 8 : 3 * Item'Length + 3
      --  from 16 to 16 : (Item'Length + 1) * 2 = 2 * Item'Length + 2
      --  from 16 to 32 : (Item'Length + 1) * 4 = 4 * Item'Length + 4 (max)
      Result : UTF_String (1 .. 4 * Item'Length + 4);
      Last : Natural;
   begin
      Do_Convert (
         Item_A,
         UTF_16_Wide_String_Scheme,
         Output_Scheme,
         Output_BOM,
         Result,
         Last);
      return Result (1 .. Last);
   end Convert;

   function Convert (
      Item : UTF_16_Wide_String;
      Output_BOM : Boolean := False)
      return UTF_8_String
   is
      Item_A : UTF_String (1 .. Item'Length * 2);
      for Item_A'Address use Item'Address;
      Result : UTF_String (
         1 ..
         Item'Length * System.UTF_Conversions.Expanding_From_16_To_8 + 3);
      Last : Natural;
   begin
      --  it should be specialized version ?
      Do_Convert (
         Item_A,
         UTF_16_Wide_String_Scheme,
         UTF_8,
         Output_BOM,
         Result,
         Last);
      return Result (1 .. Last);
   end Convert;

   function Convert (
      Item : UTF_16_Wide_String;
      Output_BOM : Boolean := False)
      return UTF_32_Wide_Wide_String
   is
      Item_A : UTF_String (1 .. Item'Length * 2);
      for Item_A'Address use Item'Address;
      Result : UTF_String (
         1 ..
         (Item'Length * System.UTF_Conversions.Expanding_From_16_To_32 + 1)
            * 4);
      for Result'Alignment use 32 / Standard'Storage_Unit;
      Last : Natural;
   begin
      --  it should be specialized version ?
      Do_Convert (
         Item_A,
         UTF_16_Wide_String_Scheme,
         UTF_32_Wide_Wide_String_Scheme,
         Output_BOM,
         Result,
         Last);
      return To_UTF_32_Wide_Wide_String (Result'Address, Last);
   end Convert;

   function Convert (
      Item : UTF_32_Wide_Wide_String;
      Output_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False)
      return UTF_String
   is
      Item_A : UTF_String (1 .. Item'Length * 4);
      for Item_A'Address use Item'Address;
      --  from 32 to 8 : 6 * Item'Length + 3 (max rate)
      --  from 32 to 16 : (2 * Item'Length + 1) * 2 = 4 * Item'Length + 2
      --  from 32 to 32 : (Item'Length + 1) * 4 = 4 * Item'Length + 4 (max BOM)
      Result : UTF_String (1 .. 6 * Item'Length + 4);
      Last : Natural;
   begin
      Do_Convert (
         Item_A,
         UTF_32_Wide_Wide_String_Scheme,
         Output_Scheme,
         Output_BOM,
         Result,
         Last);
      return Result (1 .. Last);
   end Convert;

   function Convert (
      Item : UTF_32_Wide_Wide_String;
      Output_BOM : Boolean := False)
      return UTF_8_String
   is
      Item_A : UTF_String (1 .. Item'Length * 4);
      for Item_A'Address use Item'Address;
      Result : UTF_String (
         1 ..
         Item'Length * System.UTF_Conversions.Expanding_From_32_To_8 + 3);
      Last : Natural;
   begin
      --  it should be specialized version ?
      Do_Convert (
         Item_A,
         UTF_32_Wide_Wide_String_Scheme,
         UTF_8,
         Output_BOM,
         Result,
         Last);
      return Result (1 .. Last);
   end Convert;

   function Convert (
      Item : UTF_32_Wide_Wide_String;
      Output_BOM : Boolean := False)
      return UTF_16_Wide_String
   is
      Item_A : UTF_String (1 .. Item'Length * 4);
      for Item_A'Address use Item'Address;
      Result : UTF_String (
         1 ..
         (Item'Length * System.UTF_Conversions.Expanding_From_32_To_16 + 1)
            * 2);
      for Result'Alignment use 16 / Standard'Storage_Unit;
      Last : Natural;
   begin
      --  it should be specialized version ?
      Do_Convert (
         Item_A,
         UTF_32_Wide_Wide_String_Scheme,
         UTF_16_Wide_String_Scheme,
         Output_BOM,
         Result,
         Last);
      return To_UTF_16_Wide_String (Result'Address, Last);
   end Convert;

end Ada.Strings.UTF_Encoding.Conversions;
