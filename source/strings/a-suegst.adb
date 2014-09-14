with Ada.Exception_Identification.From_Here;
with Ada.Strings.UTF_Encoding.Conversions;
with System.UTF_Conversions;
package body Ada.Strings.UTF_Encoding.Generic_Strings is
   use Exception_Identification.From_Here;
   use type System.UTF_Conversions.From_Status_Type;

   generic
      type UTF_Character_Type is (<>);
      type UTF_String_Type is array (Positive range <>) of UTF_Character_Type;
      BOM : UTF_String_Type;
      with procedure To_UTF (
         Code : System.UTF_Conversions.UCS_4;
         Result : out UTF_String_Type;
         Last : out Natural;
         Status : out System.UTF_Conversions.To_Status_Type);
   function Generic_Encode (
      Item : String_Type;
      Output_BOM : Boolean := False)
      return UTF_String_Type;

   function Generic_Encode (
      Item : String_Type;
      Output_BOM : Boolean := False)
      return UTF_String_Type
   is
      --  Expanding_From_N is not static because formal object of generic
      pragma Compile_Time_Error (
         UTF_Character_Type'Size /= 8
         and then UTF_Character_Type'Size /= 16
         and then UTF_Character_Type'Size /= 32,
         "bad UTF_Character_Type'Size");
      Expanding : Positive;
   begin
      if UTF_Character_Type'Size = 8 then
         Expanding := Expanding_To_8;
      elsif UTF_Character_Type'Size = 16 then
         Expanding := Expanding_To_16;
      else
         Expanding := Expanding_To_32;
      end if;
      declare
         Item_Last : Natural := Item'First - 1;
         Result : UTF_String_Type (1 .. BOM'Length + Item'Length * Expanding);
         Result_Last : Natural := Result'First - 1;
      begin
         if Output_BOM then
            Result (Result_Last + 1 .. Result_Last + BOM'Length) := BOM;
            Result_Last := Result_Last + BOM'Length;
         end if;
         while Item_Last < Item'Last loop
            declare
               Code : Wide_Wide_Character;
               Is_Illegal_Sequence : Boolean;
               To_Status : System.UTF_Conversions.To_Status_Type; -- ignore
            begin
               Get (
                  Item (Item_Last + 1 .. Item'Last),
                  Item_Last,
                  Code,
                  Is_Illegal_Sequence);
               if Is_Illegal_Sequence then
                  Raise_Exception (Encoding_Error'Identity);
               end if;
               To_UTF (
                  Wide_Wide_Character'Pos (Code),
                  Result (Result_Last + 1 .. Result'Last),
                  Result_Last,
                  To_Status);
            end;
         end loop;
         return Result (1 .. Result_Last);
      end;
   end Generic_Encode;

   function Encode_To_8 is
      new Generic_Encode (
         Character,
         UTF_8_String,
         BOM_8,
         System.UTF_Conversions.To_UTF_8);

   function Encode_To_16 is
      new Generic_Encode (
         Wide_Character,
         UTF_16_Wide_String,
         BOM_16,
         System.UTF_Conversions.To_UTF_16);

   function Encode_To_32 is
      new Generic_Encode (
         Wide_Wide_Character,
         UTF_32_Wide_Wide_String,
         BOM_32,
         System.UTF_Conversions.To_UTF_32);

   generic
      type UTF_Character_Type is (<>);
      type UTF_String_Type is array (Positive range <>) of UTF_Character_Type;
      BOM : UTF_String_Type;
      with procedure From_UTF (
         Data : UTF_String_Type;
         Last : out Natural;
         Result : out System.UTF_Conversions.UCS_4;
         Status : out System.UTF_Conversions.From_Status_Type);
   function Generic_Decode (Item : UTF_String_Type) return String_Type;

   function Generic_Decode (Item : UTF_String_Type) return String_Type is
      --  Expanding_To_N is not static because formal object of generic
      pragma Compile_Time_Error (
         UTF_Character_Type'Size /= 8
         and then UTF_Character_Type'Size /= 16
         and then UTF_Character_Type'Size /= 32,
         "bad UTF_Character_Type'Size");
      Expanding : Positive;
   begin
      if UTF_Character_Type'Size = 8 then
         Expanding := Expanding_From_8;
      elsif UTF_Character_Type'Size = 16 then
         Expanding := Expanding_From_16;
      else
         Expanding := Expanding_From_32;
      end if;
      declare
         Item_Last : Natural := Item'First - 1;
         Result : String_Type (1 .. Item'Length * Expanding);
         Result_Last : Natural := Result'First - 1;
      begin
         if Item (Item_Last + 1 .. Item_Last + BOM'Length) = BOM then
            Item_Last := Item_Last + BOM'Length;
         end if;
         while Item_Last < Item'Last loop
            declare
               Code : System.UTF_Conversions.UCS_4;
               From_Status : System.UTF_Conversions.From_Status_Type;
            begin
               From_UTF (
                  Item (Item_Last + 1 .. Item'Last),
                  Item_Last,
                  Code,
                  From_Status);
               if From_Status /= System.UTF_Conversions.Success then
                  Raise_Exception (Encoding_Error'Identity);
               end if;
               Put (
                  Wide_Wide_Character'Val (Code),
                  Result (Result_Last + 1 .. Result'Last),
                  Result_Last);
            end;
         end loop;
         return Result (1 .. Result_Last);
      end;
   end Generic_Decode;

   function Decode_From_8 is
      new Generic_Decode (
         Character,
         UTF_8_String,
         BOM_8,
         System.UTF_Conversions.From_UTF_8);

   function Decode_From_16 is
      new Generic_Decode (
         Wide_Character,
         UTF_16_Wide_String,
         BOM_16,
         System.UTF_Conversions.From_UTF_16);

   function Decode_From_32 is
      new Generic_Decode (
         Wide_Wide_Character,
         UTF_32_Wide_Wide_String,
         BOM_32,
         System.UTF_Conversions.From_UTF_32);

   --  implementation

   function Encode (
      Item : String_Type;
      Output_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False)
      return UTF_String is
   begin
      return Conversions.Convert (
         Encode_To_32 (Item, Output_BOM => False),
         Output_Scheme,
         Output_BOM);
   end Encode;

   function Encode (
      Item : String_Type;
      Output_BOM : Boolean := False)
      return UTF_8_String
      renames Encode_To_8;

   function Encode (
      Item : String_Type;
      Output_BOM : Boolean := False)
      return UTF_16_Wide_String
      renames Encode_To_16;

   function Encode (
      Item : String_Type;
      Output_BOM : Boolean := False)
      return UTF_32_Wide_Wide_String
      renames Encode_To_32;

   function Decode (
      Item : UTF_String;
      Input_Scheme : Encoding_Scheme)
      return String_Type is
   begin
      return Decode_From_32 (
         Conversions.Convert (Item, Input_Scheme, Output_BOM => False));
   end Decode;

   function Decode (Item : UTF_8_String) return String_Type
      renames Decode_From_8;

   function Decode (Item : UTF_16_Wide_String) return String_Type
      renames Decode_From_16;

   function Decode (Item : UTF_32_Wide_Wide_String) return String_Type
      renames Decode_From_32;

end Ada.Strings.UTF_Encoding.Generic_Strings;
