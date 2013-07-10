with Ada.Exceptions;
package body System.Native_Encoding.Generic_Strings is
   use type Ada.Streams.Stream_Element_Offset;

   function Current_Id return Encoding_Id;
   function Current_Id return Encoding_Id is
   begin
      if Character_Type'Size = 8 then
         return UTF_8;
      elsif Character_Type'Size = 16 then
         return UTF_16;
      elsif Character_Type'Size = 32 then
         return UTF_32;
      else
         raise Program_Error; -- bad instance
      end if;
   end Current_Id;

   --  implementation of decoder

   function From (Id : Encoding_Id) return Decoder is
   begin
      return Result : Decoder do
         Open (Converter (Result), From => Id, To => Current_Id);
      end return;
   end From;

   procedure Decode (
      Object : Decoder;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Status : out Error_Status) is
   begin
      if not Is_Open (Object) then
         Ada.Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Decode_No_Check (Object, Item, Last, Out_Item, Out_Last, Status);
   end Decode;

   procedure Decode (
      Object : Decoder;
      Item : Ada.Streams.Stream_Element_Array;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Substitute : Character_Type :=
         Character_Type'Val (Default_Substitute)) is
   begin
      if not Is_Open (Object) then
         Ada.Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      declare
         Index : Ada.Streams.Stream_Element_Offset := Item'First;
         Out_Index : Natural := Out_Item'First;
      begin
         loop
            declare
               Status : Error_Status;
               Last : Ada.Streams.Stream_Element_Offset;
            begin
               Decode_No_Check (
                  Object,
                  Item (Index .. Item'Last),
                  Last,
                  Out_Item (Out_Index .. Out_Item'Last),
                  Out_Last,
                  Status);
               Index := Last + 1;
               Out_Index := Out_Last + 1;
               case Status is
                  when Fine =>
                     null;
                  when Incomplete | Illegal_Sequence =>
                     Out_Item (Out_Index) := Substitute;
                     Out_Index := Out_Index + 1;
                     Index := Index + 1;
               end case;
               exit when Index > Item'Last;
            end;
         end loop;
      end;
   end Decode;

   function Decode (
      Object : Decoder;
      Item : Ada.Streams.Stream_Element_Array;
      Substitute : Character_Type := Character_Type'Val (Default_Substitute))
      return String_Type
   is
      Out_Item : aliased String_Type (
         1 ..
         Expanding * Item'Length);
      Out_Last : Natural;
   begin
      Decode (Object, Item, Out_Item, Out_Last, Substitute => Substitute);
      return Out_Item (Out_Item'First .. Out_Last);
   end Decode;

   procedure Decode_No_Check (
      Object : Decoder;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Status : out Error_Status)
   is
      Out_Item_2 : Ada.Streams.Stream_Element_Array (
         1 ..
         Out_Item'Length
            * (Character_Type'Size / Ada.Streams.Stream_Element'Size));
      for Out_Item_2'Address use Out_Item'Address;
      Out_Last_2 : Ada.Streams.Stream_Element_Offset;
   begin
      Convert_No_Check (
         Converter (Object),
         Item,
         Last,
         Out_Item_2,
         Out_Last_2,
         Status);
      pragma Assert (Out_Last_2
         rem (Character_Type'Size / Ada.Streams.Stream_Element'Size) = 0);
      Out_Last := Out_Item'First
         + Natural (
            Out_Last_2
            / (Character_Type'Size / Ada.Streams.Stream_Element'Size))
         - 1;
   end Decode_No_Check;

   --  implementation of encoder

   function To (Id : Encoding_Id) return Encoder is
   begin
      return Result : Encoder do
         Open (Converter (Result), From => Current_Id, To => Id);
      end return;
   end To;

   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Last : out Natural;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Error_Status) is
   begin
      if not Is_Open (Object) then
         Ada.Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Encode_No_Check (Object, Item, Last, Out_Item, Out_Last, Status);
   end Encode;

   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Substitute : Ada.Streams.Stream_Element := Default_Substitute) is
   begin
      if not Is_Open (Object) then
         Ada.Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      declare
         Index : Natural := Item'First;
         Out_Index : Ada.Streams.Stream_Element_Offset := Out_Item'First;
      begin
         loop
            declare
               Status : Error_Status;
               Last : Natural;
            begin
               Encode_No_Check (
                  Object,
                  Item (Index .. Item'Last),
                  Last,
                  Out_Item (Out_Index .. Out_Item'Last),
                  Out_Last,
                  Status);
               Index := Last + 1;
               Out_Index := Out_Last + 1;
               case Status is
                  when Fine =>
                     null;
                  when Incomplete | Illegal_Sequence =>
                     Out_Item (Out_Index) := Substitute;
                     Out_Index := Out_Index + 1;
                     Index := Index + 1;
               end case;
               exit when Index > Item'Last;
            end;
         end loop;
      end;
   end Encode;

   function Encode (
      Object : Encoder;
      Item : String_Type;
      Substitute : Ada.Streams.Stream_Element := Default_Substitute)
      return Ada.Streams.Stream_Element_Array
   is
      Out_Item : aliased Ada.Streams.Stream_Element_Array (
         0 ..
         Expanding * Item'Length - 1);
      Out_Last : Ada.Streams.Stream_Element_Offset;
   begin
      Encode (Object, Item, Out_Item, Out_Last, Substitute => Substitute);
      return Out_Item (Out_Item'First .. Out_Last);
   end Encode;

   procedure Encode_No_Check (
      Object : Encoder;
      Item : String_Type;
      Last : out Natural;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Error_Status)
   is
      Item_2 : Ada.Streams.Stream_Element_Array (
         1 ..
         Item'Length
            * (Character_Type'Size / Ada.Streams.Stream_Element'Size));
      for Item_2'Address use Item'Address;
      Last_2 : Ada.Streams.Stream_Element_Offset;
   begin
      Convert_No_Check (
         Converter (Object),
         Item_2,
         Last_2,
         Out_Item,
         Out_Last,
         Status);
      pragma Assert (Last_2
         rem (Character_Type'Size / Ada.Streams.Stream_Element'Size) = 0);
      Last := Item'First
         + Natural (
            Last_2
            / (Character_Type'Size / Ada.Streams.Stream_Element'Size))
         - 1;
   end Encode_No_Check;

end System.Native_Encoding.Generic_Strings;
