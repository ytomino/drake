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
      Status : out Error_Status)
   is
      Out_Item_2 : Ada.Streams.Stream_Element_Array (
         1 ..
         Out_Item'Length
            * (Character_Type'Size / Ada.Streams.Stream_Element'Size));
      for Out_Item_2'Address use Out_Item'Address;
      Out_Last_2 : Ada.Streams.Stream_Element_Offset;
   begin
      Convert (Object, Item, Last, Out_Item_2, Out_Last_2, Status);
      pragma Assert (Out_Last_2
         rem (Character_Type'Size / Ada.Streams.Stream_Element'Size) = 0);
      Out_Last := Out_Item'First
         + Natural (
            Out_Last_2
            / (Character_Type'Size / Ada.Streams.Stream_Element'Size))
         - 1;
   end Decode;

   procedure Decode (
      Object : Decoder;
      Item : Ada.Streams.Stream_Element_Array;
      Out_Item : out String_Type;
      Out_Last : out Natural)
   is
      Out_Item_2 : Ada.Streams.Stream_Element_Array (
         1 ..
         Out_Item'Length
            * (Character_Type'Size / Ada.Streams.Stream_Element'Size));
      for Out_Item_2'Address use Out_Item'Address;
      Out_Last_2 : Ada.Streams.Stream_Element_Offset;
   begin
      Convert (Object, Item, Out_Item_2, Out_Last_2);
      pragma Assert (Out_Last_2
         rem (Character_Type'Size / Ada.Streams.Stream_Element'Size) = 0);
      Out_Last := Out_Item'First
         + Natural (
            Out_Last_2
            / (Character_Type'Size / Ada.Streams.Stream_Element'Size))
         - 1;
   end Decode;

   function Decode (
      Object : Decoder;
      Item : Ada.Streams.Stream_Element_Array)
      return String_Type
   is
      Out_Item : aliased String_Type (
         1 ..
         Expanding * Item'Length);
      Out_Last : Natural;
   begin
      Decode (Object, Item, Out_Item, Out_Last);
      return Out_Item (Out_Item'First .. Out_Last);
   end Decode;

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
      Status : out Error_Status)
   is
      Item_2 : Ada.Streams.Stream_Element_Array (
         1 ..
         Item'Length
            * (Character_Type'Size / Ada.Streams.Stream_Element'Size));
      for Item_2'Address use Item'Address;
      Last_2 : Ada.Streams.Stream_Element_Offset;
   begin
      Convert (Object, Item_2, Last_2, Out_Item, Out_Last, Status);
      pragma Assert (Last_2
         rem (Character_Type'Size / Ada.Streams.Stream_Element'Size) = 0);
      Last := Item'First
         + Natural (
            Last_2
            / (Character_Type'Size / Ada.Streams.Stream_Element'Size))
         - 1;
   end Encode;

   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset)
   is
      Item_2 : Ada.Streams.Stream_Element_Array (
         1 ..
         Item'Length
            * (Character_Type'Size / Ada.Streams.Stream_Element'Size));
      for Item_2'Address use Item'Address;
   begin
      Convert (Object, Item_2, Out_Item, Out_Last);
   end Encode;

   function Encode (
      Object : Encoder;
      Item : String_Type)
      return Ada.Streams.Stream_Element_Array
   is
      Out_Item : aliased Ada.Streams.Stream_Element_Array (
         0 ..
         Expanding * Item'Length - 1);
      Out_Last : Ada.Streams.Stream_Element_Offset;
   begin
      Encode (Object, Item, Out_Item, Out_Last);
      return Out_Item (Out_Item'First .. Out_Last);
   end Encode;

end System.Native_Encoding.Generic_Strings;
