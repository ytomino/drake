with Ada.Exceptions.Finally;
with Ada.Unchecked_Deallocation;
package body Ada.Environment_Encoding.Generic_Strings is
   use type Streams.Stream_Element_Offset;

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

   type String_Type_Access is access String_Type;
   procedure Free is
      new Unchecked_Deallocation (
         String_Type,
         String_Type_Access);

   procedure Expand (
      Item : in out String_Type_Access;
      Last : Natural);
   procedure Expand (
      Item : in out String_Type_Access;
      Last : Natural)
   is
      New_Item : constant String_Type_Access :=
         new String_Type (
            Item'First ..
            Item'First + 2 * Item'Length - 1);
   begin
      New_Item (Item'First .. Last) := Item.all (Item'First .. Last);
      Free (Item);
      Item := New_Item;
   end Expand;

   type Stream_Element_Array_Access is access Streams.Stream_Element_Array;
   procedure Free is
      new Unchecked_Deallocation (
         Streams.Stream_Element_Array,
         Stream_Element_Array_Access);

   procedure Expand (
      Item : in out Stream_Element_Array_Access;
      Last : Streams.Stream_Element_Offset);
   procedure Expand (
      Item : in out Stream_Element_Array_Access;
      Last : Streams.Stream_Element_Offset)
   is
      New_Item : constant Stream_Element_Array_Access :=
         new Streams.Stream_Element_Array (
            Item'First ..
            Item'First + 2 * Item'Length - 1);
   begin
      New_Item (Item'First .. Last) := Item.all (Item'First .. Last);
      Free (Item);
      Item := New_Item;
   end Expand;

   --  implementation of decoder

   function From (Id : Encoding_Id) return Decoder is
   begin
      return Result : Decoder do
         Open (
            Converter (Result),
            From => System.Native_Encoding.Encoding_Id (Id),
            To => System.Native_Encoding.Encoding_Id (Current_Id));
      end return;
   end From;

   procedure Decode (
      Object : Decoder;
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Finish : Boolean;
      Status : out Subsequence_Status_Type)
   is
      CS_In_SE : constant Streams.Stream_Element_Count :=
         Character_Type'Size / Streams.Stream_Element'Size;
      Out_Item_2 : Streams.Stream_Element_Array (
         1 ..
         Out_Item'Length * CS_In_SE);
      for Out_Item_2'Address use Out_Item'Address;
      Out_Last_2 : Streams.Stream_Element_Offset;
   begin
      Convert (Object, Item, Last, Out_Item_2, Out_Last_2, Finish, Status);
      pragma Assert (Out_Last_2 rem CS_In_SE = 0);
      Out_Last := Out_Item'First + Natural (Out_Last_2 / CS_In_SE) - 1;
   end Decode;

   procedure Decode (
      Object : Decoder;
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Status : out Continuing_Status_Type)
   is
      CS_In_SE : constant Streams.Stream_Element_Count :=
         Character_Type'Size / Streams.Stream_Element'Size;
      Out_Item_2 : Streams.Stream_Element_Array (
         1 ..
         Out_Item'Length * CS_In_SE);
      for Out_Item_2'Address use Out_Item'Address;
      Out_Last_2 : Streams.Stream_Element_Offset;
   begin
      Convert (Object, Item, Last, Out_Item_2, Out_Last_2, Status);
      pragma Assert (Out_Last_2 rem CS_In_SE = 0);
      Out_Last := Out_Item'First + Natural (Out_Last_2 / CS_In_SE) - 1;
   end Decode;

   procedure Decode (
      Object : Decoder;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Finish : True_Only;
      Status : out Finishing_Status_Type)
   is
      CS_In_SE : constant Streams.Stream_Element_Count :=
         Character_Type'Size / Streams.Stream_Element'Size;
      Out_Item_2 : Streams.Stream_Element_Array (
         1 ..
         Out_Item'Length * CS_In_SE);
      for Out_Item_2'Address use Out_Item'Address;
      Out_Last_2 : Streams.Stream_Element_Offset;
   begin
      Convert (Object, Out_Item_2, Out_Last_2, Finish, Status);
      pragma Assert (Out_Last_2 rem CS_In_SE = 0);
      Out_Last := Out_Item'First + Natural (Out_Last_2 / CS_In_SE) - 1;
   end Decode;

   procedure Decode (
      Object : Decoder;
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Finish : True_Only;
      Status : out Status_Type)
   is
      CS_In_SE : constant Streams.Stream_Element_Count :=
         Character_Type'Size / Streams.Stream_Element'Size;
      Out_Item_2 : Streams.Stream_Element_Array (
         1 ..
         Out_Item'Length * CS_In_SE);
      for Out_Item_2'Address use Out_Item'Address;
      Out_Last_2 : Streams.Stream_Element_Offset;
   begin
      Convert (Object, Item, Last, Out_Item_2, Out_Last_2, Finish, Status);
      pragma Assert (Out_Last_2 rem CS_In_SE = 0);
      Out_Last := Out_Item'First + Natural (Out_Last_2 / CS_In_SE) - 1;
   end Decode;

   procedure Decode (
      Object : Decoder;
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Finish : True_Only;
      Status : out Substituting_Status_Type)
   is
      CS_In_SE : constant Streams.Stream_Element_Count :=
         Character_Type'Size / Streams.Stream_Element'Size;
      Out_Item_2 : Streams.Stream_Element_Array (
         1 ..
         Out_Item'Length * CS_In_SE);
      for Out_Item_2'Address use Out_Item'Address;
      Out_Last_2 : Streams.Stream_Element_Offset;
   begin
      Convert (Object, Item, Last, Out_Item_2, Out_Last_2, Finish, Status);
      pragma Assert (Out_Last_2 rem CS_In_SE = 0);
      Out_Last := Out_Item'First + Natural (Out_Last_2 / CS_In_SE) - 1;
   end Decode;

   function Decode (
      Object : Decoder;
      Item : Streams.Stream_Element_Array)
      return String_Type
   is
      procedure Finally (X : not null access String_Type_Access);
      procedure Finally (X : not null access String_Type_Access) is
      begin
         Free (X.all);
      end Finally;
      package Holder is
         new Exceptions.Finally.Scoped_Holder (
            String_Type_Access,
            Finally);
      CS_In_SE : constant Streams.Stream_Element_Count :=
         Character_Type'Size / Streams.Stream_Element'Size;
      Last : Streams.Stream_Element_Offset := Item'First - 1;
      Out_Item : aliased String_Type_Access;
      Out_Last : Natural;
      Status : Substituting_Status_Type;
   begin
      Holder.Assign (Out_Item'Access);
      Out_Item := new String_Type (
         1 ..
         2 * Item'Length / Integer (CS_In_SE));
      Out_Last := 0;
      loop
         Decode (
            Object,
            Item (Last + 1 .. Item'Last),
            Last,
            Out_Item.all (Out_Last + 1 .. Out_Item'Last),
            Out_Last,
            Finish => True,
            Status => Status);
         case Status is
            when Finished =>
               exit;
            when Success =>
               null;
            when Overflow =>
               Expand (Out_Item, Out_Last);
         end case;
      end loop;
      return Out_Item (Out_Item'First .. Out_Last);
   end Decode;

   --  implementation of encoder

   function To (Id : Encoding_Id) return Encoder is
   begin
      return Result : Encoder do
         Open (
            Converter (Result),
            From => System.Native_Encoding.Encoding_Id (Current_Id),
            To => System.Native_Encoding.Encoding_Id (Id));
      end return;
   end To;

   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Last : out Natural;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : Boolean;
      Status : out Subsequence_Status_Type)
   is
      CS_In_SE : constant Streams.Stream_Element_Count :=
         Character_Type'Size / Streams.Stream_Element'Size;
      Item_2 : Streams.Stream_Element_Array (
         1 ..
         Item'Length * CS_In_SE);
      for Item_2'Address use Item'Address;
      Last_2 : Streams.Stream_Element_Offset;
   begin
      Convert (Object, Item_2, Last_2, Out_Item, Out_Last, Finish, Status);
      pragma Assert (Last_2 rem CS_In_SE = 0);
      Last := Item'First + Natural (Last_2 / CS_In_SE) - 1;
   end Encode;

   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Last : out Natural;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Status : out Continuing_Status_Type)
   is
      CS_In_SE : constant Streams.Stream_Element_Count :=
         Character_Type'Size / Streams.Stream_Element'Size;
      Item_2 : Streams.Stream_Element_Array (
         1 ..
         Item'Length * CS_In_SE);
      for Item_2'Address use Item'Address;
      Last_2 : Streams.Stream_Element_Offset;
   begin
      Convert (Object, Item_2, Last_2, Out_Item, Out_Last, Status);
      pragma Assert (Last_2 rem CS_In_SE = 0);
      Last := Item'First + Natural (Last_2 / CS_In_SE) - 1;
   end Encode;

   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Last : out Natural;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Status_Type)
   is
      CS_In_SE : constant Streams.Stream_Element_Count :=
         Character_Type'Size / Streams.Stream_Element'Size;
      Item_2 : Streams.Stream_Element_Array (
         1 ..
         Item'Length * CS_In_SE);
      for Item_2'Address use Item'Address;
      Last_2 : Streams.Stream_Element_Offset;
   begin
      Convert (Object, Item_2, Last_2, Out_Item, Out_Last, Finish, Status);
      pragma Assert (Last_2 rem CS_In_SE = 0);
      Last := Item'First + Natural (Last_2 / CS_In_SE) - 1;
   end Encode;

   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Last : out Natural;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Substituting_Status_Type)
   is
      CS_In_SE : constant Streams.Stream_Element_Count :=
         Character_Type'Size / Streams.Stream_Element'Size;
      Item_2 : Streams.Stream_Element_Array (
         1 ..
         Item'Length * CS_In_SE);
      for Item_2'Address use Item'Address;
      Last_2 : Streams.Stream_Element_Offset;
   begin
      Convert (Object, Item_2, Last_2, Out_Item, Out_Last, Finish, Status);
      pragma Assert (Last_2 rem CS_In_SE = 0);
      Last := Item'First + Natural (Last_2 / CS_In_SE) - 1;
   end Encode;

   function Encode (
      Object : Encoder;
      Item : String_Type)
      return Streams.Stream_Element_Array
   is
      procedure Finally (X : not null access Stream_Element_Array_Access);
      procedure Finally (X : not null access Stream_Element_Array_Access) is
      begin
         Free (X.all);
      end Finally;
      package Holder is
         new Exceptions.Finally.Scoped_Holder (
            Stream_Element_Array_Access,
            Finally);
      CS_In_SE : constant Streams.Stream_Element_Count :=
         Character_Type'Size / Streams.Stream_Element'Size;
      Last : Natural := Item'First - 1;
      Out_Item : aliased Stream_Element_Array_Access;
      Out_Last : Streams.Stream_Element_Offset;
      Status : Substituting_Status_Type;
   begin
      Holder.Assign (Out_Item'Access);
      Out_Item := new Streams.Stream_Element_Array (
         0 ..
         2 * Item'Length * CS_In_SE - 1);
      Out_Last := -1;
      loop
         Encode (
            Object,
            Item (Last + 1 .. Item'Last),
            Last,
            Out_Item.all (Out_Last + 1 .. Out_Item'Last),
            Out_Last,
            Finish => True,
            Status => Status);
         case Status is
            when Finished =>
               exit;
            when Success =>
               null;
            when Overflow =>
               Expand (Out_Item, Out_Last);
         end case;
      end loop;
      return Out_Item (Out_Item'First .. Out_Last);
   end Encode;

end Ada.Environment_Encoding.Generic_Strings;
