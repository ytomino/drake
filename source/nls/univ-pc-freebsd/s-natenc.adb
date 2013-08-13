with Ada.Exceptions;
with System.Address_To_Constant_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
with C.errno;
package body System.Native_Encoding is
   use type C.signed_int;
   use type C.size_t;

   procedure Default_Substitute (
      Encoding : Encoding_Id;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   procedure Default_Substitute (
      Encoding : Encoding_Id;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      if Encoding = UTF_16_Names (High_Order_First)(0)'Access then
         Last := Item'First;
         Item (Last) := 0;
         Last := Last + 1;
         Item (Last) := Character'Pos ('?');
      elsif Encoding = UTF_16_Names (Low_Order_First)(0)'Access then
         Last := Item'First;
         Item (Last) := Character'Pos ('?');
         Last := Last + 1;
         Item (Last) := 0;
      elsif Encoding = UTF_32_Names (High_Order_First)(0)'Access then
         Last := Item'First;
         Item (Last) := 0;
         Last := Last + 1;
         Item (Last) := 0;
         Last := Last + 1;
         Item (Last) := 0;
         Last := Last + 1;
         Item (Last) := Character'Pos ('?');
      elsif Encoding = UTF_32_Names (Low_Order_First)(0)'Access then
         Last := Item'First;
         Item (Last) := Character'Pos ('?');
         Last := Last + 1;
         Item (Last) := 0;
         Last := Last + 1;
         Item (Last) := 0;
         Last := Last + 1;
         Item (Last) := 0;
      else
         Last := Item'First;
         Item (Last) := Character'Pos ('?');
      end if;
   end Default_Substitute;

   --  implementation

   function Image (Encoding : Encoding_Id) return String is
      package char_const_ptr_Conv is
         new Address_To_Constant_Access_Conversions (
            C.char,
            Encoding_Id);
   begin
      return Zero_Terminated_Strings.Value (
         char_const_ptr_Conv.To_Address (Encoding));
   end Image;

   function Default_Substitute (Encoding : Encoding_Id)
      return Ada.Streams.Stream_Element_Array
   is
      Result : Ada.Streams.Stream_Element_Array (
         0 .. -- from 0 for a result value
         Max_Substitute_Length - 1);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Default_Substitute (Encoding, Result, Last);
      return Result (Result'First .. Last);
   end Default_Substitute;

   function Min_Size_In_Stream_Elements (Encoding : Encoding_Id)
      return Ada.Streams.Stream_Element_Offset is
   begin
      if Encoding = UTF_16_Names (High_Order_First)(0)'Access
         or else Encoding = UTF_16_Names (Low_Order_First)(0)'Access
      then
         return 2;
      elsif Encoding = UTF_32_Names (High_Order_First)(0)'Access
         or else Encoding = UTF_32_Names (Low_Order_First)(0)'Access
      then
         return 4;
      else
         return 1;
      end if;
   end Min_Size_In_Stream_Elements;

   function Is_Open (Object : Converter) return Boolean is
      NC_Converter : constant not null access Non_Controlled_Converter :=
         Reference (Object);
   begin
      return Address (NC_Converter.iconv) /= Null_Address;
   end Is_Open;

   function Min_Size_In_From_Stream_Elements (Object : Converter)
      return Ada.Streams.Stream_Element_Offset
   is
      NC_Converter : constant not null access Non_Controlled_Converter :=
         Reference (Object);
   begin
      return NC_Converter.Min_Size_In_From_Stream_Elements;
   end Min_Size_In_From_Stream_Elements;

   function Substitute (Object : Converter)
      return Ada.Streams.Stream_Element_Array
   is
      NC_Converter : constant not null access Non_Controlled_Converter :=
         Reference (Object);
   begin
      return NC_Converter.Substitute (1 .. NC_Converter.Substitute_Length);
   end Substitute;

   procedure Set_Substitute (
      Object : in out Converter;
      Substitute : Ada.Streams.Stream_Element_Array)
   is
      NC_Converter : constant not null access Non_Controlled_Converter :=
         Reference (Object);
   begin
      if Substitute'Length > NC_Converter.Substitute'Length then
         raise Constraint_Error;
      end if;
      NC_Converter.Substitute_Length := Substitute'Length;
      NC_Converter.Substitute (1 .. NC_Converter.Substitute_Length) :=
         Substitute;
   end Set_Substitute;

   procedure Convert (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Status_Type) is
   begin
      if not Is_Open (Object) then
         Ada.Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Convert_No_Check (Object, Item, Last, Out_Item, Out_Last, Status);
   end Convert;

   procedure Convert (
      Object : Converter;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Finishing_Status_Type) is
   begin
      if not Is_Open (Object) then
         Ada.Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Convert_No_Check (Object, Out_Item, Out_Last, Status);
   end Convert;

   procedure Convert (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Substituting_Status_Type) is
   begin
      if not Is_Open (Object) then
         Ada.Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Convert_No_Check (Object, Item, Last, Out_Item, Out_Last, Status);
   end Convert;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Status_Type)
   is
      pragma Suppress (All_Checks);
      package C_Conv is
         new Address_To_Constant_Access_Conversions (C.char, C.char_const_ptr);
      package V_Conv is
         new Address_To_Named_Access_Conversions (C.char, C.char_ptr);
      NC_Converter : constant not null access Non_Controlled_Converter :=
         Reference (Object);
      Pointer : aliased C.char_const_ptr := C_Conv.To_Pointer (Item'Address);
      Size : aliased C.size_t := Item'Length;
      Out_Pointer : aliased C.char_ptr := V_Conv.To_Pointer (Out_Item'Address);
      Out_Size : aliased C.size_t := Out_Item'Length;
      errno : C.signed_int;
   begin
      if C.iconv.iconv (
         NC_Converter.iconv,
         Pointer'Access,
         Size'Access,
         Out_Pointer'Access,
         Out_Size'Access) = C.size_t'Last
      then
         errno := C.errno.errno;
         case errno is
            when C.errno.E2BIG =>
               Status := Insufficient;
            when C.errno.EINVAL =>
               Status := Incomplete;
            when others => -- C.errno.EILSEQ =>
               Status := Illegal_Sequence;
         end case;
      else
         Status := Fine;
      end if;
      Last := Item'First
         + (Item'Length - Ada.Streams.Stream_Element_Offset (Size))
         - 1;
      Out_Last := Out_Item'First
         + (Out_Item'Length - Ada.Streams.Stream_Element_Offset (Out_Size))
         - 1;
   end Convert_No_Check;

   procedure Convert_No_Check (
      Object : Converter;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Finishing_Status_Type)
   is
      pragma Suppress (All_Checks);
      package V_Conv is
         new Address_To_Named_Access_Conversions (C.char, C.char_ptr);
      NC_Converter : constant not null access Non_Controlled_Converter :=
         Reference (Object);
      Out_Pointer : aliased C.char_ptr := V_Conv.To_Pointer (Out_Item'Address);
      Out_Size : aliased C.size_t := Out_Item'Length;
      errno : C.signed_int;
   begin
      if C.iconv.iconv (
         NC_Converter.iconv,
         null,
         null,
         Out_Pointer'Access,
         Out_Size'Access) = C.size_t'Last
      then
         errno := C.errno.errno;
         case errno is
            when C.errno.E2BIG =>
               Status := Insufficient;
            when others => -- unknown
               Ada.Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
         end case;
      else
         Status := Fine;
      end if;
      Out_Last := Out_Item'First
         + (Out_Item'Length - Ada.Streams.Stream_Element_Offset (Out_Size))
         - 1;
   end Convert_No_Check;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Substituting_Status_Type)
   is
      NC_Converter : constant not null access Non_Controlled_Converter :=
         Reference (Object);
   begin
      Last := Item'First - 1;
      Out_Last := Out_Item'First - 1;
      while Last /= Item'Last loop
         declare
            Step_Status : Status_Type;
         begin
            Convert_No_Check (
               Object,
               Item (Last + 1 .. Item'Last),
               Last,
               Out_Item (Out_Last + 1 .. Out_Item'Last),
               Out_Last,
               Step_Status);
            case Step_Status is
               when Fine =>
                  null;
               when Insufficient =>
                  Status := Insufficient;
                  return;
               when Incomplete | Illegal_Sequence =>
                  declare
                     Is_Overflow : Boolean;
                  begin
                     Put_Substitute (
                        Object,
                        Out_Item (Out_Last + 1 .. Out_Item'Last),
                        Out_Last,
                        Is_Overflow);
                     if Is_Overflow then
                        Status := Insufficient;
                        return; -- wait a next try
                     end if;
                  end;
                  declare
                     New_Last : Ada.Streams.Stream_Element_Offset :=
                        Last + NC_Converter.Min_Size_In_From_Stream_Elements;
                  begin
                     if New_Last > Item'Last
                        or else New_Last < Last -- overflow
                     then
                        New_Last := Item'Last;
                     end if;
                     Last := New_Last;
                  end;
            end case;
         end;
      end loop;
      --  receive remaindered sequence
      declare
         Finishing_Status : Finishing_Status_Type;
      begin
         Convert_No_Check (
            Object,
            Out_Item (Out_Last + 1 .. Out_Item'Last),
            Out_Last,
            Finishing_Status);
         case Finishing_Status is
            when Fine =>
               null;
            when Insufficient =>
               Status := Insufficient;
               return;
         end case;
      end;
      Status := Fine;
   end Convert_No_Check;

   procedure Put_Substitute (
      Object : Converter;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Is_Overflow : out Boolean)
   is
      NC_Converter : constant not null access Non_Controlled_Converter :=
         Reference (Object);
   begin
      Out_Last := Out_Item'First - 1;
      Is_Overflow := Out_Item'Length < NC_Converter.Substitute_Length;
      if Is_Overflow then
         return;
      end if;
      Out_Last := Out_Last + NC_Converter.Substitute_Length;
      Out_Item (Out_Item'First .. Out_Last) :=
         NC_Converter.Substitute (1 .. NC_Converter.Substitute_Length);
   end Put_Substitute;

   package body Controlled is

      procedure Open (Object : out Converter; From, To : Encoding_Id) is
         Error : constant C.iconv.iconv_t := C.iconv.iconv_t (
            Storage_Elements.To_Address (
               Storage_Elements.Integer_Address'Mod (-1)));
         iconv : C.iconv.iconv_t;
      begin
         iconv := C.iconv.iconv_open (To, From);
         if Address (iconv) = Address (Error) then
            Ada.Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
         end if;
         Object.Data.iconv := iconv;
         --  about "From"
         Object.Data.Min_Size_In_From_Stream_Elements :=
            Min_Size_In_Stream_Elements (From);
         --  about "To"
         Default_Substitute (
            To,
            Object.Data.Substitute,
            Object.Data.Substitute_Length);
      end Open;

      function Reference (Object : Converter)
         return not null access Non_Controlled_Converter is
      begin
         return Object.Data'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out Converter) is
      begin
         if C.iconv.iconv_close (Object.Data.iconv) /= 0 then
            null; -- raise Status_Error;
         end if;
      end Finalize;

   end Controlled;

end System.Native_Encoding;
