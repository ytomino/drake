with Ada.Exceptions;
with System.Address_To_Constant_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
with System.Storage_Elements;
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

   function Default_Substitute (Encoding : Encoding_Id)
      return Ada.Streams.Stream_Element_Array
   is
      Result : Ada.Streams.Stream_Element_Array (0 .. Expanding - 1);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Default_Substitute (Encoding, Result, Last);
      return Result (Result'First .. Last);
   end Default_Substitute;

   function Is_Open (Object : Converter) return Boolean is
      iconv : constant C.iconv.iconv_t := Value (Object);
   begin
      return Address (iconv) /= Null_Address;
   end Is_Open;

   function Substitute (Object : Converter)
      return Ada.Streams.Stream_Element_Array
   is
      S : constant not null access constant Substitute_Type :=
         Substitute_Reference (Object);
   begin
      return S.Element (1 .. S.Length);
   end Substitute;

   procedure Set_Substitute (
      Object : Converter;
      Substitute : Ada.Streams.Stream_Element_Array)
   is
      S : constant not null access Substitute_Type :=
         Substitute_Reference (Object);
   begin
      if Substitute'Length > S.Element'Length then
         raise Constraint_Error;
      end if;
      S.Length := Substitute'Length;
      S.Element (1 .. S.Length) := Substitute;
   end Set_Substitute;

   procedure Convert (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Error_Status) is
   begin
      if not Is_Open (Object) then
         Ada.Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Convert_No_Check (Object, Item, Last, Out_Item, Out_Last, Status);
   end Convert;

   procedure Convert (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      if not Is_Open (Object) then
         Ada.Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Convert_No_Check (Object, Item, Out_Item, Out_Last);
   end Convert;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Error_Status)
   is
      pragma Suppress (All_Checks);
      package C_Conv is
         new Address_To_Constant_Access_Conversions (C.char, C.char_const_ptr);
      package V_Conv is
         new Address_To_Named_Access_Conversions (C.char, C.char_ptr);
      iconv : constant C.iconv.iconv_t := Value (Object);
      Pointer : aliased C.char_const_ptr := C_Conv.To_Pointer (Item'Address);
      Size : aliased C.size_t := Item'Length;
      Out_Pointer : aliased C.char_ptr := V_Conv.To_Pointer (Out_Item'Address);
      Out_Size : aliased C.size_t := Out_Item'Length;
      errno : C.signed_int;
   begin
      if C.iconv.iconv (
         iconv,
         Pointer'Access,
         Size'Access,
         Out_Pointer'Access,
         Out_Size'Access) = C.size_t'Last
      then
         errno := C.errno.errno;
         case errno is
            when C.errno.E2BIG =>
               raise Constraint_Error;
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
      Item : Ada.Streams.Stream_Element_Array;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset)
   is
      Index : Ada.Streams.Stream_Element_Offset := Item'First;
      Out_Index : Ada.Streams.Stream_Element_Offset := Out_Item'First;
   begin
      loop
         declare
            Status : Error_Status;
            Last : Ada.Streams.Stream_Element_Offset;
         begin
            Convert_No_Check (
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
                  Put_Substitute (
                     Object,
                     Out_Item (Out_Index .. Out_Item'Last),
                     Out_Last);
                  Out_Index := Out_Last + 1;
                  Index := Index + 1;
            end case;
            exit when Index > Item'Last;
         end;
      end loop;
   end Convert_No_Check;

   procedure Put_Substitute (
      Object : Converter;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset)
   is
      S : constant not null access constant Substitute_Type :=
         Substitute_Reference (Object);
   begin
      if Out_Item'Length < S.Length then
         raise Constraint_Error;
      end if;
      Out_Last := Out_Item'First - 1 + S.Length;
      Out_Item (Out_Item'First .. Out_Last) := S.Element (1 .. S.Length);
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
         Object.iconv := iconv;
         Default_Substitute (
            To,
            Object.Substitute.Element,
            Object.Substitute.Length);
      end Open;

      function Value (Object : Converter) return C.iconv.iconv_t is
      begin
         return Object.iconv;
      end Value;

      function Substitute_Reference (Object : Converter)
         return not null access Substitute_Type is
      begin
         return Object.Substitute'Unrestricted_Access;
      end Substitute_Reference;

      overriding procedure Finalize (Object : in out Converter) is
      begin
         if C.iconv.iconv_close (Object.iconv) /= 0 then
            null; -- raise Status_Error;
         end if;
      end Finalize;

   end Controlled;

end System.Native_Encoding;
