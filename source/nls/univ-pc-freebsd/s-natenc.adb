with Ada.Exceptions;
with System.Address_To_Constant_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
with System.Storage_Elements;
package body System.Native_Encoding is
   use type Ada.Streams.Stream_Element_Offset;
   use type C.signed_int;
   use type C.size_t;

   function Is_Open (Object : Converter) return Boolean is
      iconv : constant C.iconv.iconv_t := Value (Object);
   begin
      return Address (iconv) /= Null_Address;
   end Is_Open;

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
      end Open;

      function Value (Object : Converter) return C.iconv.iconv_t is
      begin
         return Object.iconv;
      end Value;

      overriding procedure Finalize (Object : in out Converter) is
      begin
         if C.iconv.iconv_close (Object.iconv) /= 0 then
            null; -- raise Status_Error;
         end if;
      end Finalize;

   end Controlled;

end System.Native_Encoding;
