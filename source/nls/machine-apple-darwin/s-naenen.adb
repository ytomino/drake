with Ada.Exception_Identification.From_Here;
with System.Address_To_Constant_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
with C.stdint;
package body System.Native_Environment_Encoding is
   use Ada.Exception_Identification.From_Here;
   use type Ada.Streams.Stream_Element_Offset;
   use type Storage_Elements.Storage_Offset;
   use type C.icucore.UChar_const_ptr;
   use type C.icucore.UConverter_ptr;

   pragma Compile_Time_Error (
      Standard'Storage_Unit /= Ada.Streams.Stream_Element_Array'Component_Size,
      "Address operations is not equivalent to Stream_Element operations");

   package char_const_ptr_Conv is
      new Address_To_Constant_Access_Conversions (C.char, C.char_const_ptr);
   package char_ptr_Conv is
      new Address_To_Named_Access_Conversions (C.char, C.char_ptr);

   package UChar_const_ptr_Conv is
      new Address_To_Constant_Access_Conversions (
         C.icucore.UChar,
         C.icucore.UChar_const_ptr);
   package UChar_ptr_Conv is
      new Address_To_Named_Access_Conversions (
         C.icucore.UChar,
         C.icucore.UChar_ptr);

   procedure Adjust_Buffer (
      Buffer : in out Buffer_Type;
      First : in out C.icucore.UChar_const_ptr;
      Limit : in out C.icucore.UChar_ptr);
   procedure Adjust_Buffer (
      Buffer : in out Buffer_Type;
      First : in out C.icucore.UChar_const_ptr;
      Limit : in out C.icucore.UChar_ptr) is
   begin
      if UChar_const_ptr_Conv.To_Address (First) >=
         Buffer'Address
         + Storage_Elements.Storage_Offset'(Half_Buffer_Length)
      then
         --  shift
         declare
            First_Index : constant Storage_Elements.Storage_Offset :=
               UChar_const_ptr_Conv.To_Address (First) - Buffer'Address;
            Limit_Index : constant Storage_Elements.Storage_Offset :=
               UChar_ptr_Conv.To_Address (Limit) - Buffer'Address;
            Length : constant Storage_Elements.Storage_Offset :=
               Limit_Index - First_Index;
            Buffer_Storage : Storage_Elements.Storage_Array (
               0 ..
               Buffer_Type'Size / Standard'Storage_Unit - 1);
            for Buffer_Storage'Address use Buffer'Address;
         begin
            Buffer_Storage (0 .. Length - 1) :=
               Buffer_Storage (First_Index .. Limit_Index - 1);
            First := UChar_const_ptr_Conv.To_Pointer (Buffer'Address);
            Limit := UChar_ptr_Conv.To_Pointer (Buffer'Address + Length);
         end;
      end if;
   end Adjust_Buffer;

   procedure Default_Substitute (
      uconv : C.icucore.UConverter_ptr;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   procedure Default_Substitute (
      uconv : C.icucore.UConverter_ptr;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      len : aliased C.stdint.int8_t := Item'Length;
      Error : aliased C.icucore.UErrorCode :=
         C.icucore.unicode.utypes.U_ZERO_ERROR;
      pragma Suppress (Validity_Check, Error);
   begin
      C.icucore.unicode.ucnv.ucnv_getSubstChars (
         uconv,
         char_ptr_Conv.To_Pointer (Item'Address),
         len'Access,
         Error'Access);
      case Error is
         when C.icucore.unicode.utypes.U_INDEX_OUTOFBOUNDS_ERROR =>
            raise Constraint_Error;
         when others =>
            null;
      end case;
      Last := Item'First + Ada.Streams.Stream_Element_Offset (len) - 1;
   end Default_Substitute;

   --  implementation

   function Get_Image (Encoding : Encoding_Id) return String is
   begin
      return Zero_Terminated_Strings.Value (Encoding);
   end Get_Image;

   function Get_Default_Substitute (Encoding : Encoding_Id)
      return Ada.Streams.Stream_Element_Array
   is
      Result : Ada.Streams.Stream_Element_Array (
         0 .. -- from 0 for a result value
         Max_Substitute_Length - 1);
      Last : Ada.Streams.Stream_Element_Offset;
      uconv : C.icucore.UConverter_ptr;
      Error : aliased C.icucore.UErrorCode :=
         C.icucore.unicode.utypes.U_ZERO_ERROR;
   begin
      uconv := C.icucore.unicode.ucnv.ucnv_open (Encoding, Error'Access);
      if uconv = null then
         Raise_Exception (Name_Error'Identity);
      end if;
      Default_Substitute (uconv, Result, Last);
      C.icucore.unicode.ucnv.ucnv_close (uconv);
      return Result (Result'First .. Last);
   end Get_Default_Substitute;

   function Get_Min_Size_In_Stream_Elements (Encoding : Encoding_Id)
      return Ada.Streams.Stream_Element_Offset
   is
      Result : Ada.Streams.Stream_Element_Offset;
      uconv : C.icucore.UConverter_ptr;
      Error : aliased C.icucore.UErrorCode :=
         C.icucore.unicode.utypes.U_ZERO_ERROR;
   begin
      uconv := C.icucore.unicode.ucnv.ucnv_open (Encoding, Error'Access);
      if uconv = null then
         Raise_Exception (Name_Error'Identity);
      end if;
      Result := Ada.Streams.Stream_Element_Offset (
         C.icucore.unicode.ucnv.ucnv_getMinCharSize (uconv));
      C.icucore.unicode.ucnv.ucnv_close (uconv);
      return Result;
   end Get_Min_Size_In_Stream_Elements;

   function Get_Current_Encoding return Encoding_Id is
   begin
      return UTF_8_Name (0)'Access;
   end Get_Current_Encoding;

   procedure Open (Object : in out Converter; From, To : Encoding_Id) is
      NC_Object : Non_Controlled_Converter
         renames Controlled.Reference (Object).all;
      From_uconv : C.icucore.UConverter_ptr;
      To_uconv : C.icucore.UConverter_ptr;
      Error : aliased C.icucore.UErrorCode :=
         C.icucore.unicode.utypes.U_ZERO_ERROR;
      pragma Suppress (Validity_Check, Error);
   begin
      From_uconv := C.icucore.unicode.ucnv.ucnv_open (From, Error'Access);
      if From_uconv = null then
         Raise_Exception (Name_Error'Identity);
      end if;
      To_uconv := C.icucore.unicode.ucnv.ucnv_open (To, Error'Access);
      if To_uconv = null then
         C.icucore.unicode.ucnv.ucnv_close (From_uconv);
         Raise_Exception (Name_Error'Identity);
      end if;
      C.icucore.unicode.ucnv.ucnv_setFromUCallBack (
         To_uconv,
         C.icucore.unicode.ucnv_err.UCNV_FROM_U_CALLBACK_STOP'Access,
         C.void_const_ptr (Null_Address),
         null,
         null,
         Error'Access);
      case Error is
         when C.icucore.unicode.utypes.U_ZERO_ERROR =>
            null;
         when others =>
            C.icucore.unicode.ucnv.ucnv_close (To_uconv);
            C.icucore.unicode.ucnv.ucnv_close (From_uconv);
            Raise_Exception (Use_Error'Identity);
      end case;
      --  about "From"
      NC_Object.From_uconv := From_uconv;
      --  intermediate
      NC_Object.Buffer_First :=
         UChar_const_ptr_Conv.To_Pointer (NC_Object.Buffer'Address);
      NC_Object.Buffer_Limit :=
         UChar_ptr_Conv.To_Pointer (NC_Object.Buffer'Address);
      --  about "To"
      NC_Object.To_uconv := To_uconv;
      Default_Substitute (
         To_uconv,
         NC_Object.Substitute,
         NC_Object.Substitute_Length);
   end Open;

   function Get_Is_Open (Object : Converter) return Boolean is
      NC_Object : Non_Controlled_Converter
         renames Controlled.Reference (Object).all;
   begin
      return NC_Object.From_uconv /= null;
   end Get_Is_Open;

   function Min_Size_In_From_Stream_Elements_No_Check (Object : Converter)
      return Ada.Streams.Stream_Element_Offset
   is
      NC_Object : Non_Controlled_Converter
         renames Controlled.Reference (Object).all;
   begin
      return Ada.Streams.Stream_Element_Offset (
         C.icucore.unicode.ucnv.ucnv_getMinCharSize (NC_Object.From_uconv));
   end Min_Size_In_From_Stream_Elements_No_Check;

   function Substitute_No_Check (Object : Converter)
      return Ada.Streams.Stream_Element_Array
   is
      NC_Object : Non_Controlled_Converter
         renames Controlled.Reference (Object).all;
   begin
      return NC_Object.Substitute (1 .. NC_Object.Substitute_Length);
   end Substitute_No_Check;

   procedure Set_Substitute_No_Check (
      Object : in out Converter;
      Substitute : Ada.Streams.Stream_Element_Array)
   is
      NC_Object : Non_Controlled_Converter
         renames Controlled.Reference (Object).all;
   begin
      if Substitute'Length > NC_Object.Substitute'Length then
         raise Constraint_Error;
      end if;
      NC_Object.Substitute_Length := Substitute'Length;
      NC_Object.Substitute (1 .. NC_Object.Substitute_Length) := Substitute;
   end Set_Substitute_No_Check;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Finish : Boolean;
      Status : out Subsequence_Status_Type)
   is
      NC_Object : Non_Controlled_Converter
         renames Controlled.Reference (Object).all;
      Unfilled_Buffer_Limit : constant C.icucore.UChar_const_ptr :=
         UChar_const_ptr_Conv.To_Pointer (
            NC_Object.Buffer'Address
            + Storage_Elements.Storage_Offset'(Buffer_Type'Length));
      Pointer : aliased C.char_const_ptr :=
         char_const_ptr_Conv.To_Pointer (Item'Address);
      Limit : constant C.char_const_ptr :=
         char_const_ptr_Conv.To_Pointer (
            Item'Address
            + Storage_Elements.Storage_Offset'(Item'Length));
      Out_Pointer : aliased C.char_ptr :=
         char_ptr_Conv.To_Pointer (Out_Item'Address);
      Out_Limit : constant C.char_ptr :=
         char_ptr_Conv.To_Pointer (
            Out_Item'Address
            + Storage_Elements.Storage_Offset'(Out_Item'Length));
      Finish_2nd : Boolean;
      Error : aliased C.icucore.UErrorCode;
      pragma Suppress (Validity_Check, Error);
   begin
      Adjust_Buffer (
         NC_Object.Buffer,
         NC_Object.Buffer_First,
         NC_Object.Buffer_Limit);
      Status := Success;
      Error := C.icucore.unicode.utypes.U_ZERO_ERROR;
      C.icucore.unicode.ucnv.ucnv_toUnicode (
         NC_Object.From_uconv,
         NC_Object.Buffer_Limit'Access,
         Unfilled_Buffer_Limit,
         Pointer'Access,
         Limit,
         null,
         Boolean'Pos (Finish),
         Error'Access);
      case Error is
         when C.icucore.unicode.utypes.U_ZERO_ERROR =>
            null;
         when C.icucore.unicode.utypes.U_TRUNCATED_CHAR_FOUND =>
            Status := Truncated;
         when C.icucore.unicode.utypes.U_ILLEGAL_CHAR_FOUND =>
            Status := Illegal_Sequence;
         when C.icucore.unicode.utypes.U_BUFFER_OVERFLOW_ERROR =>
            null; -- not finished
         when others =>
            Raise_Exception (Use_Error'Identity);
      end case;
      Last := Item'First
         + Ada.Streams.Stream_Element_Offset (
            char_const_ptr_Conv.To_Address (Pointer) - Item'Address)
         - 1;
      Finish_2nd := Finish and then Last = Item'Last;
      Error := C.icucore.unicode.utypes.U_ZERO_ERROR;
      C.icucore.unicode.ucnv.ucnv_fromUnicode (
         NC_Object.To_uconv,
         Out_Pointer'Access,
         Out_Limit,
         NC_Object.Buffer_First'Access,
         NC_Object.Buffer_Limit,
         null,
         Boolean'Pos (Finish_2nd),
         Error'Access);
      case Error is
         when C.icucore.unicode.utypes.U_ZERO_ERROR =>
            if Finish_2nd and then Status = Success then
               Status := Finished;
            end if;
         when C.icucore.unicode.utypes.U_INVALID_CHAR_FOUND =>
            if Status = Success then
               Status := Illegal_Sequence;
            end if;
         when C.icucore.unicode.utypes.U_BUFFER_OVERFLOW_ERROR =>
            if Status = Success then
               Status := Overflow;
            end if;
         when others => -- unknown
            Raise_Exception (Use_Error'Identity);
      end case;
      Out_Last := Out_Item'First
         + Ada.Streams.Stream_Element_Offset (
            char_ptr_Conv.To_Address (Out_Pointer) - Out_Item'Address)
         - 1;
   end Convert_No_Check;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Continuing_Status_Type)
   is
      Subsequence_Status : Subsequence_Status_Type;
   begin
      Convert_No_Check (
         Object,
         Item,
         Last,
         Out_Item,
         Out_Last,
         False,
         Subsequence_Status);
      pragma Assert (Subsequence_Status in
         Subsequence_Status_Type (Continuing_Status_Type'First) ..
         Subsequence_Status_Type (Continuing_Status_Type'Last));
      Status := Continuing_Status_Type (Subsequence_Status);
   end Convert_No_Check;

   procedure Convert_No_Check (
      Object : Converter;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Finishing_Status_Type)
   is
      pragma Unreferenced (Finish);
      NC_Object : Non_Controlled_Converter
         renames Controlled.Reference (Object).all;
      Out_Pointer : aliased C.char_ptr :=
         char_ptr_Conv.To_Pointer (Out_Item'Address);
      Out_Limit : constant C.char_ptr :=
         char_ptr_Conv.To_Pointer (
            Out_Item'Address
            + Storage_Elements.Storage_Offset'(Out_Item'Length));
      Error : aliased C.icucore.UErrorCode :=
         C.icucore.unicode.utypes.U_ZERO_ERROR;
      pragma Suppress (Validity_Check, Error);
   begin
      C.icucore.unicode.ucnv.ucnv_fromUnicode (
         NC_Object.To_uconv,
         Out_Pointer'Access,
         Out_Limit,
         NC_Object.Buffer_First'Access,
         NC_Object.Buffer_Limit,
         null,
         1, -- flush
         Error'Access);
      case Error is
         when C.icucore.unicode.utypes.U_ZERO_ERROR =>
            Status := Finished;
         when C.icucore.unicode.utypes.U_BUFFER_OVERFLOW_ERROR =>
            Status := Overflow;
         when others => -- unknown
            Raise_Exception (Use_Error'Identity);
      end case;
      Out_Last := Out_Item'First
         + Ada.Streams.Stream_Element_Offset (
            char_ptr_Conv.To_Address (Out_Pointer) - Out_Item'Address)
         - 1;
   end Convert_No_Check;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Substituting_Status_Type) is
   begin
      Last := Item'First - 1;
      Out_Last := Out_Item'First - 1;
      loop
         declare
            Subsequence_Status : Subsequence_Status_Type;
         begin
            Convert_No_Check (
               Object,
               Item (Last + 1 .. Item'Last),
               Last,
               Out_Item (Out_Last + 1 .. Out_Item'Last),
               Out_Last,
               Finish => Finish,
               Status => Subsequence_Status);
            pragma Assert (Subsequence_Status in
               Subsequence_Status_Type (Status_Type'First) ..
               Subsequence_Status_Type (Status_Type'Last));
            case Status_Type (Subsequence_Status) is
               when Finished =>
                  Status := Finished;
                  return;
               when Success =>
                  Status := Success;
                  return;
               when Overflow =>
                  Status := Overflow;
                  return;
               when Illegal_Sequence =>
                  declare
                     Is_Overflow : Boolean;
                  begin
                     Put_Substitute (
                        Object,
                        Out_Item (Out_Last + 1 .. Out_Item'Last),
                        Out_Last,
                        Is_Overflow);
                     if Is_Overflow then
                        Status := Overflow;
                        return; -- wait a next try
                     end if;
                  end;
                  declare
                     New_Last : Ada.Streams.Stream_Element_Offset :=
                        Last
                        + Min_Size_In_From_Stream_Elements_No_Check (Object);
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
   end Convert_No_Check;

   procedure Put_Substitute (
      Object : Converter;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Is_Overflow : out Boolean)
   is
      NC_Object : Non_Controlled_Converter
         renames Controlled.Reference (Object).all;
   begin
      Out_Last := Out_Item'First - 1;
      Is_Overflow := Out_Item'Length < NC_Object.Substitute_Length;
      if Is_Overflow then
         return;
      end if;
      Out_Last := Out_Last + NC_Object.Substitute_Length;
      Out_Item (Out_Item'First .. Out_Last) :=
         NC_Object.Substitute (1 .. NC_Object.Substitute_Length);
   end Put_Substitute;

   package body Controlled is

      function Reference (Object : Native_Environment_Encoding.Converter)
         return not null access Non_Controlled_Converter is
      begin
         return Converter (Object).Data'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out Converter) is
      begin
         C.icucore.unicode.ucnv.ucnv_close (Object.Data.From_uconv);
         C.icucore.unicode.ucnv.ucnv_close (Object.Data.To_uconv);
      end Finalize;

   end Controlled;

end System.Native_Environment_Encoding;
