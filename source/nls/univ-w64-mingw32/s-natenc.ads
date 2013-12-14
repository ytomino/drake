pragma License (Unrestricted);
--  implementation unit
with Ada.IO_Exceptions;
with Ada.Streams;
with C.windef;
with C.winnls;
package System.Native_Encoding is
   --  Platform-depended text encoding.
   pragma Preelaborate;
   use type Ada.Streams.Stream_Element_Offset;

   --  max length of one multi-byte character

   Max_Substitute_Length : constant := 6; -- UTF-8

   --  encoding identifier

   type Encoding_Id is new C.windef.UINT;

   function Get_Image (Encoding : Encoding_Id) return String;

   function Get_Default_Substitute (Encoding : Encoding_Id)
      return Ada.Streams.Stream_Element_Array;

   function Get_Min_Size_In_Stream_Elements (Encoding : Encoding_Id)
      return Ada.Streams.Stream_Element_Offset;

   UTF_8 : constant Encoding_Id := C.winnls.CP_UTF8;
   UTF_16 : constant Encoding_Id := 16#ffff_ff10#; -- dummy value
   UTF_32 : constant Encoding_Id := 16#ffff_ff20#; -- dummy value

   function Get_Current_Encoding return Encoding_Id;

   Invalid_Encoding_Id : constant := 16#ffff_ffff#; -- dummy value

   --  subsidiary types to converter

   type Subsequence_Status_Type is (
      Finished,
      Success,
      Overflow, -- the output buffer is not large enough
      Illegal_Sequence, -- a input character could not be mapped to the output
      Truncated); -- the input buffer is broken off at a multi-byte character
   pragma Discard_Names (Subsequence_Status_Type);

   type Continuing_Status_Type is
      new Subsequence_Status_Type
         range Success .. Subsequence_Status_Type'Last;
   type Finishing_Status_Type is
      new Subsequence_Status_Type
         range Finished .. Overflow;
   type Status_Type is
      new Subsequence_Status_Type
         range Finished .. Illegal_Sequence;

   type Substituting_Status_Type is
      new Status_Type
         range Finished .. Overflow;

   subtype True_Only is Boolean range True .. True;

   --  converter

   type Converter is record
      From : Encoding_Id := Invalid_Encoding_Id;
      To : Encoding_Id;
      Substitute_Length : Ada.Streams.Stream_Element_Offset;
      Substitute : aliased Ada.Streams.Stream_Element_Array (
         1 ..
         Max_Substitute_Length + 1); -- zero terminated
   end record;

   procedure Open (Object : out Converter; From, To : Encoding_Id);

   function Get_Is_Open (Object : Converter) return Boolean;

   function Min_Size_In_From_Stream_Elements_No_Check (Object : Converter)
      return Ada.Streams.Stream_Element_Offset;

   function Substitute_No_Check (Object : Converter)
      return Ada.Streams.Stream_Element_Array;

   procedure Set_Substitute_No_Check (
      Object : in out Converter;
      Substitute : Ada.Streams.Stream_Element_Array);

   --  convert subsequence
   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Finish : Boolean;
      Status : out Subsequence_Status_Type);

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Continuing_Status_Type);

   procedure Convert_No_Check (
      Object : Converter;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Finishing_Status_Type);

   --  convert all character sequence
--  procedure Convert_No_Check (
--    Object : Converter;
--    Item : Ada.Streams.Stream_Element_Array;
--    Last : out Ada.Streams.Stream_Element_Offset;
--    Out_Item : out Ada.Streams.Stream_Element_Array;
--    Out_Last : out Ada.Streams.Stream_Element_Offset;
--    Finished : True_Only;
--    Status : out Status_Type);

   --  convert all character sequence with substitute
   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Substituting_Status_Type);

   procedure Put_Substitute (
      Object : Converter;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Is_Overflow : out Boolean);

   --  exceptions

   Name_Error : exception
      renames Ada.IO_Exceptions.Name_Error;
   Status_Error : exception
      renames Ada.IO_Exceptions.Status_Error;
   Use_Error : exception
      renames Ada.IO_Exceptions.Use_Error;

end System.Native_Encoding;
