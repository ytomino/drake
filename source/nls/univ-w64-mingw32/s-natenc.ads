pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
with Ada.Streams;
private with C.windef;
private with C.winnls;
package System.Native_Encoding is
   --  Platform-depended text encoding.
   pragma Preelaborate;

   --  encoding identifier

   type Encoding_Id is private;

   function Default_Substitute (Encoding : Encoding_Id)
      return Ada.Streams.Stream_Element_Array;

   UTF_8 : constant Encoding_Id;
   UTF_16 : constant Encoding_Id;
   UTF_32 : constant Encoding_Id;

   type Error_Status is (Fine, Incomplete, Illegal_Sequence);

   --  converter

   type Converter is limited private;

   function Is_Open (Object : Converter) return Boolean;

   function Substitute (Object : Converter)
      return Ada.Streams.Stream_Element_Array;

   procedure Set_Substitute (
      Object : in out Converter;
      Substitute : Ada.Streams.Stream_Element_Array);

   procedure Convert (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Error_Status);

   procedure Convert (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset);

   --  exceptions

   Name_Error : exception
      renames Ada.IO_Exceptions.Name_Error;
   Status_Error : exception
      renames Ada.IO_Exceptions.Status_Error;

private
   use type Ada.Streams.Stream_Element_Offset;
   use type C.char_array;

   --  max length of one multi-byte character

   Expanding : constant := 6; -- UTF-8

   --  encoding identifier

   type Encoding_Id is new C.windef.UINT;

   UTF_8 : constant Encoding_Id := C.winnls.CP_UTF8;
   UTF_16 : constant Encoding_Id := 16#ffff_ff10#; -- dummy value
   UTF_32 : constant Encoding_Id := 16#ffff_ff20#; -- dummy value

   Invalid_Encoding_Id : constant := 16#ffff_ffff#; -- dummy value

   --  converter

   type Converter is record
      From : Encoding_Id := Invalid_Encoding_Id;
      To : Encoding_Id;
      Substitute_Length : Ada.Streams.Stream_Element_Offset;
      Substitute : aliased Ada.Streams.Stream_Element_Array (
         1 ..
         Expanding + 1); -- zero terminated
   end record;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Error_Status);

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset);

   procedure Put_Substitute (
      Object : Converter;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset);

   procedure Open (Object : out Converter; From, To : Encoding_Id);

end System.Native_Encoding;
