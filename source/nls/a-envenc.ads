pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
with Ada.Streams;
private with Ada.Finalization;
private with System.Native_Environment_Encoding;
package Ada.Environment_Encoding is
   --  Platform-depended text encoding.
   pragma Preelaborate;

   --  encoding identifier

   type Encoding_Id is private;

   function Image (Encoding : Encoding_Id) return String;
   pragma Inline (Image); -- renamed

   function Default_Substitute (Encoding : Encoding_Id)
      return Streams.Stream_Element_Array;
   pragma Inline (Default_Substitute); -- renamed

   function Min_Size_In_Stream_Elements (Encoding : Encoding_Id)
      return Streams.Stream_Element_Offset;
   pragma Inline (Min_Size_In_Stream_Elements); -- renamed

   UTF_8 : constant Encoding_Id;
   UTF_16 : constant Encoding_Id;
   UTF_32 : constant Encoding_Id;

   function Current_Encoding return Encoding_Id;
   pragma Inline (Current_Encoding); -- renamed

   --  subsidiary types to converter

   type Subsequence_Status_Type is (
      Finished,
      Success,
      Overflow, -- the output buffer is not large enough
      Illegal_Sequence, -- a input character could not be mapped to the output
      Truncated); -- the input buffer is broken off at a multi-byte character

   type Continuing_Status_Type is
      new Subsequence_Status_Type range
         Success ..
         Subsequence_Status_Type'Last;
   type Finishing_Status_Type is
      new Subsequence_Status_Type range
         Finished ..
         Overflow;
   type Status_Type is
      new Subsequence_Status_Type range
         Finished ..
         Illegal_Sequence;

   type Substituting_Status_Type is
      new Status_Type range
         Finished ..
         Overflow;

   subtype True_Only is Boolean range True .. True;

   --  converter

   type Converter is limited private;

--  subtype Open_Converter is Converter
--    with
--       Dynamic_Predicate => Is_Open (Converter),
--       Predicate_Failure => raise Status_Error;

   function Is_Open (Object : Converter) return Boolean;
   pragma Inline (Is_Open);

   function Min_Size_In_From_Stream_Elements (
      Object : Converter) -- Open_Converter
      return Streams.Stream_Element_Offset;
   pragma Inline (Min_Size_In_From_Stream_Elements);

   function Substitute (
      Object : Converter) -- Open_Converter
      return Streams.Stream_Element_Array;
   pragma Inline (Substitute);

   procedure Set_Substitute (
      Object : in out Converter; -- Open_Converter
      Substitute : Streams.Stream_Element_Array);

   --  convert subsequence
   procedure Convert (
      Object : Converter; -- Open_Converter
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : Boolean;
      Status : out Subsequence_Status_Type);

   procedure Convert (
      Object : Converter; -- Open_Converter
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Status : out Continuing_Status_Type);

   procedure Convert (
      Object : Converter; -- Open_Converter
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Finishing_Status_Type);

   --  convert all character sequence
   procedure Convert (
      Object : Converter; -- Open_Converter
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Status_Type);

   --  convert all character sequence with substitute
   procedure Convert (
      Object : Converter; -- Open_Converter
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Substituting_Status_Type);

   --  exceptions

   Status_Error : exception
      renames IO_Exceptions.Status_Error;
   Name_Error : exception
      renames IO_Exceptions.Name_Error;
   Use_Error : exception
      renames IO_Exceptions.Use_Error;

private

   --  max length of one multi-byte character

   Max_Substitute_Length : constant :=
      System.Native_Environment_Encoding.Max_Substitute_Length;

   --  encoding identifier

   type Encoding_Id is new System.Native_Environment_Encoding.Encoding_Id;

   function Image (Encoding : Encoding_Id) return String
      renames Get_Image;

   function Default_Substitute (Encoding : Encoding_Id)
      return Streams.Stream_Element_Array
      renames Get_Default_Substitute;

   function Min_Size_In_Stream_Elements (Encoding : Encoding_Id)
      return Streams.Stream_Element_Offset
      renames Get_Min_Size_In_Stream_Elements;

   UTF_8 : constant Encoding_Id :=
      Encoding_Id (System.Native_Environment_Encoding.UTF_8);
   UTF_16 : constant Encoding_Id :=
      Encoding_Id (System.Native_Environment_Encoding.UTF_16);
   UTF_32 : constant Encoding_Id :=
      Encoding_Id (System.Native_Environment_Encoding.UTF_32);

   function Current_Encoding return Encoding_Id
      renames Get_Current_Encoding;

   --  converter

   package Controlled is

      type Converter is limited private;

      function Reference (Object : Environment_Encoding.Converter)
         return not null access System.Native_Environment_Encoding.Converter;

      function Open (From, To : Encoding_Id)
         return Environment_Encoding.Converter;
         --  [gcc-7] strange error if this function is placed outside of
         --    the package Controlled, and Disable_Controlled => True

   private

      type Converter is limited new Finalization.Limited_Controlled with record
         Data : aliased System.Native_Environment_Encoding.Converter :=
            (others => <>);
      end record
         with
            Disable_Controlled =>
               System.Native_Environment_Encoding.Disable_Controlled;

      overriding procedure Finalize (Object : in out Converter);

   end Controlled;

   type Converter is new Controlled.Converter;

end Ada.Environment_Encoding;
