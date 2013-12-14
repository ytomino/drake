with Ada.Exceptions;
package body Ada.Environment_Encoding is

   --  implementation

   function Min_Size_In_From_Stream_Elements (Object : Converter)
      return Streams.Stream_Element_Offset is
   begin
      if not Is_Open (Object) then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      return Min_Size_In_From_Stream_Elements_No_Check (Object);
   end Min_Size_In_From_Stream_Elements;

   function Substitute (Object : Converter)
      return Streams.Stream_Element_Array is
   begin
      if not Is_Open (Object) then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      return Substitute_No_Check (Object);
   end Substitute;

   procedure Set_Substitute (
      Object : in out Converter;
      Substitute : Streams.Stream_Element_Array) is
   begin
      if not Is_Open (Object) then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Set_Substitute_No_Check (Object, Substitute);
   end Set_Substitute;

   procedure Convert (
      Object : Converter;
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : Boolean;
      Status : out Subsequence_Status_Type)
   is
      N_Status : System.Native_Encoding.Subsequence_Status_Type;
   begin
      if not Is_Open (Object) then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Convert_No_Check (Object,
         Item,
         Last,
         Out_Item,
         Out_Last,
         Finish,
         N_Status);
      Status := Subsequence_Status_Type'Enum_Val (
         System.Native_Encoding.Subsequence_Status_Type'Enum_Rep (N_Status));
   end Convert;

   procedure Convert (
      Object : Converter;
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Status : out Continuing_Status_Type)
   is
      N_Status : System.Native_Encoding.Continuing_Status_Type;
   begin
      if not Is_Open (Object) then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Convert_No_Check (
         Object,
         Item,
         Last,
         Out_Item,
         Out_Last,
         N_Status);
      Status := Continuing_Status_Type'Enum_Val (
         System.Native_Encoding.Continuing_Status_Type'Enum_Rep (N_Status));
   end Convert;

   procedure Convert (
      Object : Converter;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Finishing_Status_Type)
   is
      N_Status : System.Native_Encoding.Finishing_Status_Type;
   begin
      if not Is_Open (Object) then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Convert_No_Check (
         Object,
         Out_Item,
         Out_Last,
         Finish,
         N_Status);
      Status := Finishing_Status_Type'Enum_Val (
         System.Native_Encoding.Finishing_Status_Type'Enum_Rep (N_Status));
   end Convert;

   procedure Convert (
      Object : Converter;
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finished : True_Only;
      Status : out Status_Type)
   is
      Subsequence_Status : Subsequence_Status_Type;
   begin
      Convert (
         Object,
         Item,
         Last,
         Out_Item,
         Out_Last,
         Finished,
         Subsequence_Status);
      pragma Assert (Subsequence_Status in
         Subsequence_Status_Type (Status_Type'First) ..
         Subsequence_Status_Type (Status_Type'Last));
      Status := Status_Type (Subsequence_Status);
   end Convert;

   procedure Convert (
      Object : Converter;
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Substituting_Status_Type)
   is
      N_Status : System.Native_Encoding.Substituting_Status_Type;
   begin
      if not Is_Open (Object) then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Convert_No_Check (
         Object,
         Item,
         Last,
         Out_Item,
         Out_Last,
         Finish,
         N_Status);
      Status := Substituting_Status_Type'Enum_Val (
         System.Native_Encoding.Substituting_Status_Type'Enum_Rep (N_Status));
   end Convert;

end Ada.Environment_Encoding;
