package body Ada.Environment_Encoding is

   --  implementation

   function Is_Open (Object : Converter) return Boolean is
      N_Object : System.Native_Environment_Encoding.Converter
         renames Controlled.Reference (Object).all;
   begin
      return System.Native_Environment_Encoding.Is_Open (N_Object);
   end Is_Open;

   function Min_Size_In_From_Stream_Elements (
      Object : Converter)
      return Streams.Stream_Element_Offset
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Object) or else raise Status_Error);
      N_Object : System.Native_Environment_Encoding.Converter
         renames Controlled.Reference (Object).all;
   begin
      return System.Native_Environment_Encoding
            .Min_Size_In_From_Stream_Elements_No_Check (
         N_Object);
   end Min_Size_In_From_Stream_Elements;

   function Substitute (
      Object : Converter)
      return Streams.Stream_Element_Array
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Object) or else raise Status_Error);
      N_Object : System.Native_Environment_Encoding.Converter
         renames Controlled.Reference (Object).all;
   begin
      return System.Native_Environment_Encoding.Substitute_No_Check (N_Object);
   end Substitute;

   procedure Set_Substitute (
      Object : in out Converter;
      Substitute : Streams.Stream_Element_Array)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Object) or else raise Status_Error);
      N_Object : System.Native_Environment_Encoding.Converter
         renames Controlled.Reference (Object).all;
   begin
      System.Native_Environment_Encoding.Set_Substitute_No_Check (
         N_Object,
         Substitute);
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
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Object) or else raise Status_Error);
      N_Object : System.Native_Environment_Encoding.Converter
         renames Controlled.Reference (Object).all;
      N_Status : System.Native_Environment_Encoding.Subsequence_Status_Type;
   begin
      System.Native_Environment_Encoding.Convert_No_Check (
         N_Object,
         Item,
         Last,
         Out_Item,
         Out_Last,
         Finish,
         N_Status);
      Status := Subsequence_Status_Type'Enum_Val (
         System.Native_Environment_Encoding.Subsequence_Status_Type'Enum_Rep (
            N_Status));
   end Convert;

   procedure Convert (
      Object : Converter;
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Status : out Continuing_Status_Type)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Object) or else raise Status_Error);
      N_Object : System.Native_Environment_Encoding.Converter
         renames Controlled.Reference (Object).all;
      N_Status : System.Native_Environment_Encoding.Continuing_Status_Type;
   begin
      System.Native_Environment_Encoding.Convert_No_Check (
         N_Object,
         Item,
         Last,
         Out_Item,
         Out_Last,
         N_Status);
      Status := Continuing_Status_Type'Enum_Val (
         System.Native_Environment_Encoding.Continuing_Status_Type'Enum_Rep (
            N_Status));
   end Convert;

   procedure Convert (
      Object : Converter;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Finishing_Status_Type)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Object) or else raise Status_Error);
      N_Object : System.Native_Environment_Encoding.Converter
         renames Controlled.Reference (Object).all;
      N_Status : System.Native_Environment_Encoding.Finishing_Status_Type;
   begin
      System.Native_Environment_Encoding.Convert_No_Check (
         N_Object,
         Out_Item,
         Out_Last,
         Finish,
         N_Status);
      Status := Finishing_Status_Type'Enum_Val (
         System.Native_Environment_Encoding.Finishing_Status_Type'Enum_Rep (
            N_Status));
   end Convert;

   procedure Convert (
      Object : Converter;
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Status_Type)
   is
      Subsequence_Status : Subsequence_Status_Type;
   begin
      Convert (
         Object, -- checking the predicate
         Item,
         Last,
         Out_Item,
         Out_Last,
         Finish,
         Subsequence_Status);
      pragma Assert (
         Subsequence_Status in
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
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Object) or else raise Status_Error);
      N_Object : System.Native_Environment_Encoding.Converter
         renames Controlled.Reference (Object).all;
      N_Status : System.Native_Environment_Encoding.Substituting_Status_Type;
   begin
      System.Native_Environment_Encoding.Convert_No_Check (
         N_Object,
         Item,
         Last,
         Out_Item,
         Out_Last,
         Finish,
         N_Status);
      Status := Substituting_Status_Type'Enum_Val (
         System.Native_Environment_Encoding.Substituting_Status_Type'Enum_Rep (
            N_Status));
   end Convert;

   package body Controlled is

      function Reference (Object : Environment_Encoding.Converter)
         return not null access System.Native_Environment_Encoding.Converter is
      begin
         return Converter (Object).Data'Unrestricted_Access;
      end Reference;

      function Open (From, To : Encoding_Id)
         return Environment_Encoding.Converter is
      begin
         return Result : Environment_Encoding.Converter do
            System.Native_Environment_Encoding.Open (
               Converter (Result).Data'Unrestricted_Access.all,
               From => System.Native_Environment_Encoding.Encoding_Id (From),
               To => System.Native_Environment_Encoding.Encoding_Id (To));
         end return;
      end Open;

      overriding procedure Finalize (Object : in out Converter) is
      begin
         System.Native_Environment_Encoding.Close (Object.Data);
      end Finalize;

   end Controlled;

end Ada.Environment_Encoding;
