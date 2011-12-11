with Ada.Streams.Stream_IO.Inside; -- full view
package body Ada.Streams.Stream_IO is

   procedure Close (File : in out File_Type) is
   begin
      Inside.Close (
         Inside.Non_Controlled_File_Type (File.Stream),
         Raise_On_Error => True);
   end Close;

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "") is
   begin
      Inside.Create (
         Inside.Non_Controlled_File_Type (File.Stream),
         Mode,
         Name,
         Form);
   end Create;

   function Create (
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "")
      return File_Type is
   begin
      return Result : File_Type do
         Create (Result, Mode, Name, Form);
      end return;
   end Create;

   procedure Delete (File : in out File_Type) is
   begin
      Inside.Delete (Inside.Non_Controlled_File_Type (File.Stream));
   end Delete;

   function End_Of_File (File : File_Type) return Boolean is
   begin
      return Inside.End_Of_File (
         Inside.Non_Controlled_File_Type (File.Stream));
   end End_Of_File;

   overriding procedure Finalize (Object : in out File_Type) is
   begin
      if Inside.Is_Open (Inside.Non_Controlled_File_Type (Object.Stream)) then
         Inside.Close (
            Inside.Non_Controlled_File_Type (Object.Stream),
            Raise_On_Error => False);
      end if;
   end Finalize;

   procedure Flush (File : File_Type) is
   begin
      Inside.Flush (Inside.Non_Controlled_File_Type (File.Stream));
   end Flush;

   function Form (File : File_Type) return String is
   begin
      return Inside.Form (Inside.Non_Controlled_File_Type (File.Stream));
   end Form;

   function Index (File : File_Type) return Positive_Count is
   begin
      return Inside.Index (Inside.Non_Controlled_File_Type (File.Stream));
   end Index;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return Inside.Is_Open (Inside.Non_Controlled_File_Type (File.Stream));
   end Is_Open;

   function Name (File : File_Type) return String is
   begin
      return Inside.Name (Inside.Non_Controlled_File_Type (File.Stream));
   end Name;

   function Mode (File : File_Type) return File_Mode is
   begin
      return Inside.Mode (Inside.Non_Controlled_File_Type (File.Stream));
   end Mode;

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "") is
   begin
      Inside.Open (
         Inside.Non_Controlled_File_Type (File.Stream),
         Mode,
         Name,
         Form);
   end Open;

   function Open (
      Mode : File_Mode;
      Name : String;
      Form : String := "")
      return File_Type is
   begin
      return Result : File_Type do
         Open (Result, Mode, Name, Form);
      end return;
   end Open;

   procedure Read (
      File : File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset;
      From : Positive_Count) is
   begin
      Set_Index (File, From);
      Read (File, Item, Last);
   end Read;

   procedure Read (
      File : File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset) is
   begin
      Inside.Read (Inside.Non_Controlled_File_Type (File.Stream), Item, Last);
   end Read;

   procedure Reset (File : in out File_Type; Mode : File_Mode) is
   begin
      Inside.Reset (Inside.Non_Controlled_File_Type (File.Stream), Mode);
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      Reset (File, Mode (File));
   end Reset;

   procedure Set_Index (File : File_Type; To : Positive_Count) is
   begin
      Inside.Set_Index (Inside.Non_Controlled_File_Type (File.Stream), To);
   end Set_Index;

   procedure Set_Mode (File : in out File_Type; Mode : File_Mode) is
   begin
      Inside.Set_Mode (Inside.Non_Controlled_File_Type (File.Stream), Mode);
   end Set_Mode;

   function Size (File : File_Type) return Count is
   begin
      return Inside.Size (Inside.Non_Controlled_File_Type (File.Stream));
   end Size;

   function Stream (File : File_Type) return Stream_Access is
   begin
      return Inside.Stream (Inside.Non_Controlled_File_Type (File.Stream));
   end Stream;

   procedure Write (
      File : File_Type;
      Item : Stream_Element_Array;
      To : Positive_Count) is
   begin
      Set_Index (File, To);
      Write (File, Item, To);
   end Write;

   procedure Write (
      File : File_Type;
      Item : Stream_Element_Array) is
   begin
      Inside.Write (Inside.Non_Controlled_File_Type (File.Stream), Item);
   end Write;

end Ada.Streams.Stream_IO;
