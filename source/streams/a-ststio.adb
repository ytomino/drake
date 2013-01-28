with Ada.Streams.Stream_IO.Inside; -- full view
package body Ada.Streams.Stream_IO is

   procedure Close (File : in out File_Type) is
   begin
      Inside.Close (Reference (File).all, Raise_On_Error => True);
   end Close;

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "") is
   begin
      Inside.Create (Reference (File).all, Mode, Name => Name, Form => Form);
   end Create;

   function Create (
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "")
      return File_Type is
   begin
      return Result : File_Type do
         Create (Result, Mode, Name => Name, Form => Form);
      end return;
   end Create;

   procedure Delete (File : in out File_Type) is
   begin
      Inside.Delete (Reference (File).all);
   end Delete;

   function End_Of_File (File : File_Type) return Boolean is
   begin
      return Inside.End_Of_File (Reference (File).all);
   end End_Of_File;

   procedure Flush (File : File_Type) is
   begin
      Inside.Flush (Reference (File).all);
   end Flush;

   function Form (File : File_Type) return String is
   begin
      return Inside.Form (Reference (File).all);
   end Form;

   function Index (File : File_Type) return Positive_Count is
   begin
      return Inside.Index (Reference (File).all);
   end Index;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return Inside.Is_Open (Reference (File).all);
   end Is_Open;

   function Name (File : File_Type) return String is
   begin
      return Inside.Name (Reference (File).all);
   end Name;

   function Mode (File : File_Type) return File_Mode is
   begin
      return Inside.Mode (Reference (File).all);
   end Mode;

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "") is
   begin
      Inside.Open (Reference (File).all, Mode, Name => Name, Form => Form);
   end Open;

   function Open (
      Mode : File_Mode;
      Name : String;
      Form : String := "")
      return File_Type is
   begin
      return Result : File_Type do
         Open (Result, Mode, Name => Name, Form => Form);
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
      Inside.Read (Reference (File).all, Item, Last);
   end Read;

   procedure Reset (File : in out File_Type; Mode : File_Mode) is
   begin
      Inside.Reset (Reference (File), Mode);
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      Reset (File, Mode (File));
   end Reset;

   procedure Set_Index (File : File_Type; To : Positive_Count) is
   begin
      Inside.Set_Index (Reference (File).all, To);
   end Set_Index;

   procedure Set_Mode (File : in out File_Type; Mode : File_Mode) is
   begin
      Inside.Set_Mode (Reference (File), Mode);
   end Set_Mode;

   function Size (File : File_Type) return Count is
   begin
      return Inside.Size (Reference (File).all);
   end Size;

   function Stream (File : File_Type) return Stream_Access is
   begin
      return Inside.Stream (Reference (File).all);
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
      Inside.Write (Reference (File).all, Item);
   end Write;

   package body Controlled is

      function Reference (File : File_Type)
         return access Inside.Non_Controlled_File_Type is
      begin
         return Inside.Non_Controlled_File_Type (
            File.Stream)'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out File_Type) is
      begin
         if Inside.Is_Open (Reference (Object).all) then
            Inside.Close (Reference (Object).all, Raise_On_Error => False);
         end if;
      end Finalize;

   end Controlled;

end Ada.Streams.Stream_IO;
