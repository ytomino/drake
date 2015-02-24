with Ada.Exception_Identification.From_Here;
with Ada.Naked_Text_IO;
with System.Native_Text_IO;
package body Ada.Text_IO.Terminal is
   use Exception_Identification.From_Here;
   use type IO_Modes.File_Mode;
   use type IO_Modes.File_External;

   procedure Check_File_Mode (
      File : Naked_Text_IO.Non_Controlled_File_Type;
      Expected : IO_Modes.File_Mode;
      Line : Integer := Debug.Line);
   procedure Check_File_Mode (
      File : Naked_Text_IO.Non_Controlled_File_Type;
      Expected : IO_Modes.File_Mode;
      Line : Integer := Debug.Line) is
   begin
      if (Naked_Text_IO.Mode (File) = IO_Modes.In_File) /=
         (Expected = IO_Modes.In_File)
      then
         Raise_Exception (Mode_Error'Identity, Line => Line);
      end if;
   end Check_File_Mode;

   --  implementation

   function Is_Terminal (File : File_Type) return Boolean is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      return Naked_Text_IO.External (NC_File) = IO_Modes.Terminal;
   end Is_Terminal;

   procedure Set_Size (File : File_Type; Size : Size_Type) is
   begin
      Set_Size (File, Size.Line_Length, Size.Page_Length);
   end Set_Size;

   procedure Set_Size (File : File_Type; Line_Length, Page_Length : Count) is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Check_File_Mode (NC_File, IO_Modes.Out_File);
      System.Native_Text_IO.Set_Terminal_Size (
         Naked_Text_IO.Terminal_Handle (NC_File),
         Integer (Line_Length),
         Integer (Page_Length));
   end Set_Size;

   function Size (File : File_Type) return Size_Type is
   begin
      return Result : Size_Type do
         Size (File, Result.Line_Length, Result.Page_Length);
      end return;
   end Size;

   procedure Size (File : File_Type; Line_Length, Page_Length : out Count) is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Check_File_Mode (NC_File, IO_Modes.Out_File);
      System.Native_Text_IO.Terminal_Size (
         Naked_Text_IO.Terminal_Handle (NC_File),
         Natural'Base (Line_Length),
         Natural'Base (Page_Length));
   end Size;

   function View (File : File_Type) return View_Type is
   begin
      return R : View_Type do
         View (File, R.Left, R.Top, R.Right, R.Bottom);
      end return;
   end View;

   procedure View (
      File : File_Type;
      Left, Top : out Positive_Count;
      Right, Bottom : out Count)
   is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Check_File_Mode (NC_File, IO_Modes.Out_File);
      System.Native_Text_IO.Terminal_View (
         Naked_Text_IO.Terminal_Handle (NC_File),
         Positive'Base (Left),
         Positive'Base (Top),
         Natural'Base (Right),
         Natural'Base (Bottom));
   end View;

   procedure Set_Position (File : File_Type; Position : Position_Type) is
   begin
      Set_Position (File, Position.Col, Position.Line);
   end Set_Position;

   procedure Set_Position (File : File_Type; Col, Line : Positive_Count) is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Check_File_Mode (NC_File, IO_Modes.Out_File);
      System.Native_Text_IO.Set_Terminal_Position (
         Naked_Text_IO.Terminal_Handle (NC_File),
         Integer (Col),
         Integer (Line));
   end Set_Position;

   procedure Set_Col (File : File_Type; To : Positive_Count) is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Check_File_Mode (NC_File, IO_Modes.Out_File);
      System.Native_Text_IO.Set_Terminal_Col (
         Naked_Text_IO.Terminal_Handle (NC_File),
         Integer (To));
   end Set_Col;

   function Position (File : File_Type) return Position_Type is
   begin
      return Result : Position_Type do
         Position (
            File,
            Result.Col,
            Result.Line);
      end return;
   end Position;

   procedure Position (File : File_Type; Col, Line : out Positive_Count) is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Check_File_Mode (NC_File, IO_Modes.Out_File);
      System.Native_Text_IO.Terminal_Position (
         Naked_Text_IO.Terminal_Handle (NC_File),
         Positive'Base (Col),
         Positive'Base (Line));
   end Position;

end Ada.Text_IO.Terminal;
