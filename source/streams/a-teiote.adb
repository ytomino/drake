with Ada.Naked_Text_IO;
package body Ada.Text_IO.Terminal is
   use type IO_Text_Modes.File_External;

   function Is_Terminal (File : File_Type) return Boolean is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      return Naked_Text_IO.External (NC_File) = IO_Text_Modes.Terminal;
   end Is_Terminal;

   procedure Set_Size (File : File_Type; Size : Size_Type) is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Naked_Text_IO.Set_Size (
         NC_File,
         Integer (Size.Line_Length),
         Integer (Size.Page_Length));
   end Set_Size;

   procedure Set_Size (File : File_Type; Line_Length, Page_Length : Count) is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Naked_Text_IO.Set_Size (NC_File,
         Integer (Line_Length),
         Integer (Page_Length));
   end Set_Size;

   function Size (File : File_Type) return Size_Type is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      return Result : Size_Type do
         Naked_Text_IO.Size (
            NC_File,
            Natural'Base (Result.Line_Length),
            Natural'Base (Result.Page_Length));
      end return;
   end Size;

   procedure Size (File : File_Type; Line_Length, Page_Length : out Count) is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Naked_Text_IO.Size (
         NC_File,
         Natural'Base (Line_Length),
         Natural'Base (Page_Length));
   end Size;

   function View (File : File_Type) return View_Type is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      return R : View_Type do
         Naked_Text_IO.View (
            NC_File,
            Positive'Base (R.Left),
            Positive'Base (R.Top),
            Natural'Base (R.Right),
            Natural'Base (R.Bottom));
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
      Naked_Text_IO.View (
         NC_File,
         Positive'Base (Left),
         Positive'Base (Top),
         Natural'Base (Right),
         Natural'Base (Bottom));
   end View;

   procedure Set_Position (File : File_Type; Position : Position_Type) is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Naked_Text_IO.Set_Position_Within_Terminal (
         NC_File,
         Integer (Position.Col),
         Integer (Position.Line));
   end Set_Position;

   procedure Set_Position (File : File_Type; Col, Line : Positive_Count) is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Naked_Text_IO.Set_Position_Within_Terminal (
         NC_File,
         Integer (Col),
         Integer (Line));
   end Set_Position;

   procedure Set_Col (File : File_Type; To : Positive_Count) is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Naked_Text_IO.Set_Col_Within_Terminal (NC_File, Integer (To));
   end Set_Col;

   function Position (File : File_Type) return Position_Type is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      return Result : Position_Type do
         Naked_Text_IO.Position (
            NC_File,
            Positive'Base (Result.Col),
            Positive'Base (Result.Line));
      end return;
   end Position;

   procedure Position (File : File_Type; Col, Line : out Positive_Count) is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Naked_Text_IO.Position (
         NC_File,
         Positive'Base (Col),
         Positive'Base (Line));
   end Position;

end Ada.Text_IO.Terminal;
