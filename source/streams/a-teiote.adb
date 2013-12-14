with Ada.Text_IO.Inside;
package body Ada.Text_IO.Terminal is
   use type IO_Text_Modes.File_External;

   function Is_Terminal (File : File_Type) return Boolean is
      NC_File : Inside.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      return Inside.External (NC_File) = IO_Text_Modes.Terminal;
   end Is_Terminal;

   procedure Set_Size (File : File_Type; Size : Size_Type) is
      NC_File : Inside.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Inside.Set_Size (NC_File, Size.Line_Length, Size.Page_Length);
   end Set_Size;

   procedure Set_Size (File : File_Type; Line_Length, Page_Length : Count) is
      NC_File : Inside.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Inside.Set_Size (NC_File, Line_Length, Page_Length);
   end Set_Size;

   function Size (File : File_Type) return Size_Type is
      NC_File : Inside.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      return Result : Size_Type do
         Inside.Size (NC_File, Result.Line_Length, Result.Page_Length);
      end return;
   end Size;

   procedure Size (File : File_Type; Line_Length, Page_Length : out Count) is
      NC_File : Inside.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Inside.Size (NC_File, Line_Length, Page_Length);
   end Size;

   function View (File : File_Type) return View_Type is
      NC_File : Inside.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      return R : View_Type do
         Inside.View (NC_File, R.Left, R.Top, R.Right, R.Bottom);
      end return;
   end View;

   procedure View (
      File : File_Type;
      Left, Top : out Positive_Count;
      Right, Bottom : out Count)
   is
      NC_File : Inside.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Inside.View (NC_File, Left, Top, Right, Bottom);
   end View;

   procedure Set_Position (File : File_Type; Position : Position_Type) is
      NC_File : Inside.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Inside.Set_Position_Within_Terminal (
         NC_File,
         Position.Col,
         Position.Line);
   end Set_Position;

   procedure Set_Position (File : File_Type; Col, Line : Positive_Count) is
      NC_File : Inside.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Inside.Set_Position_Within_Terminal (NC_File, Col, Line);
   end Set_Position;

   procedure Set_Col (File : File_Type; To : Positive_Count) is
      NC_File : Inside.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Inside.Set_Col_Within_Terminal (NC_File, To);
   end Set_Col;

   function Position (File : File_Type) return Position_Type is
      NC_File : Inside.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      return Result : Position_Type do
         Inside.Position (NC_File, Result.Col, Result.Line);
      end return;
   end Position;

   procedure Position (File : File_Type; Col, Line : out Positive_Count) is
      NC_File : Inside.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      Inside.Position (NC_File, Col, Line);
   end Position;

end Ada.Text_IO.Terminal;
