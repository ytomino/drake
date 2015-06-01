with Ada.Naked_Text_IO;
with System.Native_Text_IO;
package body Ada.Text_IO.Terminal is
   use type IO_Modes.File_Mode;
   use type IO_Modes.File_External;

   --  implementation

   function Is_Terminal (
      File : File_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      return Naked_Text_IO.External (NC_File) = IO_Modes.Terminal;
   end Is_Terminal;

   procedure Set_Size (
      File : File_Type;
      Size : Size_Type) is
   begin
      Set_Size (
         File, -- checking the predicate
         Size.Line_Length,
         Size.Page_Length);
   end Set_Size;

   procedure Set_Size (
      File : File_Type;
      Line_Length, Page_Length : Count)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= In_File or else raise Mode_Error);
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      System.Native_Text_IO.Set_Terminal_Size (
         Naked_Text_IO.Terminal_Handle (NC_File),
         Integer (Line_Length),
         Integer (Page_Length));
   end Set_Size;

   function Size (
      File : File_Type)
      return Size_Type is
   begin
      return Result : Size_Type do
         Size (
            File, -- checking the predicate
            Result.Line_Length,
            Result.Page_Length);
      end return;
   end Size;

   procedure Size (
      File : File_Type;
      Line_Length, Page_Length : out Count)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= In_File or else raise Mode_Error);
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      System.Native_Text_IO.Terminal_Size (
         Naked_Text_IO.Terminal_Handle (NC_File),
         Natural'Base (Line_Length),
         Natural'Base (Page_Length));
   end Size;

   function View (
      File : File_Type)
      return View_Type is
   begin
      return R : View_Type do
         View (
            File, -- checking the predicate
            R.Left,
            R.Top,
            R.Right,
            R.Bottom);
      end return;
   end View;

   procedure View (
      File : File_Type;
      Left, Top : out Positive_Count;
      Right, Bottom : out Count)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= In_File or else raise Mode_Error);
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      System.Native_Text_IO.Terminal_View (
         Naked_Text_IO.Terminal_Handle (NC_File),
         Positive'Base (Left),
         Positive'Base (Top),
         Natural'Base (Right),
         Natural'Base (Bottom));
   end View;

   procedure Set_Position (
      File : File_Type;
      Position : Position_Type) is
   begin
      Set_Position (
         File, -- checking the predicate
         Position.Col,
         Position.Line);
   end Set_Position;

   procedure Set_Position (
      File : File_Type;
      Col, Line : Positive_Count)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= In_File or else raise Mode_Error);
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      System.Native_Text_IO.Set_Terminal_Position (
         Naked_Text_IO.Terminal_Handle (NC_File),
         Integer (Col),
         Integer (Line));
   end Set_Position;

   procedure Set_Col (
      File : File_Type;
      To : Positive_Count)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= In_File or else raise Mode_Error);
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      System.Native_Text_IO.Set_Terminal_Col (
         Naked_Text_IO.Terminal_Handle (NC_File),
         Integer (To));
   end Set_Col;

   function Position (
      File : File_Type)
      return Position_Type is
   begin
      return Result : Position_Type do
         Position (
            File, -- checking the predicate
            Result.Col,
            Result.Line);
      end return;
   end Position;

   procedure Position (
      File : File_Type;
      Col, Line : out Positive_Count)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= In_File or else raise Mode_Error);
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Reference (File).all;
   begin
      System.Native_Text_IO.Terminal_Position (
         Naked_Text_IO.Terminal_Handle (NC_File),
         Positive'Base (Col),
         Positive'Base (Line));
   end Position;

end Ada.Text_IO.Terminal;
