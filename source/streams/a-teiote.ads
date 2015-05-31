pragma License (Unrestricted);
--  extended unit
package Ada.Text_IO.Terminal is
   --  Additional terminal handling subprograms.

   function Is_Terminal (
      File : File_Type) -- Open_File_Type
      return Boolean;

   --  size of screen buffer and view port

   type Size_Type is record
      Line_Length : Count;
      Page_Length : Count;
   end record;
   pragma Suppress_Initialization (Size_Type);

   procedure Set_Size (
      File : File_Type; -- Output_File_Type
      Size : Size_Type);
   procedure Set_Size (
      File : File_Type; -- Output_File_Type
      Line_Length, Page_Length : Count);

   function Size (
      File : File_Type) -- Output_File_Type
      return Size_Type;
   procedure Size (
      File : File_Type; -- Output_File_Type
      Line_Length, Page_Length : out Count);

   type View_Type is record
      Left : Positive_Count;
      Top : Positive_Count;
      Right : Count;
      Bottom : Count;
   end record;
   pragma Suppress_Initialization (View_Type);

   function View (
      File : File_Type) -- Output_File_Type
      return View_Type;
   procedure View (
      File : File_Type; -- Output_File_Type
      Left, Top : out Positive_Count;
      Right, Bottom : out Count);

   --  cursor position

   type Position_Type is record
      Col : Positive_Count;
      Line : Positive_Count;
   end record;
   pragma Suppress_Initialization (Position_Type);

   procedure Set_Position (
      File : File_Type; -- Output_File_Type
      Position : Position_Type);
   procedure Set_Position (
      File : File_Type; -- Output_File_Type
      Col, Line : Positive_Count);
   procedure Set_Col (
      File : File_Type; -- Output_File_Type
      To : Positive_Count);

   function Position (
      File : File_Type) -- Output_File_Type
      return Position_Type;
   procedure Position (
      File : File_Type; -- Output_File_Type
      Col, Line : out Positive_Count);

end Ada.Text_IO.Terminal;
