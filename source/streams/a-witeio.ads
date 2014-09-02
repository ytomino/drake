pragma License (Unrestricted);
with Ada.IO_Exceptions;
with Ada.Text_IO;
package Ada.Wide_Text_IO is

   --  modified
--  type File_Type is limited private;
   subtype File_Type is Text_IO.File_Type;

--  type File_Mode is (In_File, Out_File, Append_File);
   subtype File_Mode is Text_IO.File_Mode;
   function In_File return File_Mode
      renames Text_IO.In_File;
   function Out_File return File_Mode
      renames Text_IO.Out_File;
   function Append_File return File_Mode
      renames Text_IO.Append_File;

--  type Count is range 0 .. implementation-defined;
   subtype Count is Text_IO.Count;
   subtype Positive_Count is Count range 1 .. Count'Last;
   Unbounded : constant Count := 0;

   subtype Field is
      Integer range 0 .. Text_IO.Field'Last; -- implementation-defined
   subtype Number_Base is Integer range 2 .. 16;

--  type Type_Set is (Lower_Case, Upper_Case);
   subtype Type_Set is Text_IO.Type_Set;
   function Lower_Case return Type_Set
      renames Text_IO.Lower_Case;
   function Upper_Case return Type_Set
      renames Text_IO.Upper_Case;

   --  File Management

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "")
      renames Text_IO.Create;

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "")
      renames Text_IO.Open;

   procedure Close (File : in out File_Type)
      renames Text_IO.Close;
   procedure Delete (File : in out File_Type)
      renames Text_IO.Delete;
   procedure Reset (File : in out File_Type; Mode : File_Mode)
      renames Text_IO.Reset;
   procedure Reset (File : in out File_Type)
      renames Text_IO.Reset;

   function Mode (File : File_Type) return File_Mode
      renames Text_IO.Mode;
   function Name (File : File_Type) return String
      renames Text_IO.Name;
   function Form (File : File_Type) return String
      renames Text_IO.Form;

   function Is_Open (File : File_Type) return Boolean
      renames Text_IO.Is_Open;

   --  Control of default input and output files

   procedure Set_Input (File : File_Type)
      renames Text_IO.Set_Input;
   procedure Set_Output (File : File_Type)
      renames Text_IO.Set_Output;
   procedure Set_Error (File : File_Type)
      renames Text_IO.Set_Error;

--  function Standard_Input return File_Type;
--  function Standard_Output return File_Type;
--  function Standard_Error return File_Type;

--  function Current_Input return File_Type;
--  function Current_Output return File_Type;
--  function Current_Error return File_Type;

--  type File_Access is access constant File_Type;
   subtype File_Access is Text_IO.File_Access;

   function Standard_Input return File_Access
      renames Text_IO.Standard_Input;
   function Standard_Output return File_Access
      renames Text_IO.Standard_Output;
   function Standard_Error return File_Access
      renames Text_IO.Standard_Error;

   function Current_Input return File_Access
      renames Text_IO.Current_Input;
   function Current_Output return File_Access
      renames Text_IO.Current_Output;
   function Current_Error return File_Access
      renames Text_IO.Current_Error;

   --  Buffer control
   procedure Flush (File : File_Type)
      renames Text_IO.Flush;
   procedure Flush
      renames Text_IO.Flush;

   --  Specification of line and page lengths

   procedure Set_Line_Length (File : File_Type; To : Count)
      renames Text_IO.Set_Line_Length;
   procedure Set_Line_Length (To : Count)
      renames Text_IO.Set_Line_Length;

   procedure Set_Page_Length (File : File_Type; To : Count)
      renames Text_IO.Set_Page_Length;
   procedure Set_Page_Length (To : Count)
      renames Text_IO.Set_Page_Length;

   function Line_Length (File : File_Type) return Count
      renames Text_IO.Line_Length;
   function Line_Length return Count
      renames Text_IO.Line_Length;

   function Page_Length (File : File_Type) return Count
      renames Text_IO.Page_Length;
   function Page_Length return Count
      renames Text_IO.Page_Length;

   --  Column, Line, and Page Control

   procedure New_Line (File : File_Type; Spacing : Positive_Count := 1)
      renames Text_IO.New_Line;
   procedure New_Line (Spacing : Positive_Count := 1)
      renames Text_IO.New_Line;

   procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1)
      renames Text_IO.Skip_Line;
   procedure Skip_Line (Spacing : Positive_Count := 1)
      renames Text_IO.Skip_Line;

   function End_Of_Line (File : File_Type) return Boolean
      renames Text_IO.End_Of_Line;
   function End_Of_Line return Boolean
      renames Text_IO.End_Of_Line;

   procedure New_Page (File : File_Type)
      renames Text_IO.New_Page;
   procedure New_Page
      renames Text_IO.New_Page;

   procedure Skip_Page (File : File_Type)
      renames Text_IO.Skip_Page;
   procedure Skip_Page
      renames Text_IO.Skip_Page;

   function End_Of_Page (File : File_Type) return Boolean
      renames Text_IO.End_Of_Page;
   function End_Of_Page return Boolean
      renames Text_IO.End_Of_Page;

   function End_Of_File (File : File_Type) return Boolean
      renames Text_IO.End_Of_File;
   function End_Of_File return Boolean
      renames Text_IO.End_Of_File;

   procedure Set_Col (File : File_Type; To : Positive_Count)
      renames Text_IO.Set_Col;
   procedure Set_Col (To : Positive_Count)
      renames Text_IO.Set_Col;

   procedure Set_Line (File : File_Type; To : Positive_Count)
      renames Text_IO.Set_Line;
   procedure Set_Line (To : Positive_Count)
      renames Text_IO.Set_Line;

   function Col (File : File_Type) return Positive_Count
      renames Text_IO.Col;
   function Col return Positive_Count
      renames Text_IO.Col;

   function Line (File : File_Type) return Positive_Count
      renames Text_IO.Line;
   function Line return Positive_Count
      renames Text_IO.Line;

   function Page (File : File_Type) return Positive_Count
      renames Text_IO.Page;
   function Page return Positive_Count
      renames Text_IO.Page;

   --  Character Input-Output

   procedure Get (File : File_Type; Item : out Wide_Character)
      renames Text_IO.Overloaded_Get;
   procedure Get (Item : out Wide_Character);

   procedure Put (File : File_Type; Item : Wide_Character)
      renames Text_IO.Overloaded_Put;
   procedure Put (Item : Wide_Character);

   procedure Look_Ahead (
      File : File_Type;
      Item : out Wide_Character;
      End_Of_Line : out Boolean)
      renames Text_IO.Overloaded_Look_Ahead;
   procedure Look_Ahead (
      Item : out Wide_Character;
      End_Of_Line : out Boolean);

   procedure Get_Immediate (File : File_Type; Item : out Wide_Character)
      renames Text_IO.Overloaded_Get_Immediate;
   procedure Get_Immediate (Item : out Wide_Character);

   procedure Get_Immediate (
      File : File_Type;
      Item : out Wide_Character;
      Available : out Boolean)
      renames Text_IO.Overloaded_Get_Immediate;
   procedure Get_Immediate (
      Item : out Wide_Character;
      Available : out Boolean);

   --  String Input-Output

   procedure Get (File : File_Type; Item : out Wide_String)
      renames Text_IO.Overloaded_Get;
   procedure Get (Item : out Wide_String);

   procedure Put (File : File_Type; Item : Wide_String)
      renames Text_IO.Overloaded_Put;
   procedure Put (Item : Wide_String);

   procedure Get_Line (
      File : File_Type;
      Item : out Wide_String;
      Last : out Natural)
      renames Text_IO.Overloaded_Get_Line;
   procedure Get_Line (
      Item : out Wide_String;
      Last : out Natural);

   function Get_Line (File : File_Type) return Wide_String
      renames Text_IO.Overloaded_Get_Line;
   function Get_Line return Wide_String;

   procedure Put_Line (File : File_Type; Item : Wide_String)
      renames Text_IO.Overloaded_Put_Line;
   procedure Put_Line (Item : Wide_String);

   --  Generic packages for Input-Output of Integer Types
   --  Generic packages for Input-Output of Real Types
   --  Generic package for Input-Output of Enumeration Types

   --  Integer_IO, Modular_IO, Float_IO, Fixed_IO, Decimal_IO, Enumeration_IO
   --  are separated by compiler

   --  Exceptions

   Status_Error : exception
      renames IO_Exceptions.Status_Error;
   Mode_Error : exception
      renames IO_Exceptions.Mode_Error;
   Name_Error : exception
      renames IO_Exceptions.Name_Error;
   Use_Error : exception
      renames IO_Exceptions.Use_Error;
   Device_Error : exception
      renames IO_Exceptions.Device_Error;
   End_Error : exception
      renames IO_Exceptions.End_Error;
   Data_Error : exception
      renames IO_Exceptions.Data_Error;
   Layout_Error : exception
      renames IO_Exceptions.Layout_Error;

end Ada.Wide_Text_IO;
