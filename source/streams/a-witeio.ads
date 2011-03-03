pragma License (Unrestricted);
with Ada.IO_Exceptions;
with Ada.IO_Modes;
with Ada.Text_IO;
package Ada.Wide_Text_IO is

   type File_Type is limited private;

--  type File_Mode is (In_File, Out_File, Append_File);
   type File_Mode is new IO_Modes.File_Mode;

   type Count is range 0 .. Text_IO.Count'Last; --  implementation-defined
   subtype Positive_Count is Count range 1 .. Count'Last;
   Unbounded : constant Count := 0;

   subtype Field is Integer range
      0 ..
      Text_IO.Field'Last; --  implementation-defined
   subtype Number_Base is Integer range 2 .. 16;

--  type Type_Set is (Lower_Case, Upper_Case);
   type Type_Set is new Text_IO.Type_Set; --  has no additional primitive

   --  File Management

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "");
   pragma Inline (Create);

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "");
   pragma Inline (Open);

   procedure Close (File : in out File_Type);
   pragma Inline (Close);
   procedure Delete (File : in out File_Type);
   pragma Inline (Delete);
   procedure Reset (File : in out File_Type; Mode : File_Mode);
   pragma Inline (Reset);
   procedure Reset (File : in out File_Type);
   pragma Inline (Reset);

   function Mode (File : File_Type) return File_Mode;
   pragma Inline (Mode);
   function Name (File : File_Type) return String;
   pragma Inline (Name);
   function Form (File : File_Type) return String;
   pragma Inline (Form);

   function Is_Open (File : File_Type) return Boolean;
   pragma Inline (Is_Open);

   --  Control of default input and output files

   procedure Set_Input (File : File_Type);
   pragma Inline (Set_Input);
   procedure Set_Output (File : File_Type);
   pragma Inline (Set_Output);
   procedure Set_Error (File : File_Type);
   pragma Inline (Set_Error);

--  function Standard_Input return File_Type;
--  function Standard_Output return File_Type;
--  function Standard_Error return File_Type;

--  function Current_Input return File_Type;
--  function Current_Output return File_Type;
--  function Current_Error return File_Type;

   type File_Access is access constant File_Type;

   function Standard_Input return File_Access;
   pragma Inline (Standard_Input);
   function Standard_Output return File_Access;
   pragma Inline (Standard_Output);
   function Standard_Error return File_Access;
   pragma Inline (Standard_Error);

   function Current_Input return File_Access;
   pragma Inline (Current_Input);
   function Current_Output return File_Access;
   pragma Inline (Current_Output);
   function Current_Error return File_Access;
   pragma Inline (Current_Error);

   --  Buffer control
   procedure Flush (File : File_Type);
   pragma Inline (Flush);
   procedure Flush;
   pragma Inline (Flush);

   --  Specification of line and page lengths

   procedure Set_Line_Length (File : File_Type; To : Count);
   pragma Inline (Set_Line_Length);
   procedure Set_Line_Length (To : Count);
   pragma Inline (Set_Line_Length);

   procedure Set_Page_Length (File : File_Type; To : Count);
   pragma Inline (Set_Page_Length);
   procedure Set_Page_Length (To : Count);
   pragma Inline (Set_Page_Length);

   function Line_Length (File : File_Type) return Count;
   pragma Inline (Line_Length);
   function Line_Length return Count;
   pragma Inline (Line_Length);

   function Page_Length (File : File_Type) return Count;
   pragma Inline (Page_Length);
   function Page_Length return Count;
   pragma Inline (Page_Length);

   --  Column, Line, and Page Control

   procedure New_Line (File : File_Type; Spacing : Positive_Count := 1);
   pragma Inline (New_Line);
   procedure New_Line (Spacing : Positive_Count := 1);
   pragma Inline (New_Line);

   procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1);
   pragma Inline (Skip_Line);
   procedure Skip_Line (Spacing : Positive_Count := 1);
   pragma Inline (Skip_Line);

   function End_Of_Line (File : File_Type) return Boolean;
   pragma Inline (End_Of_Line);
   function End_Of_Line return Boolean;
   pragma Inline (End_Of_Line);

   procedure New_Page (File : File_Type);
   pragma Inline (New_Page);
   procedure New_Page;
   pragma Inline (New_Page);

   procedure Skip_Page (File : File_Type);
   pragma Inline (Skip_Page);
   procedure Skip_Page;
   pragma Inline (Skip_Page);

   function End_Of_Page (File : File_Type) return Boolean;
   pragma Inline (End_Of_Page);
   function End_Of_Page return Boolean;
   pragma Inline (End_Of_Page);

   function End_Of_File (File : File_Type) return Boolean;
   pragma Inline (End_Of_File);
   function End_Of_File return Boolean;
   pragma Inline (End_Of_File);

   procedure Set_Col (File : File_Type; To : Positive_Count);
   pragma Inline (Set_Col);
   procedure Set_Col (To : Positive_Count);
   pragma Inline (Set_Col);

   procedure Set_Line (File : File_Type; To : Positive_Count);
   pragma Inline (Set_Line);
   procedure Set_Line (To : Positive_Count);
   pragma Inline (Set_Line);

   function Col (File : File_Type) return Positive_Count;
   pragma Inline (Col);
   function Col return Positive_Count;
   pragma Inline (Col);

   function Line (File : File_Type) return Positive_Count;
   pragma Inline (Line);
   function Line return Positive_Count;
   pragma Inline (Line);

   function Page (File : File_Type) return Positive_Count;
   pragma Inline (Page);
   function Page return Positive_Count;
   pragma Inline (Page);

   --  Character Input-Output

   procedure Get (File : File_Type; Item : out Wide_Character);
   procedure Get (Item : out Wide_Character);

   procedure Put (File : File_Type; Item : Wide_Character);
   procedure Put (Item : Wide_Character);

   procedure Look_Ahead (
      File : File_Type;
      Item : out Wide_Character;
      End_Of_Line : out Boolean);
   procedure Look_Ahead (
      Item : out Wide_Character;
      End_Of_Line : out Boolean);

   procedure Get_Immediate (File : File_Type; Item : out Wide_Character);
   procedure Get_Immediate (Item : out Wide_Character);

   procedure Get_Immediate (
      File : File_Type;
      Item : out Wide_Character;
      Available : out Boolean);
   procedure Get_Immediate (
      Item : out Wide_Character;
      Available : out Boolean);

   --  String Input-Output

   procedure Get (File : File_Type; Item : out Wide_String);
   procedure Get (Item : out Wide_String);

   procedure Put (File : File_Type; Item : Wide_String);
   procedure Put (Item : Wide_String);

   procedure Get_Line (
      File : File_Type;
      Item : out Wide_String;
      Last : out Natural);
   procedure Get_Line (
      Item : out Wide_String;
      Last : out Natural);

   function Get_Line (File : File_Type) return Wide_String;
   function Get_Line return Wide_String;

   procedure Put_Line (File : File_Type; Item : Wide_String);
   procedure Put_Line (Item : Wide_String);

   --  Generic packages for Input-Output of Integer Types
   --  Generic packages for Input-Output of Real Types
   --  Generic package for Input-Output of Enumeration Types

   --  Integer_IO, Modular_IO, Float_IO, Fixed_IO, Decimal_IO, Enumeration_IO
   --  are separated by compiler

   --  Exceptions

   Status_Error : exception renames IO_Exceptions.Status_Error;
   Mode_Error : exception renames IO_Exceptions.Mode_Error;
   Name_Error : exception renames IO_Exceptions.Name_Error;
   Use_Error : exception renames IO_Exceptions.Use_Error;
   Device_Error : exception renames IO_Exceptions.Device_Error;
   End_Error : exception renames IO_Exceptions.End_Error;
   Data_Error : exception renames IO_Exceptions.Data_Error;
   Layout_Error : exception renames IO_Exceptions.Layout_Error;

private

   type File_Type is new Text_IO.File_Type;

   --  Hide inherited subprogram
   overriding procedure Put (File : File_Type; Item : Character) is abstract;

end Ada.Wide_Text_IO;
