pragma License (Unrestricted);
with Ada.Formatting;
with Ada.IO_Exceptions;
with Ada.IO_Modes;
with Ada.IO_Text_Modes;
private with Ada.Finalization;
limited private with Ada.Text_IO.Inside;
package Ada.Text_IO is

   type File_Type is limited private;
   type File_Access is access constant File_Type; -- moved from below
   for File_Access'Storage_Size use 0; -- modified

--  type File_Mode is (In_File, Out_File, Append_File);
   type File_Mode is new IO_Modes.File_Mode; -- for conversion

   type Count is range 0 .. Natural'Last;
   subtype Positive_Count is Count range 1 .. Count'Last;
   Unbounded : constant Count := 0;

   subtype Field is Integer range 0 .. 255; -- implementation-defined
   subtype Number_Base is Integer range 2 .. 16;

--  type Type_Set is (Lower_Case, Upper_Case);
   type Type_Set is new Formatting.Type_Set;

   --  File Management

   --  modified
   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String); -- removed default
   --  extended
   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      External : IO_Text_Modes.File_External_Encoding := IO_Text_Modes.Locale;
      New_Line : IO_Text_Modes.File_New_Line := IO_Text_Modes.By_Target;
      SUB : IO_Text_Modes.File_SUB := IO_Text_Modes.Ordinary);
   function Create (
      Mode : File_Mode := Out_File;
      Name : String := "";
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      External : IO_Text_Modes.File_External_Encoding := IO_Text_Modes.Locale;
      New_Line : IO_Text_Modes.File_New_Line := IO_Text_Modes.By_Target;
      SUB : IO_Text_Modes.File_SUB := IO_Text_Modes.Ordinary)
      return File_Type;
   pragma Inline (Create);

   --  modified
   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String); -- removed default
   --  extended
   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      External : IO_Text_Modes.File_External_Encoding := IO_Text_Modes.Locale;
      New_Line : IO_Text_Modes.File_New_Line := IO_Text_Modes.By_Target;
      SUB : IO_Text_Modes.File_SUB := IO_Text_Modes.Ordinary);
   function Open (
      Mode : File_Mode;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      External : IO_Text_Modes.File_External_Encoding := IO_Text_Modes.Locale;
      New_Line : IO_Text_Modes.File_New_Line := IO_Text_Modes.By_Target;
      SUB : IO_Text_Modes.File_SUB := IO_Text_Modes.Ordinary)
      return File_Type;
   pragma Inline (Open);

   procedure Close (File : in out File_Type);
   procedure Delete (File : in out File_Type);
   procedure Reset (File : in out File_Type; Mode : File_Mode);
   procedure Reset (File : in out File_Type);

   function Mode (File : File_Type) return File_Mode;
   pragma Inline (Mode);
   function Name (File : File_Type) return String;
   function Name (File : not null File_Access) return String; -- alt
   pragma Inline (Name);
   function Form (File : File_Type) return String;

   function Is_Open (File : File_Type) return Boolean;
   function Is_Open (File : not null File_Access) return Boolean; -- alt
   pragma Inline (Is_Open);

   --  Control of default input and output files

   procedure Set_Input (File : File_Type);
   procedure Set_Input (File : not null File_Access); -- alt
   procedure Set_Output (File : File_Type);
   procedure Set_Output (File : not null File_Access); -- alt
   procedure Set_Error (File : File_Type);
   procedure Set_Error (File : not null File_Access); -- alt

   --  Wait for Implicit_Dereference since File_Type is limited (marked "alt")
--  function Standard_Input return File_Type;
--  function Standard_Output return File_Type;
--  function Standard_Error return File_Type;

--  function Current_Input return File_Type;
--  function Current_Output return File_Type;
--  function Current_Error return File_Type;

--  type File_Access is access constant File_Type;
   --  declarated in above

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
   procedure Flush;

   --  Specification of line and page lengths

   procedure Set_Line_Length (File : File_Type; To : Count);
   procedure Set_Line_Length (To : Count);
   procedure Set_Line_Length (File : not null File_Access; To : Count); -- alt

   procedure Set_Page_Length (File : File_Type; To : Count);
   procedure Set_Page_Length (To : Count);
   procedure Set_Page_Length (File : not null File_Access; To : Count); -- alt

   function Line_Length (File : File_Type) return Count;
   function Line_Length return Count;
   pragma Inline (Line_Length);

   function Page_Length (File : File_Type) return Count;
   function Page_Length return Count;
   pragma Inline (Page_Length);

   --  Column, Line, and Page Control

   procedure New_Line (File : File_Type; Spacing : Positive_Count := 1);
   procedure New_Line (Spacing : Positive_Count := 1);
   procedure New_Line (
      File : not null File_Access;
      Spacing : Positive_Count := 1); -- alt

   procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1);
   procedure Skip_Line (Spacing : Positive_Count := 1);
   procedure Skip_Line (
      File : not null File_Access;
      Spacing : Positive_Count := 1); -- alt

   function End_Of_Line (File : File_Type) return Boolean;
   function End_Of_Line return Boolean;
   pragma Inline (End_Of_Line);

   procedure New_Page (File : File_Type);
   procedure New_Page;
   procedure New_Page (File : not null File_Access); -- alt

   procedure Skip_Page (File : File_Type);
   procedure Skip_Page;
   procedure Skip_Page (File : not null File_Access); -- alt

   function End_Of_Page (File : File_Type) return Boolean;
   function End_Of_Page return Boolean;
   function End_Of_Page (File : not null File_Access) return Boolean; -- alt
   pragma Inline (End_Of_Page);

   function End_Of_File (File : File_Type) return Boolean;
   function End_Of_File return Boolean;
   function End_Of_File (File : not null File_Access) return Boolean; -- alt
   pragma Inline (End_Of_File);

   procedure Set_Col (File : File_Type; To : Positive_Count);
   procedure Set_Col (To : Positive_Count);
   procedure Set_Col (File : not null File_Access; To : Positive_Count); -- alt

   procedure Set_Line (File : File_Type; To : Positive_Count);
   procedure Set_Line (To : Positive_Count);
   procedure Set_Line (
      File : not null File_Access;
      To : Positive_Count); -- alt

   function Col (File : File_Type) return Positive_Count;
   function Col return Positive_Count;
   function Col (File : not null File_Access) return Positive_Count; -- alt
   pragma Inline (Col);

   function Line (File : File_Type) return Positive_Count;
   function Line return Positive_Count;
   function Line (File : not null File_Access) return Positive_Count; -- alt
   pragma Inline (Line);

   function Page (File : File_Type) return Positive_Count;
   function Page return Positive_Count;
   function Page (File : not null File_Access) return Positive_Count; -- alt
   pragma Inline (Page);

   --  Character Input-Output

   procedure Get (File : File_Type; Item : out Character);
   procedure Get (Item : out Character);
   procedure Get (File : not null File_Access; Item : out Character); -- alt

   procedure Put (File : File_Type; Item : Character);
   procedure Put (Item : Character);
   procedure Put (File : not null File_Access; Item : Character); -- alt

   procedure Look_Ahead (
      File : File_Type;
      Item : out Character;
      End_Of_Line : out Boolean);
   procedure Look_Ahead (
      Item : out Character;
      End_Of_Line : out Boolean);

   procedure Get_Immediate (File : File_Type; Item : out Character);
   procedure Get_Immediate (Item : out Character);

   procedure Get_Immediate (
      File : File_Type;
      Item : out Character;
      Available : out Boolean);
   procedure Get_Immediate (
      Item : out Character;
      Available : out Boolean);

   --  String Input-Output

   procedure Get (File : File_Type; Item : out String);
   procedure Get (Item : out String);
   procedure Get (File : not null File_Access; Item : out String); -- alt

   procedure Put (File : File_Type; Item : String);
   procedure Put (Item : String);
   procedure Put (File : not null File_Access; Item : String); -- alt

   procedure Get_Line (
      File : File_Type;
      Item : out String;
      Last : out Natural);
   procedure Get_Line (
      Item : out String;
      Last : out Natural);
   procedure Get_Line (
      File : not null File_Access;
      Item : out String;
      Last : out Natural); -- alt

   function Get_Line (File : File_Type) return String;
   function Get_Line return String;

   procedure Put_Line (File : File_Type; Item : String);
   procedure Put_Line (Item : String);
   procedure Put_Line (File : not null File_Access; Item : String); -- alt

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

private

   package Controlled is

      type File_Type is limited private;
      type File_Access is access constant File_Type;
      for File_Access'Storage_Size use 0;

      function Standard_Input return File_Access;
      pragma Inline (Standard_Input);
      function Standard_Output return File_Access;
      pragma Inline (Standard_Output);
      function Standard_Error return File_Access;
      pragma Inline (Standard_Error);

      function Reference_Current_Input return access File_Access;
      pragma Inline (Reference_Current_Input);
      function Reference_Current_Output return access File_Access;
      pragma Inline (Reference_Current_Output);
      function Reference_Current_Error return access File_Access;
      pragma Inline (Reference_Current_Error);

      function Reference (File : File_Type)
         return access Inside.Non_Controlled_File_Type;
      pragma Inline (Reference);

   private

      type File_Type is
         limited new Finalization.Limited_Controlled with
      record
         Text : access Inside.Text_Type;
      end record;

      overriding procedure Finalize (Object : in out File_Type);

   end Controlled;

   type File_Type is new Controlled.File_Type;

end Ada.Text_IO;
