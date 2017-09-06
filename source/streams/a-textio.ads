pragma License (Unrestricted);
with Ada.Formatting;
with Ada.IO_Exceptions;
with Ada.IO_Modes;
with Ada.Unchecked_Deallocation;
private with Ada.Finalization;
private with Ada.Naked_Text_IO;
package Ada.Text_IO is

   type File_Type is limited private;
   type File_Access is access constant File_Type; -- moved from below
   for File_Access'Storage_Size use 0; -- modified

   --  AI12-0054-2, Text_IO (see A.10.1) could have used predicates to describe
   --    some common exceptional conditions as follows:
--  subtype Open_File_Type is File_Type
--    with
--       Dynamic_Predicate => Is_Open (Open_File_Type),
--       Predicate_Failure => raise Status_Error with "File not open";
--  subtype Input_File_Type is Open_File_Type
--    with
--       Dynamic_Predicate => Mode (Input_File_Type) = In_File,
--       Predicate_Failure =>
--          raise Mode_Error with
--             "Cannot read file: " & Name (Input_File_Type);
--  subtype Output_File_Type is Open_File_Type
--    with
--       Dynamic_Predicate => Mode (Output_File_Type) /= In_File,
--       Predicate_Failure =>
--         raise Mode_Error with
--            "Cannot write file: " & Name (Output_File_Type);

--  type File_Mode is (In_File, Out_File, Append_File);
   type File_Mode is new IO_Modes.File_Mode; -- for conversion

   type Count is range 0 .. Natural'Last;
   subtype Positive_Count is Count range 1 .. Count'Last;
   Unbounded : constant Count := 0;

   subtype Field is Integer range 0 .. 255; -- implementation-defined
   subtype Number_Base is Integer range 2 .. 16;

--  type Type_Set is (Lower_Case, Upper_Case);
   type Type_Set is new Formatting.Type_Set;

   --  extended
   type String_Access is access String;
   procedure Free is
      new Unchecked_Deallocation (String, String_Access);
   type Wide_String_Access is access Wide_String;
   procedure Free is
      new Unchecked_Deallocation (Wide_String, Wide_String_Access);
   type Wide_Wide_String_Access is access Wide_Wide_String;
   procedure Free is
      new Unchecked_Deallocation (Wide_Wide_String, Wide_Wide_String_Access);

   --  File Management

   --  modified
   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String); -- removed default
   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      External : IO_Modes.File_External_Spec := IO_Modes.By_Target;
      New_Line : IO_Modes.File_New_Line_Spec := IO_Modes.By_Target);
   --  extended
   function Create (
      Mode : File_Mode := Out_File;
      Name : String := "";
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      External : IO_Modes.File_External_Spec := IO_Modes.By_Target;
      New_Line : IO_Modes.File_New_Line_Spec := IO_Modes.By_Target)
      return File_Type;

   --  modified
   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String); -- removed default
   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      External : IO_Modes.File_External_Spec := IO_Modes.By_Target;
      New_Line : IO_Modes.File_New_Line_Spec := IO_Modes.By_Target);
   --  extended
   function Open (
      Mode : File_Mode;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      External : IO_Modes.File_External_Spec := IO_Modes.By_Target;
      New_Line : IO_Modes.File_New_Line_Spec := IO_Modes.By_Target)
      return File_Type;

   procedure Close (File : in out File_Type);
   procedure Delete (File : in out File_Type);
   procedure Reset (File : in out File_Type; Mode : File_Mode);
   procedure Reset (File : in out File_Type);

   function Mode (
      File : File_Type) -- Open_File_Type
      return File_Mode;
   function Name (
      File : File_Type) -- Open_File_Type
      return String;
   function Name (File : not null File_Access) return String; -- alt
   function Form (
      File : File_Type) -- Open_File_Type
      return String;

   pragma Inline (Mode);
   pragma Inline (Name);

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

--  type File_Access is access constant File_Type; -- declared in above

   function Standard_Input return File_Access;
   function Standard_Output return File_Access;
   function Standard_Error return File_Access;

   pragma Pure_Function (Standard_Input);
   pragma Pure_Function (Standard_Output);
   pragma Pure_Function (Standard_Error);
   pragma Inline (Standard_Input);
   pragma Inline (Standard_Output);
   pragma Inline (Standard_Error);

   function Current_Input return File_Access;
   function Current_Output return File_Access;
   function Current_Error return File_Access;

   pragma Inline (Current_Input);
   pragma Inline (Current_Output);
   pragma Inline (Current_Error);

   --  Buffer control
   procedure Flush (
      File : File_Type); -- Output_File_Type
   procedure Flush;

   --  Specification of line and page lengths

   procedure Set_Line_Length (
      File : File_Type; -- Output_File_Type
      To : Count);
   procedure Set_Line_Length (To : Count);
   procedure Set_Line_Length (File : not null File_Access; To : Count); -- alt

   procedure Set_Page_Length (
      File : File_Type; -- Output_File_Type
      To : Count);
   procedure Set_Page_Length (To : Count);
   procedure Set_Page_Length (File : not null File_Access; To : Count); -- alt

   function Line_Length (
      File : File_Type) -- Output_File_Type
      return Count;
   function Line_Length return Count;
   pragma Inline (Line_Length);

   function Page_Length (
      File : File_Type) -- Output_File_Type
      return Count;
   function Page_Length return Count;
   pragma Inline (Page_Length);

   --  Column, Line, and Page Control

   procedure New_Line (
      File : File_Type; -- Output_File_Type
      Spacing : Positive_Count := 1);
   procedure New_Line (Spacing : Positive_Count := 1);
   procedure New_Line (
      File : not null File_Access;
      Spacing : Positive_Count := 1); -- alt

   procedure Skip_Line (
      File : File_Type; -- Input_File_Type
      Spacing : Positive_Count := 1);
   procedure Skip_Line (Spacing : Positive_Count := 1);
   procedure Skip_Line (
      File : not null File_Access;
      Spacing : Positive_Count := 1); -- alt

   function End_Of_Line (
      File : File_Type) -- Input_File_Type
      return Boolean;
   function End_Of_Line return Boolean;
   pragma Inline (End_Of_Line);

   procedure New_Page (
      File : File_Type); -- Output_File_Type
   procedure New_Page;
   procedure New_Page (File : not null File_Access); -- alt

   procedure Skip_Page (
      File : File_Type); -- Input_File_Type
   procedure Skip_Page;
   procedure Skip_Page (File : not null File_Access); -- alt

   function End_Of_Page (
      File : File_Type) -- Input_File_Type
      return Boolean;
   function End_Of_Page return Boolean;
   function End_Of_Page (File : not null File_Access) return Boolean; -- alt
   pragma Inline (End_Of_Page);

   function End_Of_File (
      File : File_Type) -- Input_File_Type
      return Boolean;
   function End_Of_File return Boolean;
   function End_Of_File (File : not null File_Access) return Boolean; -- alt
   pragma Inline (End_Of_File);

   procedure Set_Col (
      File : File_Type; -- Open_File_Type
      To : Positive_Count);
   procedure Set_Col (To : Positive_Count);
   procedure Set_Col (File : not null File_Access; To : Positive_Count); -- alt

   procedure Set_Line (
      File : File_Type; -- Open_File_Type
      To : Positive_Count);
   procedure Set_Line (To : Positive_Count);
   procedure Set_Line (
      File : not null File_Access;
      To : Positive_Count); -- alt

   function Col (
      File : File_Type) -- Open_File_Type
      return Positive_Count;
   function Col return Positive_Count;
   function Col (File : not null File_Access) return Positive_Count; -- alt
   pragma Inline (Col);

   function Line (
      File : File_Type) -- Open_File_Type
      return Positive_Count;
   function Line return Positive_Count;
   function Line (File : not null File_Access) return Positive_Count; -- alt
   pragma Inline (Line);

   function Page (
      File : File_Type) -- Open_File_Type
      return Positive_Count;
   function Page return Positive_Count;
   function Page (File : not null File_Access) return Positive_Count; -- alt
   pragma Inline (Page);

   --  Character Input-Output

   --  extended
   procedure Overloaded_Get (
      File : File_Type; -- Input_File_Type
      Item : out Character);
   procedure Overloaded_Get (
      File : File_Type; -- Input_File_Type
      Item : out Wide_Character);
   procedure Overloaded_Get (
      File : File_Type; -- Input_File_Type
      Item : out Wide_Wide_Character);
   procedure Overloaded_Get (Item : out Character);
   procedure Overloaded_Get (Item : out Wide_Character);
   procedure Overloaded_Get (Item : out Wide_Wide_Character);

   procedure Get (
      File : File_Type; -- Input_File_Type
      Item : out Character)
      renames Overloaded_Get;
   procedure Get (Item : out Character)
      renames Overloaded_Get;
   procedure Get (File : not null File_Access; Item : out Character); -- alt

   --  extended
   procedure Overloaded_Put (
      File : File_Type; -- Output_File_Type
      Item : Character);
   procedure Overloaded_Put (
      File : File_Type; -- Output_File_Type
      Item : Wide_Character);
   procedure Overloaded_Put (
      File : File_Type; -- Output_File_Type
      Item : Wide_Wide_Character);
   procedure Overloaded_Put (Item : Character);
   procedure Overloaded_Put (Item : Wide_Character);
   procedure Overloaded_Put (Item : Wide_Wide_Character);

   procedure Put (
      File : File_Type; -- Output_File_Type
      Item : Character)
      renames Overloaded_Put;
   procedure Put (Item : Character)
      renames Overloaded_Put;
   procedure Put (File : not null File_Access; Item : Character); -- alt

   --  extended
   procedure Overloaded_Look_Ahead (
      File : File_Type; -- Input_File_Type
      Item : out Character;
      End_Of_Line : out Boolean);
   procedure Overloaded_Look_Ahead (
      File : File_Type; -- Input_File_Type
      Item : out Wide_Character;
      End_Of_Line : out Boolean);
   procedure Overloaded_Look_Ahead (
      File : File_Type; -- Input_File_Type
      Item : out Wide_Wide_Character;
      End_Of_Line : out Boolean);
   procedure Overloaded_Look_Ahead (
      Item : out Character;
      End_Of_Line : out Boolean);
   procedure Overloaded_Look_Ahead (
      Item : out Wide_Character;
      End_Of_Line : out Boolean);
   procedure Overloaded_Look_Ahead (
      Item : out Wide_Wide_Character;
      End_Of_Line : out Boolean);

   procedure Look_Ahead (
      File : File_Type; -- Input_File_Type
      Item : out Character;
      End_Of_Line : out Boolean)
      renames Overloaded_Look_Ahead;
   procedure Look_Ahead (
      Item : out Character;
      End_Of_Line : out Boolean)
      renames Overloaded_Look_Ahead;

   --  extended
   --  Skip one character or mark of new-line
   --    looked by last calling of Look_Ahead.
   procedure Skip_Ahead (
      File : File_Type); -- Input_File_Type

   --  extended
   procedure Overloaded_Get_Immediate (
      File : File_Type; -- Input_File_Type
      Item : out Character);
   procedure Overloaded_Get_Immediate (
      File : File_Type; -- Input_File_Type
      Item : out Wide_Character);
   procedure Overloaded_Get_Immediate (
      File : File_Type; -- Input_File_Type
      Item : out Wide_Wide_Character);
   procedure Overloaded_Get_Immediate (Item : out Character);
   procedure Overloaded_Get_Immediate (Item : out Wide_Character);
   procedure Overloaded_Get_Immediate (Item : out Wide_Wide_Character);

   procedure Get_Immediate (
      File : File_Type; -- Input_File_Type
      Item : out Character)
      renames Overloaded_Get_Immediate;
   procedure Get_Immediate (Item : out Character)
      renames Overloaded_Get_Immediate;

   --  extended
   procedure Overloaded_Get_Immediate (
      File : File_Type; -- Input_File_Type
      Item : out Character;
      Available : out Boolean);
   procedure Overloaded_Get_Immediate (
      File : File_Type; -- Input_File_Type
      Item : out Wide_Character;
      Available : out Boolean);
   procedure Overloaded_Get_Immediate (
      File : File_Type; -- Input_File_Type
      Item : out Wide_Wide_Character;
      Available : out Boolean);
   procedure Overloaded_Get_Immediate (
      Item : out Character;
      Available : out Boolean);
   procedure Overloaded_Get_Immediate (
      Item : out Wide_Character;
      Available : out Boolean);
   procedure Overloaded_Get_Immediate (
      Item : out Wide_Wide_Character;
      Available : out Boolean);

   procedure Get_Immediate (
      File : File_Type; -- Input_File_Type
      Item : out Character;
      Available : out Boolean)
      renames Overloaded_Get_Immediate;
   procedure Get_Immediate (
      Item : out Character;
      Available : out Boolean)
      renames Overloaded_Get_Immediate;

   --  String Input-Output

   --  extended
   procedure Overloaded_Get (
      File : File_Type; -- Input_File_Type
      Item : out String);
   procedure Overloaded_Get (
      File : File_Type; -- Input_File_Type
      Item : out Wide_String);
   procedure Overloaded_Get (
      File : File_Type; -- Input_File_Type
      Item : out Wide_Wide_String);
   procedure Overloaded_Get (Item : out String);
   procedure Overloaded_Get (Item : out Wide_String);
   procedure Overloaded_Get (Item : out Wide_Wide_String);

   procedure Get (
      File : File_Type; -- Input_File_Type
      Item : out String)
      renames Overloaded_Get;
   procedure Get (Item : out String)
      renames Overloaded_Get;
   procedure Get (File : not null File_Access; Item : out String); -- alt

   --  extended
   procedure Overloaded_Put (
      File : File_Type; -- Output_File_Type
      Item : String);
   procedure Overloaded_Put (
      File : File_Type; -- Output_File_Type
      Item : Wide_String);
   procedure Overloaded_Put (
      File : File_Type; -- Output_File_Type
      Item : Wide_Wide_String);
   procedure Overloaded_Put (Item : String);
   procedure Overloaded_Put (Item : Wide_String);
   procedure Overloaded_Put (Item : Wide_Wide_String);

   procedure Put (
      File : File_Type; -- Output_File_Type
      Item : String)
      renames Overloaded_Put;
   procedure Put (Item : String)
      renames Overloaded_Put;
   procedure Put (File : not null File_Access; Item : String); -- alt

   --  extended
   procedure Overloaded_Get_Line (
      File : File_Type; -- Input_File_Type
      Item : out String;
      Last : out Natural);
   procedure Overloaded_Get_Line (
      File : File_Type; -- Input_File_Type
      Item : out Wide_String;
      Last : out Natural);
   procedure Overloaded_Get_Line (
      File : File_Type; -- Input_File_Type
      Item : out Wide_Wide_String;
      Last : out Natural);
   procedure Overloaded_Get_Line (
      Item : out String;
      Last : out Natural);
   procedure Overloaded_Get_Line (
      Item : out Wide_String;
      Last : out Natural);
   procedure Overloaded_Get_Line (
      Item : out Wide_Wide_String;
      Last : out Natural);

   procedure Get_Line (
      File : File_Type; -- Input_File_Type
      Item : out String;
      Last : out Natural)
      renames Overloaded_Get_Line;
   procedure Get_Line (
      Item : out String;
      Last : out Natural)
      renames Overloaded_Get_Line;
   procedure Get_Line (
      File : not null File_Access;
      Item : out String;
      Last : out Natural); -- alt

   --  extended
   procedure Overloaded_Get_Line (
      File : File_Type; -- Input_File_Type
      Item : out String_Access);
   procedure Overloaded_Get_Line (
      File : File_Type; -- Input_File_Type
      Item : out Wide_String_Access);
   procedure Overloaded_Get_Line (
      File : File_Type; -- Input_File_Type
      Item : out Wide_Wide_String_Access);

   --  extended
   function Overloaded_Get_Line (
      File : File_Type) -- Input_File_Type
      return String;
   function Overloaded_Get_Line (
      File : File_Type) -- Input_File_Type
      return Wide_String;
   function Overloaded_Get_Line (
      File : File_Type) -- Input_File_Type
      return Wide_Wide_String;
   function Overloaded_Get_Line return String;
   function Overloaded_Get_Line return Wide_String;
   function Overloaded_Get_Line return Wide_Wide_String;

   function Get_Line (
      File : File_Type) -- Input_File_Type
      return String
      renames Overloaded_Get_Line;
   function Get_Line return String
      renames Overloaded_Get_Line;

   --  extended
   procedure Overloaded_Put_Line (
      File : File_Type; -- Output_File_Type
      Item : String);
   procedure Overloaded_Put_Line (
      File : File_Type; -- Output_File_Type
      Item : Wide_String);
   procedure Overloaded_Put_Line (
      File : File_Type; -- Output_File_Type
      Item : Wide_Wide_String);
   procedure Overloaded_Put_Line (Item : String);
   procedure Overloaded_Put_Line (Item : Wide_String);
   procedure Overloaded_Put_Line (Item : Wide_Wide_String);

   procedure Put_Line (
      File : File_Type; -- Output_File_Type
      Item : String)
      renames Overloaded_Put_Line;
   procedure Put_Line (Item : String)
      renames Overloaded_Put_Line;
   procedure Put_Line (File : not null File_Access; Item : String); -- alt

   --  Generic packages for Input-Output of Integer Types
   --  Generic packages for Input-Output of Real Types
   --  Generic package for Input-Output of Enumeration Types

   --  Note: Integer_IO, Modular_IO, Float_IO, Fixed_IO, Decimal_IO, and
   --    Enumeration_IO are separated by compiler.

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
      function Standard_Output return File_Access;
      function Standard_Error return File_Access;

      pragma Inline (Standard_Input);
      pragma Inline (Standard_Output);
      pragma Inline (Standard_Error);

      function Reference_Current_Input return not null access File_Access;
      function Reference_Current_Output return not null access File_Access;
      function Reference_Current_Error return not null access File_Access;

      pragma Inline (Reference_Current_Input);
      pragma Inline (Reference_Current_Output);
      pragma Inline (Reference_Current_Error);

      function Reference (File : Text_IO.File_Type)
         return not null access Naked_Text_IO.Non_Controlled_File_Type;
      pragma Inline (Reference);

   private

      type File_Type is limited new Finalization.Limited_Controlled with record
         Text : aliased Naked_Text_IO.Non_Controlled_File_Type;
      end record;

      overriding procedure Finalize (Object : in out File_Type);

   end Controlled;

   type File_Type is new Controlled.File_Type;

end Ada.Text_IO;
