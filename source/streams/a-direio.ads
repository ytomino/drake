pragma License (Unrestricted);
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
generic
   type Element_Type is private;
package Ada.Direct_IO is

   type File_Type is limited private;

   type File_Mode is (In_File, Inout_File, Out_File);
   type Count is new Streams.Stream_IO.Count; --  implementation-defined
   subtype Positive_Count is Count range 1 .. Count'Last;

   --  File management

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Inout_File;
      Name : String := "";
      Form : String := "");
   pragma Inline (Create);

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "");
   pragma Inline (Open);

   procedure Close (File : in out File_Type);
   pragma Inline (Close);
   procedure Delete (File : in out File_Type);
   pragma Inline (Delete);
   procedure Reset (File : in out File_Type; Mode : File_Mode);
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

   --  Input and output operations

   procedure Read (
      File : File_Type;
      Item : out Element_Type;
      From : Positive_Count);
   procedure Read (
      File : File_Type;
      Item : out Element_Type);
   pragma Inline (Read);

   procedure Write (
      File : File_Type;
      Item : Element_Type;
      To : Positive_Count);
   procedure Write (
      File : File_Type;
      Item : Element_Type);
   pragma Inline (Write);

   procedure Set_Index (File : File_Type; To : Positive_Count);
   pragma Inline (Set_Index);

   function Index (File : File_Type) return Positive_Count;
   pragma Inline (Index);
   function Size (File : File_Type) return Count;
   pragma Inline (Size);

   function End_Of_File (File : File_Type) return Boolean;
   pragma Inline (End_Of_File);

   --  Exceptions

   Status_Error : exception renames IO_Exceptions.Status_Error;
   Mode_Error : exception renames IO_Exceptions.Mode_Error;
   Name_Error : exception renames IO_Exceptions.Name_Error;
   Use_Error : exception renames IO_Exceptions.Use_Error;
   Device_Error : exception renames IO_Exceptions.Device_Error;
   End_Error : exception renames IO_Exceptions.End_Error;
   Data_Error : exception renames IO_Exceptions.Data_Error;

private

   type File_Type is new Streams.Stream_IO.File_Type;

end Ada.Direct_IO;
