pragma License (Unrestricted);
with Ada.IO_Exceptions;
with Ada.IO_Modes;
private with Ada.Finalization;
private with Ada.Streams.Naked_Stream_IO;
package Ada.Streams.Stream_IO is
   pragma Preelaborate;

   type Stream_Access is access all Root_Stream_Type'Class;
   for Stream_Access'Storage_Size use 0;

   type File_Type is limited private;
   pragma Preelaborable_Initialization (File_Type); -- AI12-0102-1

   --  Similar to Text_IO in AI12-0054-2:
--  subtype Open_File_Type is File_Type
--     with
--       Dynamic_Predicate => Is_Open (Open_File_Type),
--       Predicate_Failure => raise Status_Error with "File not open";
--  subtype Input_File_Type is Open_File_Type
--    with
--       Dynamic_Predicate => Mode (Input_File_Type) /= Out_File,
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

   --  modified
   --  Count is essentially same as Stream_Element_Count.
--  type Count is range 0 .. implementation-defined;
   subtype Count is Stream_Element_Count;
   subtype Positive_Count is Count range 1 .. Count'Last;
      --  Index into file, in stream elements

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
      Overwrite : Boolean := True);
   --  extended
   function Create (
      Mode : File_Mode := Out_File;
      Name : String := "";
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True)
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
      Overwrite : Boolean := True);
   --  extended
   function Open (
      Mode : File_Mode;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True)
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
   function Form (
      File : File_Type) -- Open_File_Type
      return String;

   pragma Inline (Mode);
   pragma Inline (Name);

   function Is_Open (File : File_Type) return Boolean;
   function End_Of_File (
      File : File_Type) -- Open_File_Type
      return Boolean;

   pragma Inline (Is_Open);
   pragma Inline (End_Of_File);

   function Stream (
      File : File_Type) -- Open_File_Type
      return Stream_Access;
      --  Return stream access for use with T'Input and T'Output
   pragma Inline (Stream);

   --  Read array of stream elements from file
   procedure Read (
      File : File_Type; -- Input_File_Type
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset;
      From : Positive_Count);
   procedure Read (
      File : File_Type; -- Input_File_Type
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset);

   --  Write array of stream elements into file
   procedure Write (
      File : File_Type; -- Output_File_Type
      Item : Stream_Element_Array;
      To : Positive_Count);
   procedure Write (
      File : File_Type; -- Output_File_Type
      Item : Stream_Element_Array);

   --  Operations on position within file

   procedure Set_Index (
      File : File_Type; -- Open_File_Type
      To : Positive_Count);

   function Index (
      File : File_Type) -- Open_File_Type
      return Positive_Count;
   function Size (
      File : File_Type) -- Open_File_Type
      return Count;

   pragma Inline (Index);
   pragma Inline (Size);

   procedure Set_Mode (File : in out File_Type; Mode : File_Mode);

   procedure Flush (
      File : File_Type); -- Output_File_Type

   --  exceptions
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

private

   package Controlled is

      type File_Type is limited private;

      function Reference (File : Stream_IO.File_Type)
         return not null access Naked_Stream_IO.Non_Controlled_File_Type;
      pragma Inline (Reference);

   private

      type File_Type is
         limited new Finalization.Limited_Controlled with
      record
         Stream : aliased Naked_Stream_IO.Non_Controlled_File_Type;
      end record;

      overriding procedure Finalize (Object : in out File_Type);

   end Controlled;

   type File_Type is new Controlled.File_Type;

end Ada.Streams.Stream_IO;
