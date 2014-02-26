pragma License (Unrestricted);
with Ada.IO_Exceptions;
with Ada.IO_Modes;
private with Ada.Finalization;
private with Ada.Tags;
limited private with Ada.Streams.Stream_IO.Inside;
package Ada.Streams.Stream_IO is
   pragma Preelaborate; -- AI12-0010-1

   type Stream_Access is access all Root_Stream_Type'Class;
   for Stream_Access'Storage_Size use 0;

   type File_Type is limited private;

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
   --  extended
   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True);
   function Create (
      Mode : File_Mode := Out_File;
      Name : String := "";
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True)
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
      Overwrite : Boolean := True);
   function Open (
      Mode : File_Mode;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True)
      return File_Type;
   pragma Inline (Open);

   procedure Close (File : in out File_Type);
   procedure Delete (File : in out File_Type);
   procedure Reset (File : in out File_Type; Mode : File_Mode);
   procedure Reset (File : in out File_Type);

   function Mode (File : File_Type) return File_Mode;
   pragma Inline (Mode);
   function Name (File : File_Type) return String;
   pragma Inline (Name);
   function Form (File : File_Type) return String;

   function Is_Open (File : File_Type) return Boolean;
   pragma Inline (Is_Open);
   function End_Of_File (File : File_Type) return Boolean;
   pragma Inline (End_Of_File);

   function Stream (File : File_Type) return Stream_Access;
   pragma Inline (Stream);
   --  Return stream access for use with T'Input and T'Output

   --  Read array of stream elements from file
   procedure Read (
      File : File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset;
      From : Positive_Count);
   procedure Read (
      File : File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset);

   --  Write array of stream elements into file
   procedure Write (
      File : File_Type;
      Item : Stream_Element_Array;
      To : Positive_Count);
   procedure Write (
      File : File_Type;
      Item : Stream_Element_Array);

   --  Operations on position within file

   procedure Set_Index (File : File_Type; To : Positive_Count);

   function Index (File : File_Type) return Positive_Count;
   pragma Inline (Index);
   function Size (File : File_Type) return Count;
   pragma Inline (Size);

   procedure Set_Mode (File : in out File_Type; Mode : File_Mode);

   procedure Flush (File : File_Type);

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

      function Reference (File : File_Type)
         return access Inside.Non_Controlled_File_Type;
      pragma Inline (Reference);

   private

      type File_Type is
         limited new Finalization.Limited_Controlled with
      record
         Stream : access Inside.Stream_Type;
      end record;

      overriding procedure Finalize (Object : in out File_Type);

   end Controlled;

   type File_Type is new Controlled.File_Type;

   --  for non-controlled

   package Dispatchers is

      type Root_Dispatcher is new Root_Stream_Type with record
         File : access Inside.Stream_Type;
      end record;
      pragma Suppress_Initialization (Root_Dispatcher);

      overriding procedure Read (
         Stream : in out Root_Dispatcher;
         Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset);

      overriding procedure Write (
         Stream : in out Root_Dispatcher;
         Item : Stream_Element_Array);

      type Seekable_Dispatcher is new Seekable_Stream_Type with record
         File : access Inside.Stream_Type;
      end record;
      pragma Suppress_Initialization (Seekable_Dispatcher);

      overriding procedure Read (
         Stream : in out Seekable_Dispatcher;
         Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset);

      overriding procedure Write (
         Stream : in out Seekable_Dispatcher;
         Item : Stream_Element_Array);

      overriding procedure Set_Index (
         Stream : in out Seekable_Dispatcher;
         To : Stream_Element_Positive_Count);

      overriding function Index (Stream : Seekable_Dispatcher)
         return Stream_Element_Positive_Count;
      overriding function Size (Stream : Seekable_Dispatcher)
         return Stream_Element_Count;

      type Dispatcher is record
         Tag : Tags.Tag := Tags.No_Tag;
         File : access Inside.Stream_Type := null;
      end record;
      pragma Suppress_Initialization (Dispatcher);

      pragma Compile_Time_Error (
         Seekable_Dispatcher'Size /= Root_Dispatcher'Size
         or else Dispatcher'Size /= Root_Dispatcher'Size,
         "size mismatch");

   end Dispatchers;

end Ada.Streams.Stream_IO;
