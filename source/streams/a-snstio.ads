pragma License (Unrestricted);
--  implementation unit
with Ada.IO_Exceptions;
with Ada.IO_Modes;
with System.Native_IO;
private with Ada.Tags;
package Ada.Streams.Naked_Stream_IO is
   pragma Preelaborate;

   --  the parameter Form

   Default_Form : constant System.Native_IO.Packed_Form := (
      Shared => IO_Modes.By_Mode,
      Wait => False,
      Overwrite => True);

   subtype Form_String is String (1 .. 256);

   procedure Set (
      Form : in out System.Native_IO.Packed_Form;
      Keyword : String;
      Item : String);
   function Pack (Form : String) return System.Native_IO.Packed_Form;
   procedure Unpack (
      Form : System.Native_IO.Packed_Form;
      Result : out Form_String;
      Last : out Natural);

   --  non-controlled

   type Stream_Type (<>) is limited private;
   type Non_Controlled_File_Type is access all Stream_Type;
   --  Non_Controlled_File_Type is a pass-by-value type whether in out or not,
   --  and it's possible that Reset/Set_Mode may close the file.
   --  So these functions have access mode.

   procedure Create (
      File : in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode := IO_Modes.Out_File;
      Name : String := "";
      Form : System.Native_IO.Packed_Form := Default_Form);

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode;
      Name : String;
      Form : System.Native_IO.Packed_Form := Default_Form);

   procedure Close (
      File : aliased in out Non_Controlled_File_Type;
      Raise_On_Error : Boolean := True);
   procedure Delete (File : aliased in out Non_Controlled_File_Type);
   procedure Reset (
      File : aliased in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode);

   function Mode (File : Non_Controlled_File_Type) return IO_Modes.File_Mode;
   pragma Inline (Mode);
   function Name (File : Non_Controlled_File_Type) return String;
   pragma Inline (Name);
   function Form (File : Non_Controlled_File_Type)
      return System.Native_IO.Packed_Form;
   pragma Inline (Form);

   function Is_Open (File : Non_Controlled_File_Type) return Boolean;
   pragma Inline (Is_Open);
   function End_Of_File (File : Non_Controlled_File_Type) return Boolean;

   function Stream (File : Non_Controlled_File_Type)
      return not null access Root_Stream_Type'Class;
   pragma Inline (Stream);

   procedure Read (
      File : not null Non_Controlled_File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset);

   procedure Write (
      File : not null Non_Controlled_File_Type;
      Item : Stream_Element_Array);

   procedure Set_Index (
      File : not null Non_Controlled_File_Type;
      To : Stream_Element_Positive_Count);

   function Index (File : not null Non_Controlled_File_Type)
      return Stream_Element_Positive_Count;
   function Size (File : not null Non_Controlled_File_Type)
      return Stream_Element_Count;

   procedure Set_Mode (
      File : aliased in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode);

   procedure Flush (File : Non_Controlled_File_Type);

   --  handle for non-controlled

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode;
      Handle : System.Native_IO.Handle_Type;
      Name : String := "";
      Form : System.Native_IO.Packed_Form := Default_Form;
      To_Close : Boolean := False);

   function Handle (File : Non_Controlled_File_Type)
      return System.Native_IO.Handle_Type;
   pragma Inline (Handle);

   function Is_Standard (File : Non_Controlled_File_Type) return Boolean;
   pragma Inline (Is_Standard);

   --  exceptions

   Status_Error : exception
      renames IO_Exceptions.Status_Error;
   Mode_Error : exception
      renames IO_Exceptions.Mode_Error;
   Use_Error : exception
      renames IO_Exceptions.Use_Error;
   Device_Error : exception
      renames IO_Exceptions.Device_Error;
   End_Error : exception
      renames IO_Exceptions.End_Error;

private

   package Dispatchers is

      type Root_Dispatcher is new Root_Stream_Type with record
         File : Non_Controlled_File_Type;
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
         File : Non_Controlled_File_Type;
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
         File : Non_Controlled_File_Type := null;
      end record;
      pragma Suppress_Initialization (Dispatcher);
      for Dispatcher'Alignment use
         Standard'Address_Size / Standard'Storage_Unit;

      pragma Compile_Time_Error (
         Seekable_Dispatcher'Size /= Root_Dispatcher'Size
         or else Dispatcher'Size /= Root_Dispatcher'Size,
         "size mismatch");

      pragma Compile_Time_Error (
         Seekable_Dispatcher'Alignment /= Root_Dispatcher'Alignment
         or else Dispatcher'Alignment /= Root_Dispatcher'Alignment,
         "misaligned");

   end Dispatchers;

   type Stream_Kind is (
      Ordinary,
      Temporary,
      External,
      External_No_Close,
      Standard_Handle);
   pragma Discard_Names (Stream_Kind);

   Uninitialized_Buffer : constant := -1;

   type Stream_Type is record -- "limited" prevents No_Elaboration_Code
      Handle : aliased System.Native_IO.Handle_Type; -- file descripter
      Mode : IO_Modes.File_Mode;
      Kind : Stream_Kind;
      Buffer_Inline : aliased Stream_Element;
      Name : System.Native_IO.Name_Pointer;
      Name_Length : System.Native_IO.Name_Length;
      Form : System.Native_IO.Packed_Form;
      Buffer : System.Address;
      Buffer_Length : Stream_Element_Offset;
      Buffer_Index : Stream_Element_Offset; -- Index (File) mod Buffer_Length
      Reading_Index : Stream_Element_Offset;
      Writing_Index : Stream_Element_Offset;
      Dispatcher : aliased Dispatchers.Dispatcher := (
         Tag => Tags.No_Tag,
         File => null);
   end record;
   pragma Suppress_Initialization (Stream_Type);

end Ada.Streams.Naked_Stream_IO;
