pragma License (Unrestricted);
--  implementation unit specialized for Windows
with System.Native_IO;
private with Ada.Tags;
package Ada.Streams.Stream_IO.Inside is
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

   --  handle for controlled

   procedure Open (
      File : in out File_Type;
      Handle : System.Native_IO.Handle_Type;
      Mode : File_Mode;
      Name : String := "";
      Form : System.Native_IO.Packed_Form := Default_Form;
      To_Close : Boolean := False);

   function Handle (File : File_Type) return System.Native_IO.Handle_Type;
   pragma Inline (Handle);

   type Non_Controlled_File_Type; -- forward

   function Non_Controlled (File : File_Type)
      return not null access Non_Controlled_File_Type;
   pragma Inline (Non_Controlled);

   --  non-controlled

   type Stream_Type (<>) is limited private;
   type Non_Controlled_File_Type is access all Stream_Type;
   --  Non_Controlled_File_Type is a pass-by-value type whether in out or not,
   --  and it's possible that Reset/Set_Mode may close the file.
   --  So these functions have access mode.

   procedure Create (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : System.Native_IO.Packed_Form := Default_Form);

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : System.Native_IO.Packed_Form := Default_Form);

   procedure Close (
      File : aliased in out Non_Controlled_File_Type;
      Raise_On_Error : Boolean := True);
   procedure Delete (File : aliased in out Non_Controlled_File_Type);
   procedure Reset (
      File : aliased in out Non_Controlled_File_Type;
      Mode : File_Mode);

   function Mode (File : Non_Controlled_File_Type) return File_Mode;
   pragma Inline (Mode);
   function Name (File : Non_Controlled_File_Type) return String;
   pragma Inline (Name);
   function Form (File : Non_Controlled_File_Type)
      return System.Native_IO.Packed_Form;
   pragma Inline (Form);

   function Is_Open (File : Non_Controlled_File_Type) return Boolean;
   pragma Inline (Is_Open);
   function End_Of_File (File : Non_Controlled_File_Type) return Boolean;

   function Stream (File : Non_Controlled_File_Type) return Stream_Access;
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
      To : Positive_Count);

   function Index (File : not null Non_Controlled_File_Type)
      return Positive_Count;
   function Size (File : not null Non_Controlled_File_Type)
      return Count;

   procedure Set_Mode (
      File : aliased in out Non_Controlled_File_Type;
      Mode : File_Mode);

   procedure Flush (File : Non_Controlled_File_Type);

   --  handle for non-controlled

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Handle : System.Native_IO.Handle_Type;
      Mode : File_Mode;
      Name : String := "";
      Form : System.Native_IO.Packed_Form := Default_Form;
      To_Close : Boolean := False);

   function Handle (File : Non_Controlled_File_Type)
      return System.Native_IO.Handle_Type;
   pragma Inline (Handle);

   function Is_Standard (File : Non_Controlled_File_Type) return Boolean;
   pragma Inline (Is_Standard);

private

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
      Mode : File_Mode;
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

end Ada.Streams.Stream_IO.Inside;
