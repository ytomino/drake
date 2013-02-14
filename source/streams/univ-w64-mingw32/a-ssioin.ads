pragma License (Unrestricted);
--  implementation unit
with Ada.Tags;
with C.winnt;
private with System;
package Ada.Streams.Stream_IO.Inside is
   pragma Preelaborate;

   --  handle

   subtype Handle_Type is C.winnt.HANDLE;

   function Is_Terminal (Handle : Handle_Type) return Boolean;
   function Is_Seekable (Handle : Handle_Type) return Boolean;

   --  handle for controlled

   procedure Open (
      File : in out File_Type;
      Handle : Handle_Type;
      Mode : File_Mode;
      Name : String := "";
      Form : String := "";
      To_Close : Boolean := False);

   function Handle (File : File_Type) return Handle_Type;
   pragma Inline (Handle);

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
      Form : String := "");

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "");

   procedure Close (
      File : in out Non_Controlled_File_Type;
      Raise_On_Error : Boolean := True);
   procedure Delete (File : in out Non_Controlled_File_Type);
   procedure Reset (
      File : not null access Non_Controlled_File_Type;
      Mode : File_Mode);

   function Mode (File : Non_Controlled_File_Type) return File_Mode;
   pragma Inline (Mode);
   function Name (File : Non_Controlled_File_Type) return String;
   pragma Inline (Name);
   function Form (File : Non_Controlled_File_Type) return String;
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
      File : not null access Non_Controlled_File_Type;
      Mode : File_Mode);

   procedure Flush (File : Non_Controlled_File_Type);

   --  handle for non-controlled

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Handle : Handle_Type;
      Mode : File_Mode;
      Name : String := "";
      Form : String := "";
      To_Close : Boolean := False);

   function Handle (File : Non_Controlled_File_Type) return Handle_Type;
   pragma Inline (Handle);

   function Is_Standard (File : Non_Controlled_File_Type) return Boolean;
   pragma Inline (Is_Standard);

private

   type Stream_Kind is (
      Normal,
      Temporary,
      External,
      External_No_Close,
      Standard_Handle);
   pragma Discard_Names (Stream_Kind);

   Uninitialized_Buffer : constant := -1;

   type Stream_Type is record -- "limited" prevents No_Elaboration_Code
      Handle : C.winnt.HANDLE;
      Mode : File_Mode;
      Kind : Stream_Kind;
      Buffer_Inline : aliased Stream_Element;
      Name : C.winnt.LPWSTR;
      Form : System.Address; -- Ada String
      Name_Length : C.signed_int;
      Form_Length : Natural;
      Buffer : System.Address;
      Buffer_Length : Stream_Element_Offset;
      Buffer_Index : Stream_Element_Offset; -- Index (File) mod Buffer_Length
      Reading_Index : Stream_Element_Offset;
      Writing_Index : Stream_Element_Offset;
      Dispatcher : aliased Dispatchers.Dispatcher := (
         Tag => Ada.Tags.No_Tag,
         File => null);
   end record;
   pragma Suppress_Initialization (Stream_Type);

end Ada.Streams.Stream_IO.Inside;
