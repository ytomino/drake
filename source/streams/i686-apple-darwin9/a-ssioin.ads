pragma License (Unrestricted);
--  implementation unit
with C;
package Ada.Streams.Stream_IO.Inside is

   --  handle of operating system

   subtype Handle_Type is C.signed_int;

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
   procedure Reset (File : in out Non_Controlled_File_Type; Mode : File_Mode);

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
      File : Non_Controlled_File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset);

   procedure Write (
      File : Non_Controlled_File_Type;
      Item : Stream_Element_Array);

   procedure Set_Index (File : Non_Controlled_File_Type; To : Positive_Count);

   function Index (File : Non_Controlled_File_Type) return Positive_Count;
   function Size (File : Non_Controlled_File_Type) return Count;

   procedure Set_Mode (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode);

   procedure Flush (File : Non_Controlled_File_Type);

   --  handle of operating system for non-controlled

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Handle : Handle_Type;
      Mode : File_Mode;
      Name : String := "";
      Form : String := "";
      To_Close : Boolean := False);

   function Handle (File : Non_Controlled_File_Type) return Handle_Type;
   pragma Inline (Handle);

   function Is_Terminal (File : Non_Controlled_File_Type) return Boolean;

   --  parsing form parameter

   procedure Form_Parameter (
      Form : String;
      Keyword : String;
      First : out Positive;
      Last : out Natural);

   --  The form "shared=yes" or "shared=no" sets shared mode.
   --  The form "wcem=?" sets wide characters encoding method by Text_IO.

private

   package Dispatchers is

      type Root_Dispatcher is new Root_Stream_Type with record
         Stream : Non_Controlled_File_Type;
      end record;

      overriding procedure Read (
         Stream : in out Root_Dispatcher;
         Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset);

      overriding procedure Write (
         Stream : in out Root_Dispatcher;
         Item : Stream_Element_Array);

      type Seekable_Dispatcher is new Seekable_Stream_Type with record
         Stream : Non_Controlled_File_Type;
      end record;

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

   end Dispatchers;

   type Stream_Kind is (
      Normal,
      Temporary,
      External,
      External_No_Close,
      Standard_Handle);
   pragma Discard_Names (Stream_Kind);

   type Stream_Type (
      Name_Length : Natural;
      Form_Length : Natural) is limited
   record
      Root_Dispatcher : aliased Dispatchers.Root_Dispatcher :=
         (Root_Stream_Type with Stream => Stream_Type'Unchecked_Access);
      Seekable_Dispatcher : aliased Dispatchers.Seekable_Dispatcher :=
         (Seekable_Stream_Type with Stream => Stream_Type'Unchecked_Access);
      Handle : C.signed_int; -- file descripter
      Mode : File_Mode;
      Kind : Stream_Kind;
      Buffer : aliased Stream_Element_Array (1 .. 1);
      Last : Stream_Element_Count;
      Name : String (1 .. Name_Length);
      Form : String (1 .. Form_Length);
   end record;
   pragma Suppress_Initialization (Stream_Type);

end Ada.Streams.Stream_IO.Inside;
