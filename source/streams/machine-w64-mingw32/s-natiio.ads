pragma License (Unrestricted);
--  implementation unit specialized for Windows
with Ada.Exception_Identification;
with Ada.IO_Exceptions;
with Ada.IO_Modes;
with Ada.Streams;
with System.Storage_Elements;
with System.Zero_Terminated_WStrings;
with C.winbase;
with C.windef;
with C.winnt;
package System.Native_IO is
   pragma Preelaborate;
   use type C.windef.DWORD;

   subtype Name_Character is C.winnt.WCHAR;
   subtype Name_String is C.winnt.WCHAR_array;
   subtype Name_Pointer is C.winnt.LPWSTR;

   subtype Handle_Type is C.winnt.HANDLE;

   Invalid_Handle : constant Handle_Type := C.winbase.INVALID_HANDLE_VALUE;

   --  modes

   subtype File_Mode is C.windef.DWORD;

   Read_Only_Mode : constant File_Mode := C.winnt.GENERIC_READ; -- 0x80000000
   Write_Only_Mode : constant File_Mode := C.winnt.GENERIC_WRITE; -- 0x40000000
   Read_Write_Mode : constant File_Mode := Read_Only_Mode or Write_Only_Mode;
   Read_Write_Mask : constant File_Mode := Read_Write_Mode;
   Append_Mode : constant File_Mode := 1;

   pragma Compile_Time_Error (
      (Append_Mode and Read_Write_Mask) /= 0,
      "Append_Mode is overlapped with Read_Write_Mask");

   --  name

   function Value (First : not null access constant Name_Character)
      return String
      renames Zero_Terminated_WStrings.Value;

   procedure Free (Item : in out Name_Pointer);

   procedure New_External_Name (
      Item : String;
      Out_Item : aliased out Name_Pointer); -- '*' & Name & NUL

   --  file management

   procedure Open_Temporary (
      Handle : aliased out Handle_Type;
      Out_Item : aliased out Name_Pointer);

   type Open_Method is (Open, Create, Reset);
   pragma Discard_Names (Open_Method);

   type Packed_Form is record
      Shared : Ada.IO_Modes.File_Shared_Spec;
      Wait : Boolean;
      Overwrite : Boolean;
   end record;
   pragma Suppress_Initialization (Packed_Form);
   pragma Pack (Packed_Form);

   procedure Open_Ordinary (
      Method : Open_Method;
      Handle : aliased out Handle_Type;
      Mode : File_Mode;
      Name : not null Name_Pointer;
      Form : Packed_Form);

   procedure Close_Ordinary (
      Handle : Handle_Type;
      Name : Name_Pointer;
      Raise_On_Error : Boolean);

   procedure Delete_Ordinary (
      Handle : Handle_Type;
      Name : Name_Pointer; -- not null
      Raise_On_Error : Boolean);

   procedure Close_Temporary (
      Handle : Handle_Type;
      Name : Name_Pointer; -- not null
      Raise_On_Error : Boolean)
      renames Close_Ordinary;

--  procedure Set_Close_On_Exec (Handle : Handle_Type);
   procedure Unset (Handle : Handle_Type; Mask : File_Mode) is null;

   pragma Inline (Unset); -- [gcc-7] can not skip calling null procedure

   function Is_Terminal (Handle : Handle_Type) return Boolean;
   function Is_Seekable (Handle : Handle_Type) return Boolean;

   function Block_Size (Handle : Handle_Type)
      return Ada.Streams.Stream_Element_Count;

   --  read from file

   procedure Read (
      Handle : Handle_Type;
      Item : Address;
      Length : Ada.Streams.Stream_Element_Offset;
      Out_Length : out Ada.Streams.Stream_Element_Offset); -- -1 when error

   --  write into file

   procedure Write (
      Handle : Handle_Type;
      Item : Address;
      Length : Ada.Streams.Stream_Element_Offset;
      Out_Length : out Ada.Streams.Stream_Element_Offset); -- -1 when error

   procedure Flush (Handle : Handle_Type);

   --  position within file

   subtype Whence_Type is C.windef.DWORD;

   From_Begin : constant := C.winbase.FILE_BEGIN;
   From_Current : constant := C.winbase.FILE_CURRENT;
   From_End : constant := C.winbase.FILE_END;

   procedure Set_Relative_Index (
      Handle : Handle_Type;
      Relative_To : Ada.Streams.Stream_Element_Offset; -- 0-origin
      Whence : Whence_Type;
      New_Index : out Ada.Streams.Stream_Element_Offset); -- 1-origin

   function Index (Handle : Handle_Type)
      return Ada.Streams.Stream_Element_Offset; -- 1-origin

   function Size (Handle : Handle_Type)
      return Ada.Streams.Stream_Element_Count;

   --  default input and output files

   function Standard_Input return Handle_Type;
   function Standard_Output return Handle_Type;
   function Standard_Error return Handle_Type;

   Uninitialized_Standard_Input : constant Handle_Type :=
      C.winbase.INVALID_HANDLE_VALUE;
   Uninitialized_Standard_Output : constant Handle_Type :=
      C.winbase.INVALID_HANDLE_VALUE;
   Uninitialized_Standard_Error : constant Handle_Type :=
      C.winbase.INVALID_HANDLE_VALUE;

   procedure Initialize (
      Standard_Input_Handle : aliased in out Handle_Type;
      Standard_Output_Handle : aliased in out Handle_Type;
      Standard_Error_Handle : aliased in out Handle_Type);

   --  pipes

   procedure Open_Pipe (
      Reading_Handle : aliased out Handle_Type;
      Writing_Handle : aliased out Handle_Type);

   --  storage mapped I/O

   type Mapping_Type is record
      Storage_Address : Address;
      Storage_Size : Storage_Elements.Storage_Count;
      File_Mapping : C.winnt.HANDLE;
   end record;
   pragma Suppress_Initialization (Mapping_Type);

   procedure Map (
      Mapping : out Mapping_Type;
      Handle : Handle_Type;
      Mode : File_Mode; -- masked by Read_Write_Mask
      Private_Copy : Boolean;
      Offset : Ada.Streams.Stream_Element_Offset; -- 1-origin
      Size : Ada.Streams.Stream_Element_Count);

   procedure Unmap (
      Mapping : in out Mapping_Type;
      Raise_On_Error : Boolean);

   --  exceptions

   function IO_Exception_Id (Error : C.windef.DWORD)
      return Ada.Exception_Identification.Exception_Id;

   Name_Error : exception
      renames Ada.IO_Exceptions.Name_Error;
   Use_Error : exception
      renames Ada.IO_Exceptions.Use_Error;
   Device_Error : exception
      renames Ada.IO_Exceptions.Device_Error;

end System.Native_IO;
