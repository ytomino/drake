pragma License (Unrestricted);
--  implementation unit specialized for Windows
with Ada.IO_Exceptions;
with Ada.IO_Modes;
with System.Zero_Terminated_WStrings;
with C.winbase;
with C.winnt;
package System.Native_IO is
   pragma Preelaborate;

   subtype Name_Length is C.size_t;
   subtype Name_Character is C.winnt.WCHAR;
   subtype Name_String is C.winnt.WCHAR_array;
   subtype Name_Pointer is C.winnt.LPWSTR;

   subtype Handle_Type is C.winnt.HANDLE;

   Invalid_Handle : constant Handle_Type := C.winbase.INVALID_HANDLE_VALUE;

   --  name

   function Value (
      First : not null access constant Name_Character;
      Length : Name_Length)
      return String
      renames Zero_Terminated_WStrings.Value;

   procedure Free (Item : in out Name_Pointer);

   procedure New_Full_Name (
      Item : String;
      Out_Item : aliased out Name_Pointer; -- Full_Name (Name) & NUL
      Out_Length : out Name_Length);

   procedure New_External_Name (
      Item : String;
      Out_Item : aliased out Name_Pointer; -- '*' & Name & NUL
      Out_Length : out Name_Length);

   --  file management

   procedure Open_Temporary (
      Handle : aliased out Handle_Type;
      Out_Item : aliased out Name_Pointer;
      Out_Length : out Name_Length);

   type Open_Method is (Open, Create, Reset);
   pragma Discard_Names (Open_Method);

   type Packed_Form is record
      Shared : Ada.IO_Modes.File_Shared_Spec;
      Wait : Boolean;
      Overwrite : Boolean;
   end record;
   pragma Suppress_Initialization (Packed_Form);
   pragma Pack (Packed_Form);
   pragma Compile_Time_Error (Packed_Form'Size /= 4, "not packed");

   procedure Open_Ordinary (
      Method : Open_Method;
      Handle : aliased out Handle_Type;
      Mode : Ada.IO_Modes.File_Mode;
      Name : not null Name_Pointer;
      Form : Packed_Form);

   procedure Close_Temporary (
      Handle : Handle_Type;
      Name : not null Name_Pointer;
      Raise_On_Error : Boolean);

   procedure Close_Ordinary (
      Handle : Handle_Type;
      Raise_On_Error : Boolean);

   procedure Delete_Ordinary (
      Handle : Handle_Type;
      Name : not null Name_Pointer;
      Raise_On_Error : Boolean);

--  procedure Set_Close_On_Exec (Handle : Handle_Type);

   function Is_Terminal (Handle : Handle_Type) return Boolean;
   function Is_Seekable (Handle : Handle_Type) return Boolean;

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

   --  exceptions

   Name_Error : exception
      renames Ada.IO_Exceptions.Name_Error;
   Use_Error : exception
      renames Ada.IO_Exceptions.Use_Error;

end System.Native_IO;
