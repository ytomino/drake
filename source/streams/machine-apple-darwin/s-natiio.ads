pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with Ada.IO_Exceptions;
with System.Zero_Terminated_Strings;
with C;
package System.Native_IO is
   pragma Preelaborate;

   subtype Name_Length is C.size_t;
   subtype Name_Character is C.char;
   subtype Name_String is C.char_array;
   subtype Name_Pointer is C.char_ptr;

   subtype Handle_Type is C.signed_int;

   --  name

   function Value (
      First : not null access constant Name_Character;
      Length : Name_Length)
      return String
      renames Zero_Terminated_Strings.Value;

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

   procedure Set_Close_On_Exec (Handle : Handle_Type);

   function Is_Terminal (Handle : Handle_Type) return Boolean;
   function Is_Seekable (Handle : Handle_Type) return Boolean;

   --  default input and output files

   Standard_Input : constant Handle_Type := 0;
   Standard_Output : constant Handle_Type := 1;
   Standard_Error : constant Handle_Type := 2;

   Uninitialized_Standard_Input : Handle_Type
      renames Standard_Input;
   Uninitialized_Standard_Output : Handle_Type
      renames Standard_Output;
   Uninitialized_Standard_Error : Handle_Type
      renames Standard_Error;

   procedure Initialize (
      Standard_Input_Handle : aliased in out Handle_Type;
      Standard_Output_Handle : aliased in out Handle_Type;
      Standard_Error_Handle : aliased in out Handle_Type) is null;

   --  exceptions

   Use_Error : exception
      renames Ada.IO_Exceptions.Use_Error;

end System.Native_IO;
