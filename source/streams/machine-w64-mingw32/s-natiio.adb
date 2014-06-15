with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.Storage_Elements;
with C.wincon;
with C.windef;
package body System.Native_IO is
   use type Storage_Elements.Storage_Offset;
   use type C.size_t; -- Name_Length
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;

   package Name_Pointer_Conv is
      new Address_To_Named_Access_Conversions (Name_Character, Name_Pointer);

   --  implementation

   procedure Free (Item : in out Name_Pointer) is
   begin
      Standard_Allocators.Free (Name_Pointer_Conv.To_Address (Item));
      Item := null;
   end Free;

   procedure New_Full_Name (
      Item : String;
      Out_Item : aliased out Name_Pointer;
      Out_Length : out Name_Length)
   is
      W_Item : aliased Name_String (
         0 ..
         Item'Length * Zero_Terminated_WStrings.Expanding);
      W_Item_Length : Name_Length;
      Full_Path_Buffer : aliased Name_String (0 .. C.windef.MAX_PATH - 1);
      Full_Path_Pointer : not null access C.winnt.WCHAR :=
         Full_Path_Buffer (0)'Access;
      Full_Path_Length : Name_Length;
   begin
      Zero_Terminated_WStrings.To_C (
         Item,
         W_Item (0)'Access,
         W_Item_Length);
      Full_Path_Length := Name_Length (
         C.winbase.GetFullPathName (
            W_Item (0)'Access,
            Full_Path_Buffer'Length,
            Full_Path_Pointer,
            null));
      if Full_Path_Length = 0 then -- GetFullPathName failed
         Full_Path_Pointer := W_Item (0)'Access;
         Full_Path_Length := W_Item_Length;
      end if;
      --  allocate filename
      Out_Length := Full_Path_Length;
      Out_Item := Name_Pointer_Conv.To_Pointer (
         Standard_Allocators.Allocate (
            Storage_Elements.Storage_Offset (Out_Length + 1) -- NUL
            * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
      declare
         Full_Path_A : Name_String (Name_Length);
         for Full_Path_A'Address use
            Name_Pointer_Conv.To_Address (Full_Path_Pointer);
         Out_Item_A : Name_String (Name_Length);
         for Out_Item_A'Address use Name_Pointer_Conv.To_Address (Out_Item);
      begin
         Out_Item_A (0 .. Out_Length) := Full_Path_A (0 .. Out_Length);
      end;
   end New_Full_Name;

   procedure New_External_Name (
      Item : String;
      Out_Item : aliased out Name_Pointer; -- '*' & Name & NUL
      Out_Length : out Name_Length) is
   begin
      Out_Item := Name_Pointer_Conv.To_Pointer (
         Standard_Allocators.Allocate (
            Storage_Elements.Storage_Offset (
               Item'Length * Zero_Terminated_WStrings.Expanding
               + 2) -- '*' & NUL
            * (C.winnt.WCHAR'Size / Standard'Storage_Unit)));
      declare
         Out_Item_A : Name_String (Name_Length);
         for Out_Item_A'Address use Name_Pointer_Conv.To_Address (Out_Item);
      begin
         Out_Item_A (0) := Name_Character'Val (Wide_Character'Pos ('*'));
         Zero_Terminated_WStrings.To_C (
            Item,
            Out_Item_A (1)'Access,
            Out_Length);
      end;
      Out_Length := Out_Length + 1; -- '*'
   end New_External_Name;

   function Is_Terminal (Handle : Handle_Type) return Boolean is
      Mode : aliased C.windef.DWORD;
   begin
      return C.winbase.GetFileType (Handle) = C.winbase.FILE_TYPE_CHAR
         and then C.wincon.GetConsoleMode (Handle, Mode'Access) /= 0;
   end Is_Terminal;

   function Is_Seekable (Handle : Handle_Type) return Boolean is
   begin
      return C.winbase.SetFilePointerEx (
         Handle,
         (Unchecked_Tag => 2, QuadPart => 0),
         null,
         C.winbase.FILE_CURRENT) /= 0;
   end Is_Seekable;

   function Standard_Input return Handle_Type is
   begin
      return C.winbase.GetStdHandle (C.winbase.STD_INPUT_HANDLE);
   end Standard_Input;

   function Standard_Output return Handle_Type is
   begin
      return C.winbase.GetStdHandle (C.winbase.STD_OUTPUT_HANDLE);
   end Standard_Output;

   function Standard_Error return Handle_Type is
   begin
      return C.winbase.GetStdHandle (C.winbase.STD_ERROR_HANDLE);
   end Standard_Error;

   procedure Initialize (
      Standard_Input_Handle : aliased in out Handle_Type;
      Standard_Output_Handle : aliased in out Handle_Type;
      Standard_Error_Handle : aliased in out Handle_Type) is
   begin
      Standard_Input_Handle := Standard_Input;
      Standard_Output_Handle := Standard_Output;
      Standard_Error_Handle := Standard_Error;
   end Initialize;

end System.Native_IO;
