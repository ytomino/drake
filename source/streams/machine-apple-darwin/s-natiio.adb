with Ada.Exception_Identification.From_Here;
with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with C.fcntl;
with C.stdlib;
with C.sys.types;
with C.unistd;
package body System.Native_IO is
   use Ada.Exception_Identification.From_Here;
   use type C.char; -- Name_Character
   use type C.char_ptr; -- Name_Pointer
   use type C.signed_int;
   use type C.size_t; -- Name_Length
   use type C.sys.types.off_t;

   function strlen (Item : not null access constant C.char) return C.size_t;
   pragma Import (Intrinsic, strlen, "__builtin_strlen");

   package Name_Pointer_Conv is
      new Address_To_Named_Access_Conversions (Name_Character, Name_Pointer);

   --  implementation

   procedure Free (Item : in out Name_Pointer) is
   begin
      C.stdlib.free (C.void_ptr (Name_Pointer_Conv.To_Address (Item)));
      Item := null;
   end Free;

   procedure New_Full_Name (
      Item : String;
      Out_Item : aliased out Name_Pointer;
      Out_Length : out Name_Length)
   is
      function To_Address is
         new Ada.Unchecked_Conversion (C.void_ptr, Address);
   begin
      if Item (Item'First) = '/' then
         --  absolute path
         Out_Item := Name_Pointer_Conv.To_Pointer (
            To_Address (
               C.stdlib.malloc (
                  Item'Length * Zero_Terminated_Strings.Expanding
                  + 1))); -- NUL
         if Out_Item = null then
            raise Storage_Error;
         end if;
         Out_Length := 0;
      else
         --  current directory
         Out_Item := C.unistd.getcwd (null, 0);
         Out_Length := strlen (Out_Item);
         --  reuse the memory from malloc (similar to reallocf)
         declare
            New_Out_Item : constant Name_Pointer :=
               Name_Pointer_Conv.To_Pointer (
                  To_Address (
                     C.stdlib.realloc (
                        C.void_ptr (Name_Pointer_Conv.To_Address (Out_Item)),
                        Out_Length
                           + Item'Length * Zero_Terminated_Strings.Expanding
                           + 2))); -- '/' & NUL
         begin
            if New_Out_Item = null then
               raise Storage_Error;
            end if;
            Out_Item := New_Out_Item;
         end;
         --  append slash
         declare
            Out_Item_A : Name_String (Name_Length);
            for Out_Item_A'Address use Name_Pointer_Conv.To_Address (Out_Item);
         begin
            if Out_Item_A (Out_Length - 1) /= '/' then
               Out_Item_A (Out_Length) := '/';
               Out_Length := Out_Length + 1;
            end if;
         end;
      end if;
      --  append Item
      declare
         Out_Item_A : Name_String (Name_Length);
         for Out_Item_A'Address use Name_Pointer_Conv.To_Address (Out_Item);
         Appended_Name_Length : Name_Length;
      begin
         Zero_Terminated_Strings.To_C (
            Item,
            Out_Item_A (Out_Length)'Access,
            Appended_Name_Length);
         Out_Length := Out_Length + Appended_Name_Length;
      end;
   end New_Full_Name;

   procedure New_External_Name (
      Item : String;
      Out_Item : aliased out Name_Pointer;
      Out_Length : out Name_Length)
   is
      function To_Address is
         new Ada.Unchecked_Conversion (C.void_ptr, Address);
   begin
      Out_Item := Name_Pointer_Conv.To_Pointer (
         To_Address (
            C.stdlib.malloc (
               Item'Length * Zero_Terminated_Strings.Expanding
               + 2))); -- '*' & NUL
      if Out_Item = null then
         raise Storage_Error;
      end if;
      declare
         Out_Item_A : Name_String (Name_Length);
         for Out_Item_A'Address use Name_Pointer_Conv.To_Address (Out_Item);
      begin
         Out_Item_A (0) := '*';
         Zero_Terminated_Strings.To_C (
            Item,
            Out_Item_A (1)'Access,
            Out_Length);
      end;
      Out_Length := Out_Length + 1; -- '*'
   end New_External_Name;

   procedure Set_Close_On_Exec (Handle : Handle_Type) is
      Error : Boolean;
   begin
      Error := C.fcntl.fcntl (
         Handle,
         C.fcntl.F_SETFD,
         C.fcntl.FD_CLOEXEC) < 0;
      if Error then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Set_Close_On_Exec;

   function Is_Terminal (Handle : Handle_Type) return Boolean is
   begin
      return C.unistd.isatty (Handle) /= 0;
   end Is_Terminal;

   function Is_Seekable (Handle : Handle_Type) return Boolean is
   begin
      return C.unistd.lseek (
         Handle,
         0,
         C.unistd.SEEK_CUR) >= 0;
   end Is_Seekable;

end System.Native_IO;
