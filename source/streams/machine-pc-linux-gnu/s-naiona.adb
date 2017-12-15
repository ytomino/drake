with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with System.Formatting;
with System.Long_Long_Integer_Types;
with System.Zero_Terminated_Strings;
with C.stdlib;
with C.sys.types;
with C.unistd;
package body System.Native_IO.Names is
   use Ada.Exception_Identification.From_Here;
   use type C.char;
   use type C.char_ptr;
   use type C.size_t;
   use type C.signed_long; -- ssize_t in 64bit Linux

   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;

   package Name_Pointer_Conv is
      new Address_To_Named_Access_Conversions (Name_Character, Name_Pointer);

   proc_self_fd : constant C.char_array (0 .. 13) := "/proc/self/fd/";

   --  implementation

   procedure Open_Ordinary (
      Method : Open_Method;
      Handle : aliased out Handle_Type;
      Mode : File_Mode;
      Name : String;
      Out_Name : aliased out Name_Pointer;
      Form : Packed_Form)
   is
      C_Name : aliased Name_String (
         0 .. Name'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      Open_Ordinary (Method, Handle, Mode, C_Name (0)'Unchecked_Access, Form);
      Out_Name := null;
   end Open_Ordinary;

   procedure Get_Full_Name (
      Handle : Handle_Type;
      Has_Full_Name : in out Boolean;
      Name : in out Name_Pointer;
      Is_Standard : Boolean;
      Raise_On_Error : Boolean)
   is
      package Holder is
         new Ada.Exceptions.Finally.Scoped_Holder (Name_Pointer, Free);
      Link : aliased C.char_array (0 .. 14 + Handle_Type'Width);
      New_Name_Capacity : C.size_t := 1024;
      New_Name_Length : C.size_t;
      New_Name : aliased Name_Pointer :=
         Name_Pointer_Conv.To_Pointer (
            Address (C.stdlib.malloc (New_Name_Capacity)));
   begin
      Holder.Assign (New_Name);
      Link (0 .. 13) := proc_self_fd;
      declare
         Link_As_String : String (1 .. Link'Length);
         for Link_As_String'Address use Link'Address;
         Last : Natural;
         Error : Boolean;
      begin
         Formatting.Image (
            Word_Unsigned (Handle),
            Link_As_String (proc_self_fd'Length + 1 .. Link_As_String'Last),
            Last,
            Error => Error);
         Link (C.size_t (Last)) := C.char'Val (0);
      end;
      loop
         declare
            S_Length : C.sys.types.ssize_t;
         begin
            S_Length :=
               C.unistd.readlink (
                  Link (0)'Access,
                  New_Name,
                  New_Name_Capacity);
            if S_Length < 0 then
               --  Failed, keep Has_Full_Name and Name.
               if Raise_On_Error then
                  Raise_Exception (Use_Error'Identity);
               end if;
               return;
            end if;
            New_Name_Length := C.size_t (S_Length);
         end;
         exit when New_Name_Length < New_Name_Capacity; -- success
         New_Name_Capacity := New_Name_Capacity * 2;
         declare
            New_New_Name : constant Name_Pointer :=
               Name_Pointer_Conv.To_Pointer (
                  Address (
                     C.stdlib.realloc (
                        C.void_ptr (
                           Name_Pointer_Conv.To_Address (New_Name)),
                        New_Name_Capacity)));
         begin
            if New_New_Name = null then
               --  Failed, keep Has_Full_Name and Name.
               if Raise_On_Error then
                  raise Storage_Error;
               end if;
               return;
            end if;
            New_Name := New_New_Name;
         end;
      end loop;
      --  Succeeded.
      if New_Name.all /= '/' then
         --  For example, a pipe's name is like "pipe:[N]".
         declare
            New_New_Name : constant Name_Pointer :=
               Name_Pointer_Conv.To_Pointer (
                  Address (
                     C.stdlib.realloc (
                        C.void_ptr (Name_Pointer_Conv.To_Address (New_Name)),
                        C.size_t (New_Name_Length + 2)))); -- '*' & NUL
         begin
            if New_New_Name = null then
               --  Failed, keep Has_Full_Name and Name.
               if Raise_On_Error then
                  raise Storage_Error;
               end if;
               return;
            end if;
            New_Name := New_New_Name;
         end;
         declare
            New_Name_All : C.char_array (
               0 .. New_Name_Length + 1); -- '*' & NUL
            for New_Name_All'Address use
               Name_Pointer_Conv.To_Address (New_Name);
         begin
            New_Name_All (1 .. New_Name_Length) :=
               New_Name_All (0 .. New_Name_Length - 1);
            New_Name_All (0) := '*';
            New_Name_All (New_Name_Length + 1) := C.char'Val (0);
         end;
      end if;
      if not Is_Standard then
         Free (Name); -- External or External_No_Close
      end if;
      Name := New_Name;
      Has_Full_Name := True;
      Holder.Clear;
   end Get_Full_Name;

end System.Native_IO.Names;
