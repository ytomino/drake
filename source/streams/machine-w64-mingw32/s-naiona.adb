with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with Ada.Unchecked_Conversion;
with System.Storage_Map;
with System.System_Allocators;
with System.Zero_Terminated_WStrings;
with C.string;
with C.winternl;
package body System.Native_IO.Names is
   use Ada.Exception_Identification.From_Here;
   use type Storage_Elements.Storage_Offset;
   use type C.char_array;
   use type C.size_t;
   use type C.windef.DWORD;
   use type C.winternl.NTSTATUS;
   use type C.winnt.LPWSTR; -- Name_Pointer
   use type C.winnt.WCHAR; -- Name_Character
   use type C.winnt.WCHAR_array; -- Name_String

   package Name_Pointer_Conv is
      new Address_To_Named_Access_Conversions (
         Name_Character,
         Name_Pointer);

   package OBJECT_NAME_INFORMATION_ptr_Conv is
      new Address_To_Named_Access_Conversions (
         C.winternl.struct_OBJECT_NAME_INFORMATION,
         C.winternl.struct_OBJECT_NAME_INFORMATION_ptr);

   function "+" (Left : Name_Pointer; Right : C.ptrdiff_t)
      return Name_Pointer
      with Convention => Intrinsic;
   pragma Inline_Always ("+");

   function "+" (Left : Name_Pointer; Right : C.ptrdiff_t)
      return Name_Pointer is
   begin
      return Name_Pointer_Conv.To_Pointer (
         Name_Pointer_Conv.To_Address (Left)
            + Storage_Elements.Storage_Offset (Right)
               * (Name_Character'Size / Standard'Storage_Unit));
   end "+";

   type NtQueryObject_Type is access function (
      Handle : C.winnt.HANDLE;
      ObjectInformationClass : C.winternl.OBJECT_INFORMATION_CLASS;
      ObjectInformation : C.winnt.PVOID;
      ObjectInformationLength : C.windef.ULONG;
      ReturnLength : access C.windef.ULONG)
      return C.winternl.NTSTATUS
      with Convention => WINAPI;

   NtQueryObject_Name : constant C.char_array (0 .. 13) :=
      "NtQueryObject" & C.char'Val (0);

   function To_NtQueryObject_Type is
      new Ada.Unchecked_Conversion (C.windef.FARPROC, NtQueryObject_Type);

   --  implementation

   procedure Open_Ordinary (
      Method : Open_Method;
      Handle : aliased out Handle_Type;
      Mode : Ada.IO_Modes.File_Mode;
      Name : String;
      Out_Name : aliased out Name_Pointer;
      Form : Packed_Form)
   is
      C_Name : aliased Name_String (
         0 .. Name'Length * Zero_Terminated_WStrings.Expanding);
   begin
      Zero_Terminated_WStrings.To_C (Name, C_Name (0)'Access);
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
      procedure Finally (X : in out C.winnt.PVOID);
      procedure Finally (X : in out C.winnt.PVOID) is
      begin
         System_Allocators.Free (Address (X));
      end Finally;
      package Holder is
         new Ada.Exceptions.Finally.Scoped_Holder (C.winnt.PVOID, Finally);
      NtQueryObject : constant NtQueryObject_Type :=
         To_NtQueryObject_Type (
            C.winbase.GetProcAddress (
               Storage_Map.NTDLL,
               NtQueryObject_Name (0)'Access));
      Info : aliased C.winnt.PVOID := C.void_ptr (Null_Address);
      ReturnLength : aliased C.windef.ULONG;
      Unicode_Name : C.winternl.PUNICODE_STRING;
      New_Name : Name_Pointer;
      Drives : aliased Name_String (0 .. 3 * 26); -- (A to Z) * "X:\"
      Drive : Name_String (0 .. 2); -- "X:"
      Name_Length : C.size_t;
      Relative_Index : C.size_t;
      Skip_Length : C.size_t;
   begin
      if NtQueryObject = null then
         --  Failed, keep Has_Full_Name and Name.
         if Raise_On_Error then
            raise Program_Error; -- ??
         end if;
         return;
      end if;
      --  Get ObjectNameInformation.
      Holder.Assign (Info);
      Info := C.winnt.PVOID (System_Allocators.Allocate (4096));
      if Address (Info) = Null_Address then
         --  Failed, keep Has_Full_Name and Name.
         if Raise_On_Error then
            raise Storage_Error;
         end if;
         return;
      end if;
      if NtQueryObject (
         Handle,
         C.winternl.ObjectNameInformation,
         Info,
         4096,
         ReturnLength'Access) /= 0 -- STATUS_SUCCESS
      then
         declare
            New_Info : constant C.winnt.PVOID := C.winnt.PVOID (
               System_Allocators.Reallocate (
                  Address (Info),
                  Storage_Elements.Storage_Offset (ReturnLength)));
         begin
            if Address (New_Info) = Null_Address then
               --  Failed, keep Has_Full_Name and Name.
               if Raise_On_Error then
                  raise Storage_Error;
               end if;
               return;
            end if;
            Info := New_Info;
         end;
         if NtQueryObject (
            Handle,
            C.winternl.ObjectNameInformation,
            Info,
            ReturnLength,
            null) /= 0 -- STATUS_SUCCESS
         then
            --  Failed, keep Has_Full_Name and Name.
            if Raise_On_Error then
               Raise_Exception (Use_Error'Identity);
            end if;
            return;
         end if;
      end if;
      Unicode_Name := OBJECT_NAME_INFORMATION_ptr_Conv.To_Pointer (
         Address (Info)).Name'Access;
      --  To DOS name.
      if C.winbase.GetLogicalDriveStrings (Drives'Length, Drives (0)'Access) >=
         Drives'Length
      then
         --  Failed, keep Has_Full_Name and Name.
         if Raise_On_Error then
            Raise_Exception (Use_Error'Identity);
         end if;
         return;
      end if;
      Drive (1) := Name_Character'Val (Character'Pos (':'));
      Drive (2) := Name_Character'Val (0);
      Skip_Length := 0;
      Relative_Index := 0;
      Name_Length := C.size_t (Unicode_Name.Length);
      declare
         Source : Name_String (C.size_t);
         for Source'Address use
            Name_Pointer_Conv.To_Address (Unicode_Name.Buffer);
         P : Name_Pointer := Drives (0)'Unchecked_Access;
      begin
         while P.all /= Name_Character'Val (0) loop
            declare
               Length : constant C.size_t := C.string.wcslen (P);
               Long_Name : aliased Name_String (0 .. C.windef.MAX_PATH - 1);
            begin
               if Length > 0 then
                  Drive (0) := P.all; -- drop trailing '\'
                  if C.winbase.QueryDosDevice (
                     Drive (0)'Access,
                     Long_Name (0)'Access,
                     C.windef.MAX_PATH) /= 0
                  then
                     declare
                        Long_Name_Length : C.size_t := 0;
                     begin
                        while Long_Name (Long_Name_Length) /=
                           Name_Character'Val (0)
                        loop
                           if Long_Name (Long_Name_Length) =
                              Name_Character'Val (Character'Pos (';'))
                           then
                              --  Skip ";X:\" (Is it a bug of VirtualBox?)
                              Long_Name (
                                 Long_Name_Length ..  Long_Name'Last - 4) :=
                                 Long_Name (
                                    Long_Name_Length + 4 .. Long_Name'Last);
                           end if;
                           Long_Name_Length := Long_Name_Length + 1;
                        end loop;
                        if Long_Name (0 .. Long_Name_Length - 1) =
                              Source (0 .. Long_Name_Length - 1)
                           and then Source (Long_Name_Length) =
                              Name_Character'Val (Character'Pos ('\'))
                        then
                           Skip_Length := Long_Name_Length;
                           Relative_Index := 2;
                           Name_Length := Name_Length - Skip_Length + 2;
                           exit;
                        end if;
                     end;
                  end if;
               end if;
               P := P + C.ptrdiff_t (Length);
            end;
            P := P + 1; -- skip NUL
         end loop;
         if Relative_Index = 0
            and then Source (1) /= Name_Character'Val (Character'Pos (':'))
            and then (
               Source (0) /= Name_Character'Val (Character'Pos ('\'))
               or else Source (1) /= Name_Character'Val (Character'Pos ('\')))
         then
            --  For example, a pipe's name is like "pipe:[N]".
            Drive (0) := Name_Character'Val (Character'Pos ('*'));
            Relative_Index := 1;
            Name_Length := Name_Length + 1;
         end if;
      end;
      --  Copy a name.
      New_Name := Name_Pointer_Conv.To_Pointer (
         System_Allocators.Allocate (
            (Storage_Elements.Storage_Offset (Name_Length) + 1)
               * (Name_Character'Size / Standard'Storage_Unit)));
      if New_Name = null then
         --  Failed, keep Has_Full_Name and Name.
         if Raise_On_Error then
            raise Storage_Error;
         end if;
         return;
      end if;
      declare
         Source : Name_String (C.size_t);
         for Source'Address use
            Name_Pointer_Conv.To_Address (Unicode_Name.Buffer);
         Target : Name_String (C.size_t);
         for Target'Address use Name_Pointer_Conv.To_Address (New_Name);
      begin
         if Relative_Index > 0 then
            Target (0 .. Relative_Index - 1) :=
               Drive (0 .. Relative_Index - 1);
         end if;
         Target (Relative_Index .. Name_Length - 1) :=
            Source (Skip_Length .. C.size_t (Unicode_Name.Length) - 1);
         Target (Name_Length) := Name_Character'Val (0);
      end;
      --  Succeeded.
      if not Is_Standard then
         Free (Name); -- External or External_No_Close
      end if;
      Name := New_Name;
      Has_Full_Name := True;
   end Get_Full_Name;

end System.Native_IO.Names;
