with Ada.Exception_Identification.From_Here;
with C.basetsd;
with C.windef;
package body Ada.Storage_Mapped_IO is
   use Exception_Identification.From_Here;
   use type Streams.Stream_Element_Offset;
   use type C.windef.WINBOOL;
   use type C.winnt.ULONGLONG;
--  use type System.Address;
--  use type C.winnt.HANDLE; -- C.void_ptr

   procedure Map (
      Object : in out Non_Controlled_Mapping;
      File : Streams.Stream_IO.Inside.Non_Controlled_File_Type;
      Offset : Streams.Stream_IO.Positive_Count;
      Size : Streams.Stream_IO.Count;
      Writable : Boolean);
   procedure Map (
      Object : in out Non_Controlled_Mapping;
      File : Streams.Stream_IO.Inside.Non_Controlled_File_Type;
      Offset : Streams.Stream_IO.Positive_Count;
      Size : Streams.Stream_IO.Count;
      Writable : Boolean)
   is
      Protects : constant array (Boolean) of C.windef.DWORD := (
         C.winnt.PAGE_READONLY,
         C.winnt.PAGE_READWRITE);
      Accesses : constant array (Boolean) of C.windef.DWORD := (
         C.winbase.FILE_MAP_READ,
         C.winbase.FILE_MAP_WRITE);
      Mapped_Offset : C.winnt.ULARGE_INTEGER;
      Mapped_Size : C.winnt.ULARGE_INTEGER;
      Mapped_Address : C.windef.LPVOID;
      File_Mapping : C.winnt.HANDLE;
   begin
      Mapped_Offset.QuadPart := C.winnt.ULONGLONG (Offset) - 1;
      Mapped_Size.QuadPart := C.winnt.ULONGLONG (Size);
      if Mapped_Size.QuadPart = 0 then
         Mapped_Size.QuadPart :=
            C.winnt.ULONGLONG (Streams.Stream_IO.Inside.Size (File))
            - Mapped_Offset.QuadPart;
      end if;
      File_Mapping := C.winbase.CreateFileMapping (
         Streams.Stream_IO.Inside.Handle (File),
         null,
         Protects (Writable),
         Mapped_Size.HighPart,
         Mapped_Size.LowPart,
         null);
      declare
         use type C.winnt.HANDLE;
      begin
         if File_Mapping = C.winbase.INVALID_HANDLE_VALUE then
            Raise_Exception (Use_Error'Identity);
         end if;
      end;
      Mapped_Address := C.winbase.MapViewOfFileEx (
         File_Mapping,
         Accesses (Writable),
         Mapped_Offset.HighPart,
         Mapped_Offset.LowPart,
         C.basetsd.SIZE_T (Mapped_Size.QuadPart),
         C.windef.LPVOID (System.Null_Address));
      declare
         use type C.windef.LPVOID;
      begin
         if Mapped_Address = C.windef.LPVOID (System.Null_Address) then
            if C.winbase.CloseHandle (File_Mapping) = 0 then
               null; -- raise Use_Error;
            end if;
            Raise_Exception (Use_Error'Identity);
         end if;
      end;
      Object.Address := System.Address (Mapped_Address);
      Object.Size := System.Storage_Elements.Storage_Count (Size);
      Object.File_Mapping := File_Mapping;
   end Map;

   procedure Unmap (
      Object : in out Non_Controlled_Mapping;
      Raise_On_Error : Boolean);
   procedure Unmap (
      Object : in out Non_Controlled_Mapping;
      Raise_On_Error : Boolean) is
   begin
      --  unmap
      if C.winbase.UnmapViewOfFile (C.windef.LPCVOID (Object.Address)) = 0
         or else C.winbase.CloseHandle (Object.File_Mapping) = 0
      then
         if Raise_On_Error then
            Raise_Exception (Use_Error'Identity);
         end if;
      end if;
      --  reset
      Object.Address := System.Null_Address;
      --  close file
      if Streams.Stream_IO.Inside.Is_Open (Object.File) then
         Streams.Stream_IO.Inside.Close (
            Object.File'Access,
            Raise_On_Error => Raise_On_Error);
      end if;
   end Unmap;

   --  implementation

   function Is_Map (Object : Mapping) return Boolean is
      use type System.Address;
      NC_Mapping : constant not null access Non_Controlled_Mapping :=
         Reference (Object);
   begin
      return NC_Mapping.Address /= System.Null_Address;
   end Is_Map;

   procedure Map (
      Object : out Mapping;
      File : Streams.Stream_IO.File_Type;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0)
   is
      pragma Unmodified (Object); -- modified via 'Unrestricted_Access
      use type System.Address;
      NC_Mapping : constant not null access Non_Controlled_Mapping :=
         Reference (Object);
   begin
      --  check already opened
      if NC_Mapping.Address /= System.Null_Address then
         Raise_Exception (Status_Error'Identity);
      end if;
      --  map
      Map (
         NC_Mapping.all,
         Streams.Stream_IO.Inside.Non_Controlled (File).all,
         Offset,
         Size,
         Writable => Streams.Stream_IO.Mode (File) /= In_File);
   end Map;

   procedure Map (
      Object : out Mapping;
      Mode : File_Mode := In_File;
      Name : String;
      Form : String := "";
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0)
   is
      pragma Unmodified (Object); -- modified via 'Unrestricted_Access
      use type System.Address;
      NC_Mapping : constant not null access Non_Controlled_Mapping :=
         Reference (Object);
   begin
      --  check already opened
      if NC_Mapping.Address /= System.Null_Address then
         Raise_Exception (Status_Error'Identity);
      end if;
      --  open file
      --  this file will be closed in Finalize even if any exception is raised
      Streams.Stream_IO.Inside.Open (
         NC_Mapping.File,
         Mode,
         Name,
         Streams.Stream_IO.Inside.Pack (Form));
      Map (
         NC_Mapping.all,
         NC_Mapping.File,
         Offset,
         Size,
         Writable => Mode /= In_File);
   end Map;

   procedure Unmap (Object : in out Mapping) is
      use type System.Address;
      NC_Mapping : constant not null access Non_Controlled_Mapping :=
         Reference (Object);
   begin
      if NC_Mapping.Address = System.Null_Address then
         Raise_Exception (Status_Error'Identity);
      end if;
      Unmap (NC_Mapping.all, Raise_On_Error => True);
   end Unmap;

   function Address (Object : Mapping) return System.Address is
      NC_Mapping : constant not null access Non_Controlled_Mapping :=
         Reference (Object);
   begin
      return NC_Mapping.Address;
   end Address;

   function Size (Object : Mapping)
      return System.Storage_Elements.Storage_Count
   is
      NC_Mapping : constant not null access Non_Controlled_Mapping :=
         Reference (Object);
   begin
      return NC_Mapping.Size;
   end Size;

   package body Controlled is

      function Reference (Object : Mapping)
         return not null access Non_Controlled_Mapping is
      begin
         return Object.Data'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out Mapping) is
         use type System.Address;
      begin
         if Object.Data.Address /= System.Null_Address then
            Unmap (Object.Data, Raise_On_Error => False);
         end if;
      end Finalize;

   end Controlled;

end Ada.Storage_Mapped_IO;
