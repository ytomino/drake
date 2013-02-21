with Ada.Exceptions;
with C.basetsd;
with C.windef;
package body Ada.Memory_Mapped_IO is
   use type Ada.Streams.Stream_Element_Offset;
   use type System.Address;
   use type C.windef.WINBOOL;

   procedure Map (
      Object : in out Non_Controlled_Mapping;
      Handle : Streams.Stream_IO.Inside.Handle_Type;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0;
      Writable : Boolean);
   procedure Map (
      Object : in out Non_Controlled_Mapping;
      Handle : Streams.Stream_IO.Inside.Handle_Type;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0;
      Writable : Boolean)
   is
      Large_Size, Large_Offset : C.winnt.ULARGE_INTEGER;
      Protects : constant array (Boolean) of C.windef.DWORD := (
         C.winnt.PAGE_READONLY,
         C.winnt.PAGE_READWRITE);
      Accesses : constant array (Boolean) of C.windef.DWORD := (
         C.winbase.FILE_MAP_READ,
         C.winbase.FILE_MAP_WRITE);
      Mapped_Address : C.windef.LPVOID;
      File_Mapping : C.winnt.HANDLE;
   begin
      if Object.Address /= System.Null_Address then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Large_Size.QuadPart := C.winnt.ULONGLONG (Size);
      File_Mapping := C.winbase.CreateFileMapping (
         Handle,
         null,
         Protects (Writable),
         Large_Size.HighPart,
         Large_Size.LowPart,
         null);
      if File_Mapping = C.winbase.INVALID_HANDLE_VALUE then
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
      Large_Offset.QuadPart := C.winnt.ULONGLONG (Offset - 1);
      Mapped_Address := C.winbase.MapViewOfFileEx (
         File_Mapping,
         Accesses (Writable),
         Large_Offset.HighPart,
         Large_Offset.LowPart,
         C.basetsd.SIZE_T (Size),
         C.windef.LPVOID (System.Null_Address));
      if Mapped_Address = C.windef.LPVOID (System.Null_Address) then
         if C.winbase.CloseHandle (File_Mapping) = 0 then
            null; -- raise Use_Error;
         end if;
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
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
      if C.winbase.UnmapViewOfFile (Object.Address) = 0
         or else C.winbase.CloseHandle (Object.File_Mapping) = 0
      then
         if Raise_On_Error then
            Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
         end if;
      end if;
      --  close file
      if Streams.Stream_IO.Inside.Is_Open (Object.File) then
         Streams.Stream_IO.Inside.Close (
            Object.File,
            Raise_On_Error => Raise_On_Error);
      end if;
      --  reset
      Object.Address := System.Null_Address;
   end Unmap;

   --  implementation

   function Is_Map (Object : Mapping) return Boolean is
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
      Map_Size : Streams.Stream_IO.Count := Size;
   begin
      if Map_Size = 0 then
         Map_Size := Streams.Stream_IO.Size (File);
      end if;
      Map (
         Reference (Object).all,
         Streams.Stream_IO.Inside.Handle (File),
         Offset,
         Map_Size,
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
      NC_Mapping : constant not null access Non_Controlled_Mapping :=
         Reference (Object);
   begin
      Streams.Stream_IO.Inside.Open (NC_Mapping.File, Mode, Name, Form);
      --  the file will be closed in Finalize if any exception is raised or not
      declare
         Map_Size : Streams.Stream_IO.Count := Size;
      begin
         if Map_Size = 0 then
            Map_Size := Streams.Stream_IO.Inside.Size (NC_Mapping.File);
         end if;
         Map (
            NC_Mapping.all,
            Streams.Stream_IO.Inside.Handle (NC_Mapping.File),
            Offset,
            Map_Size,
            Writable => Mode /= In_File);
      end;
   end Map;

   procedure Unmap (Object : in out Mapping) is
      NC_Mapping : constant not null access Non_Controlled_Mapping :=
         Reference (Object);
   begin
      if NC_Mapping.Address = System.Null_Address then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
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
      begin
         if Object.Data.Address /= System.Null_Address then
            Unmap (Object.Data, Raise_On_Error => False);
         end if;
      end Finalize;

   end Controlled;

end Ada.Memory_Mapped_IO;
