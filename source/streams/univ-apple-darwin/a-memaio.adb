with C.sys.mman;
with C.sys.types;
package body Ada.Memory_Mapped_IO is
   use type Streams.Stream_IO.Count;
   use type System.Address;
   use type C.signed_int;
   use type C.sys.types.off_t;

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
      Protects : constant array (Boolean) of C.signed_int := (
         C.sys.mman.PROT_READ,
         C.sys.mman.PROT_READ + C.sys.mman.PROT_WRITE);
      Mapped_Address : C.void_ptr;
   begin
      if Object.Address /= System.Null_Address then
         raise Status_Error;
      end if;
      Mapped_Address := C.sys.mman.mmap (
         C.void_ptr (System.Null_Address),
         C.size_t (Size),
         Protects (Writable),
         C.sys.mman.MAP_FILE + C.sys.mman.MAP_SHARED,
         Handle,
         C.sys.types.off_t (Offset) - 1);
      if System.Address (Mapped_Address)
         = System.Address (C.sys.mman.MAP_FAILED)
      then
         raise Use_Error;
      end if;
      Object.Address := System.Address (Mapped_Address);
      Object.Size := System.Storage_Elements.Storage_Count (Size);
   end Map;

   procedure Unmap (
      Object : in out Non_Controlled_Mapping;
      Raise_On_Error : Boolean);
   procedure Unmap (
      Object : in out Non_Controlled_Mapping;
      Raise_On_Error : Boolean) is
   begin
      if C.sys.mman.munmap (
         C.void_ptr (Object.Address),
         C.size_t (Object.Size)) /= 0
      then
         if Raise_On_Error then
            raise Use_Error;
         end if;
      end if;
      if Streams.Stream_IO.Inside.Is_Open (Object.File) then
         Streams.Stream_IO.Inside.Close (
            Object.File,
            Raise_On_Error => Raise_On_Error);
      end if;
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
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      if NC_Mapping.Address = System.Null_Address then
         raise Status_Error;
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
