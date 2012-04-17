with Ada.Exceptions.Finally;
with C.sys.mman;
with C.sys.types;
package body Ada.Memory_Mapped_IO is
   use type Streams.Stream_IO.Count;
   use type System.Address;
   use type C.signed_int;
   use type C.sys.types.off_t;

   procedure Map (
      Object : out Mapping;
      Handle : Streams.Stream_IO.Inside.Handle_Type;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0;
      Writable : Boolean);
   procedure Map (
      Object : out Mapping;
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

   procedure Unmap (Object : in out Mapping; Raise_On_Error : Boolean);
   procedure Unmap (Object : in out Mapping; Raise_On_Error : Boolean) is
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
   begin
      return Object.Address /= System.Null_Address;
   end Is_Map;

   procedure Map (
      Object : out Mapping;
      File : Streams.Stream_IO.File_Type;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0)
   is
      Map_Size : Streams.Stream_IO.Count := Size;
   begin
      if Map_Size = 0 then
         Map_Size := Streams.Stream_IO.Size (File);
      end if;
      Map (
         Object,
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
      Size : Streams.Stream_IO.Count := 0) is
   begin
      Streams.Stream_IO.Inside.Open (Object.File, Mode, Name, Form);
      declare
         procedure Finally (X : not null access
            Streams.Stream_IO.Inside.Non_Controlled_File_Type);
         procedure Finally (X : not null access
            Streams.Stream_IO.Inside.Non_Controlled_File_Type) is
         begin
            Streams.Stream_IO.Inside.Close (
               X.all,
               Raise_On_Error => False);
         end Finally;
         package Holder is new Exceptions.Finally.Scoped_Holder (
            Streams.Stream_IO.Inside.Non_Controlled_File_Type,
            Finally);
      begin
         Holder.Assign (Object.File'Access);
         declare
            Map_Size : Streams.Stream_IO.Count := Size;
         begin
            if Map_Size = 0 then
               Map_Size := Streams.Stream_IO.Inside.Size (Object.File);
            end if;
            Map (
               Object,
               Streams.Stream_IO.Inside.Handle (Object.File),
               Offset,
               Map_Size,
               Writable => Mode /= In_File);
         end;
         Holder.Clear;
      end;
   end Map;

   procedure Unmap (Object : in out Mapping) is
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      if Object.Address = System.Null_Address then
         raise Status_Error;
      end if;
      Unmap (Object, Raise_On_Error => True);
   end Unmap;

   function Address (Object : Mapping) return System.Address is
   begin
      return Object.Address;
   end Address;

   function Size (Object : Mapping)
      return System.Storage_Elements.Storage_Count is
   begin
      return Object.Size;
   end Size;

   overriding procedure Finalize (Object : in out Mapping) is
   begin
      if Object.Address /= System.Null_Address then
         Unmap (Object, Raise_On_Error => False);
      end if;
   end Finalize;

end Ada.Memory_Mapped_IO;
