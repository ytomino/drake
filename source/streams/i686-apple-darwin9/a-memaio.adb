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

   function Address (Object : Mapping) return System.Address is
   begin
      return Object.Address;
   end Address;

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

   --  local
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

   procedure Unmap (Object : in out Mapping) is
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      if Object.Address = System.Null_Address then
         raise Status_Error;
      end if;
      Dummy := C.sys.mman.munmap (
         C.void_ptr (Object.Address),
         C.size_t (Object.Size));
      if Streams.Stream_IO.Inside.Is_Open (Object.File) then
         Streams.Stream_IO.Inside.Close (Object.File);
      end if;
      Object.Address := System.Null_Address;
   end Unmap;

   function Size (Object : Mapping)
      return System.Storage_Elements.Storage_Count is
   begin
      return Object.Size;
   end Size;

end Ada.Memory_Mapped_IO;
