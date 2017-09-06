with C.elf;
with C.link;
with C.sys.types;
package body System.Storage_Map is
   pragma Suppress (All_Checks);
   use type C.signed_int;
   use type C.signed_long; -- 64bit ssize_t

   type Rec is record
      First_Load_Address : Address;
   end record;
   pragma Suppress_Initialization (Rec);

   function Callback (
      Info : access C.link.struct_dl_phdr_info;
      Size : C.size_t;
      Data : C.void_ptr)
      return C.signed_int
      with Convention => C;

   function Callback (
      Info : access C.link.struct_dl_phdr_info;
      Size : C.size_t;
      Data : C.void_ptr)
      return C.signed_int
   is
      pragma Unreferenced (Size);
      R : Rec;
      for R'Address use Address (Data);
   begin
      if Standard'Address_Size <= 32 then
         declare
            use type C.elf.Elf32_Half; -- dlpi_phnum
            use type C.elf.Elf32_Word; -- p_type and p_vaddr
            type Elf32_Phdr_array is
               array (C.sys.types.ssize_t range <>) of aliased C.elf.Elf32_Phdr
               with Convention => C;
            dlpi_phdr : Elf32_Phdr_array (
               0 .. C.sys.types.ssize_t (Info.dlpi_phnum) - 1);
            for dlpi_phdr'Address use Info.dlpi_phdr.all'Address;
         begin
            for I in dlpi_phdr'Range loop
               if dlpi_phdr (I).p_type = C.elf.PT_LOAD then
                  R.First_Load_Address := System'To_Address (
                     dlpi_phdr (I).p_vaddr
                     + C.elf.Elf32_Addr'Mod (Info.dlpi_addr));
                  return 1; -- finish
               end if;
            end loop;
         end;
      else
         declare
            use type C.elf.Elf64_Half; -- dlpi_phnum
            use type C.elf.Elf64_Word; -- p_type
            use type C.elf.Elf64_Addr; -- p_vaddr
            type Elf64_Phdr_array is
               array (C.sys.types.ssize_t range <>) of aliased C.elf.Elf64_Phdr
               with Convention => C;
            dlpi_phdr : Elf64_Phdr_array (
               0 .. C.sys.types.ssize_t (Info.dlpi_phnum) - 1);
            for dlpi_phdr'Address use Info.dlpi_phdr.all'Address;
         begin
            for I in dlpi_phdr'Range loop
               if dlpi_phdr (I).p_type = C.elf.PT_LOAD then
                  R.First_Load_Address := System'To_Address (
                     dlpi_phdr (I).p_vaddr
                     + C.elf.Elf64_Addr'Mod (Info.dlpi_addr));
                  return 1; -- finish
               end if;
            end loop;
         end;
      end if;
      return 0; -- continue
   end Callback;

   --  implementation

   function Load_Address return Address is
      R : aliased Rec;
      Dummy : C.signed_int;
   begin
      R.First_Load_Address := Null_Address;
      Dummy := C.link.dl_iterate_phdr (
         Callback'Access,
         C.void_ptr (R'Address));
      return R.First_Load_Address;
   end Load_Address;

end System.Storage_Map;
