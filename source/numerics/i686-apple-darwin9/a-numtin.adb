with C.sys.fcntl;
with C.sys.types;
with C.unistd;
procedure Ada.Numerics.MT19937.Initiator (Item : out Cardinal_Vector) is
   pragma Suppress (All_Checks);
   use type C.char_array;
   use type C.signed_int; -- ssize_t is signed int or signed long
   use type C.signed_long;
   use type C.size_t;
   Random_File_Name : constant C.char_array := "/dev/random" & C.char'Val (0);
   Size : constant C.size_t := Item'Size / Standard'Storage_Unit;
   F : C.signed_int;
   Read_Size : C.sys.types.ssize_t;
   Closed : C.signed_int;
begin
   F := C.sys.fcntl.open (
      Random_File_Name (0)'Access,
      C.sys.fcntl.O_RDONLY);
   if F = -1 then
      raise Use_Error;
   end if;
   Read_Size := C.unistd.read (F, C.void_ptr (Item'Address), Size);
   Closed := C.unistd.close (F);
   if Read_Size /= C.sys.types.ssize_t (Size) or else Closed = -1 then
      raise Use_Error;
   end if;
end Ada.Numerics.MT19937.Initiator;
