with Ada.Exception_Identification.From_Here;
with C.fcntl;
with C.sys.types;
with C.unistd;
package body System.Random_Initiators is
   use Ada.Exception_Identification.From_Here;
   use type Storage_Elements.Storage_Offset;
   use type C.char_array;
   use type C.signed_int; -- ssize_t is signed int or signed long
   use type C.signed_long;

   Random_File_Name : constant C.char_array := "/dev/urandom" & C.char'Val (0);

   procedure Get (
      Item : Address;
      Size : Storage_Elements.Storage_Count)
   is
      Handle : C.signed_int;
      Error : Boolean;
   begin
      Handle := C.fcntl.open (
         Random_File_Name (0)'Access,
         C.fcntl.O_RDONLY);
      if Handle < 0 then
         Raise_Exception (Use_Error'Identity);
      end if;
      Error := False;
      declare
         Total_Read_Size : Storage_Elements.Storage_Count := 0;
      begin
         while Total_Read_Size < Size loop
            declare
               Read_Size : C.sys.types.ssize_t;
            begin
               Read_Size :=
                  C.unistd.read (
                     Handle,
                     C.void_ptr (Item + Total_Read_Size),
                     C.size_t (Size - Total_Read_Size));
               if Read_Size < 0 then
                  Error := True;
                  exit;
               end if;
               Total_Read_Size :=
                  Total_Read_Size
                  + Storage_Elements.Storage_Offset (Read_Size);
            end;
         end loop;
      end;
      if C.unistd.close (Handle) < 0 then
         Error := True;
      end if;
      if Error then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Get;

end System.Random_Initiators;
