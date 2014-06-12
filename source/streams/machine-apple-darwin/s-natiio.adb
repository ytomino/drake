with Ada.Exception_Identification.From_Here;
with C.fcntl;
with C.sys.types;
with C.unistd;
package body System.Native_IO is
   use Ada.Exception_Identification.From_Here;
   use type C.signed_int;
   use type C.sys.types.off_t;

   --  implementation

   procedure Set_Close_On_Exec (Handle : Handle_Type) is
      Error : Boolean;
   begin
      Error := C.fcntl.fcntl (
         Handle,
         C.fcntl.F_SETFD,
         C.fcntl.FD_CLOEXEC) < 0;
      if Error then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Set_Close_On_Exec;

   function Is_Terminal (Handle : Handle_Type) return Boolean is
   begin
      return C.unistd.isatty (Handle) /= 0;
   end Is_Terminal;

   function Is_Seekable (Handle : Handle_Type) return Boolean is
   begin
      return C.unistd.lseek (
         Handle,
         0,
         C.unistd.SEEK_CUR) >= 0;
   end Is_Seekable;

end System.Native_IO;
