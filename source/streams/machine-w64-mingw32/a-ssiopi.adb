with Ada.Exception_Identification.From_Here;
with Ada.Streams.Stream_IO.Inside;
with System;
with C.winbase;
with C.windef;
with C.winnt;
package body Ada.Streams.Stream_IO.Pipes is
   use Exception_Identification.From_Here;
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;

   --  implementation

   procedure Create (Reading, Writing : out File_Type) is
      Inheritable_Security_Attributes : aliased constant
         C.winbase.SECURITY_ATTRIBUTES := (
            nLength =>
               C.winbase.SECURITY_ATTRIBUTES'Size / Standard'Storage_Unit,
            lpSecurityDescriptor => C.windef.LPVOID (System.Null_Address),
            bInheritHandle => 1);
      Reading_Handle, Writing_Handle : aliased C.winnt.HANDLE;
   begin
      if C.winbase.CreatePipe (
         Reading_Handle'Access,
         Writing_Handle'Access,
         Inheritable_Security_Attributes'Unrestricted_Access,
         0) = 0
      then
         Raise_Exception (Use_Error'Identity);
      else
         Inside.Open (
            Reading,
            Reading_Handle,
            In_File,
            To_Close => True);
         Inside.Open (
            Writing,
            Writing_Handle,
            Out_File,
            To_Close => True);
      end if;
   end Create;

end Ada.Streams.Stream_IO.Pipes;
