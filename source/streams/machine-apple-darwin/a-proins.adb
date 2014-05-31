with Ada.Exception_Identification.From_Here;
with System.Address_To_Named_Access_Conversions;
with System.Environment_Block;
with System.Zero_Terminated_Strings;
with C.errno;
with C.spawn;
with C.stdlib;
with C.unistd;
package body Ada.Processes.Inside is
   use Exception_Identification.From_Here;
   use type Exception_Identification.Exception_Id;
   use type C.char_ptr;
   use type C.signed_int;
   use type C.size_t;

   procedure Spawn (
      Child : out C.sys.types.pid_t;
      Command_Line : String;
      Directory : String;
      Search_Path : Boolean;
      Input : Streams.Stream_IO.Inside.Handle_Type;
      Output : Streams.Stream_IO.Inside.Handle_Type;
      Error : Streams.Stream_IO.Inside.Handle_Type)
   is
      package char_ptr_Conv is
         new System.Address_To_Named_Access_Conversions (C.char, C.char_ptr);
      Old_Directory : C.char_ptr := null;
      Exception_Id : Exception_Identification.Exception_Id :=
         Exception_Identification.Null_Id;
   begin
      --  set current directory
      if Directory /= "" then
         Old_Directory := C.unistd.getcwd (null, 0);
         declare
            C_Directory : C.char_array (
               0 ..
               Directory'Length * System.Zero_Terminated_Strings.Expanding);
         begin
            System.Zero_Terminated_Strings.To_C (
               Directory,
               C_Directory (0)'Access);
            if C.unistd.chdir (C_Directory (0)'Access) < 0 then
               Exception_Id := Name_Error'Identity;
               goto Cleanup;
            end if;
         end;
      end if;
      --  execute
      declare
         C_Command_Line : C.char_array (
            0 ..
            Command_Line'Length * System.Zero_Terminated_Strings.Expanding);
         Arguments : C.char_ptr_array (0 .. 255) := (others => <>);
         Environment_Block : constant C.char_ptr_ptr :=
            System.Environment_Block;
         Actions : aliased C.spawn.posix_spawn_file_actions_t;
         Attrs : aliased C.spawn.posix_spawnattr_t;
         New_Child : aliased C.sys.types.pid_t;
         Dummy : C.signed_int;
         pragma Unreferenced (Dummy);
         errno : C.signed_int;
      begin
         System.Zero_Terminated_Strings.To_C (
            Command_Line,
            C_Command_Line (0)'Access);
         Split_Argument (C_Command_Line, Arguments);
         Dummy := C.spawn.posix_spawn_file_actions_init (Actions'Access);
         Dummy := C.spawn.posix_spawnattr_init (Attrs'Access);
         if Input /= 0 then
            Dummy := C.spawn.posix_spawn_file_actions_adddup2 (
               Actions'Access,
               0,
               Input);
         end if;
         if Output /= 1 then
            Dummy := C.spawn.posix_spawn_file_actions_adddup2 (
               Actions'Access,
               1,
               Output);
         end if;
         if Error /= 2 then
            Dummy := C.spawn.posix_spawn_file_actions_adddup2 (
               Actions'Access,
               2,
               Error);
         end if;
         if Search_Path then
            errno := C.spawn.posix_spawnp (
               New_Child'Access,
               Arguments (0),
               Actions'Access,
               Attrs'Access,
               Arguments (0)'Access,
               Environment_Block);
         else
            errno := C.spawn.posix_spawn (
               New_Child'Access,
               Arguments (0),
               Actions'Access,
               Attrs'Access,
               Arguments (0)'Access,
               Environment_Block);
         end if;
         case errno is
            when 0 => -- success
               Child := New_Child;
            when C.errno.E2BIG => -- too long arguments
               Exception_Id := Constraint_Error'Identity;
            when C.errno.ENOMEM =>
               Exception_Id := Storage_Error'Identity;
            when C.errno.EINVAL => -- file descriptor
               Exception_Id := Use_Error'Identity;
            when C.errno.EIO =>
               Exception_Id := Device_Error'Identity;
            when others =>
               Exception_Id := Name_Error'Identity;
         end case;
      end;
   <<Cleanup>>
      --  restore current directory
      if Old_Directory /= null then
         if C.unistd.chdir (Old_Directory) < 0 then
            Exception_Id := Name_Error'Identity;
         end if;
         C.stdlib.free (C.void_ptr (char_ptr_Conv.To_Address (Old_Directory)));
      end if;
      if Exception_Id /= Exception_Identification.Null_Id then
         Raise_Exception (Exception_Id);
      end if;
   end Spawn;

end Ada.Processes.Inside;
