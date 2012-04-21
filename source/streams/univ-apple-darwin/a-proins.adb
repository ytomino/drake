with Ada.Environment_Variables.Inside;
with Ada.Exceptions;
with System.Address_To_Named_Access_Conversions;
with C.unistd;
with C.spawn;
with C.stdlib;
package body Ada.Processes.Inside is
   use type Ada.Exceptions.Exception_Id;
   use type C.char_ptr;
   use type C.signed_int;

   procedure Spawn (
      Child : out C.sys.types.pid_t;
      Command_Line : String;
      Directory : String;
      Search_Path : Boolean;
      Input : Streams.Stream_IO.Inside.Handle_Type;
      Output : Streams.Stream_IO.Inside.Handle_Type;
      Error : Streams.Stream_IO.Inside.Handle_Type)
   is
      package char_ptr_Conv is new System.Address_To_Named_Access_Conversions (
         C.char,
         C.char_ptr);
      Old_Directory : C.char_ptr := null;
      Exception_Id : Ada.Exceptions.Exception_Id := Ada.Exceptions.Null_Id;
   begin
      --  set current directory
      if Directory /= "" then
         Old_Directory := C.unistd.getcwd (null, 0);
         declare
            Z_Directory : String := Directory & Character'Val (0);
            C_Directory : C.char_array (0 .. Directory'Length);
            for C_Directory'Address use Z_Directory'Address;
         begin
            if C.unistd.chdir (C_Directory (0)'Access) < 0 then
               Exception_Id := Name_Error'Identity;
               goto Cleanup;
            end if;
         end;
      end if;
      --  execute
      declare
         Z_Command_Line : String := Command_Line & Character'Val (0);
         C_Command_Line : C.char_array (C.size_t);
         for C_Command_Line'Address use Z_Command_Line'Address;
         Arguments : C.char_ptr_array (0 .. 255);
         Environment_Block : constant C.char_ptr_ptr :=
            Ada.Environment_Variables.Inside.Environment_Block;
         Actions : aliased C.spawn.posix_spawn_file_actions_t;
         Attrs : aliased C.spawn.posix_spawnattr_t;
         New_Child : aliased C.sys.types.pid_t;
         Dummy : C.signed_int;
         pragma Unreferenced (Dummy);
         Result : C.signed_int;
      begin
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
            Result := C.spawn.posix_spawnp (
               New_Child'Access,
               Arguments (0),
               Actions'Access,
               Attrs'Access,
               Arguments (0)'Access,
               Environment_Block);
         else
            Result := C.spawn.posix_spawn (
               New_Child'Access,
               Arguments (0),
               Actions'Access,
               Attrs'Access,
               Arguments (0)'Access,
               Environment_Block);
         end if;
         if Result < 0 then
            Exception_Id := Name_Error'Identity;
         else
            Child := New_Child;
         end if;
      end;
   <<Cleanup>>
      --  restore current directory
      if Old_Directory /= null then
         if C.unistd.chdir (Old_Directory) < 0 then
            Exception_Id := Name_Error'Identity;
         end if;
         C.stdlib.free (char_ptr_Conv.To_Address (Old_Directory));
      end if;
      if Exception_Id /= Ada.Exceptions.Null_Id then
         Ada.Exceptions.Raise_Exception (Exception_Id);
      end if;
   end Spawn;

end Ada.Processes.Inside;