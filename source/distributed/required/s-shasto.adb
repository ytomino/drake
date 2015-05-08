pragma Check_Policy (Trace => Ignore);
package body System.Shared_Storage is

   function Nop (Key : String)
      return access Ada.Streams.Root_Stream_Type'Class
   is
      pragma Unreferenced (Key);
   begin
      return null;
   end Nop;

   procedure Shared_Var_Lock (Var : String) is
   begin
      pragma Check (Trance, Ada.Debug.Put (Var));
      Lock_Hook (Var);
   end Shared_Var_Lock;

   procedure Shared_Var_Unlock (Var : String) is
   begin
      pragma Check (Trance, Ada.Debug.Put (Var));
      Unlock_Hook (Var);
   end Shared_Var_Unlock;

   package body Shared_Var_Procs is

      procedure Read is
         pragma Check (Trance, Ada.Debug.Put (Full_Name));
         Stream : constant access Ada.Streams.Root_Stream_Type'Class :=
            Read_Hook (Full_Name);
      begin
         if Stream /= null then
            Typ'Read (Stream, V);
         end if;
      end Read;

      procedure Write is
         pragma Check (Trance, Ada.Debug.Put (Full_Name));
         Stream : constant access Ada.Streams.Root_Stream_Type'Class :=
            Write_Hook (Full_Name);
      begin
         if Stream /= null then
            Typ'Write (Stream, V);
         end if;
      end Write;

   end Shared_Var_Procs;

end System.Shared_Storage;
