pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Streams;
package System.Shared_Storage is

   --  no-operation
   procedure Nop (Key : String) is null;
   function Nop (Key : String)
      return access Ada.Streams.Root_Stream_Type'Class;

   type Lock_Handler is access procedure (Key : String);

   Lock_Hook : Lock_Handler := Nop'Access;
   pragma Suppress (Access_Check, Lock_Hook); -- not null

   type Unlock_Handler is access procedure (Key : String);

   Unlock_Hook : Unlock_Handler := Nop'Access;
   pragma Suppress (Access_Check, Unlock_Hook); -- not null

   type Read_Handler is access function (Key : String)
      return access Ada.Streams.Root_Stream_Type'Class;

   Read_Hook : Read_Handler := Nop'Access;
   pragma Suppress (Access_Check, Read_Hook); -- not null

   type Write_Handler is access function (Key : String)
      return access Ada.Streams.Root_Stream_Type'Class;

   Write_Hook : Write_Handler := Nop'Access;
   pragma Suppress (Access_Check, Write_Hook); -- not null

   --  required for a protected in a package with pragma Shared_Passive
   --    by compiler (s-shasto.ads)
   procedure Shared_Var_Lock (Var : String);
   pragma Inline (Shared_Var_Lock);
   procedure Shared_Var_Unlock (Var : String);
   pragma Inline (Shared_Var_Unlock);

   --  required for a variable in a package with pragma Shared_Passive
   --    by compiler (s-shasto.ads)
   generic
      type Typ is limited private;
      V : in out Typ;
      Full_Name : String;
   package Shared_Var_Procs is
      procedure Read;
      procedure Write;
   end Shared_Var_Procs;

end System.Shared_Storage;
