pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Streams;
package System.Shared_Storage is

   --  no-operation
   procedure Nop (Key : String) is null;
   function Nop (Key : String)
      return access Ada.Streams.Root_Stream_Type'Class;

   type Lock_Handler is access procedure (Key : String);

   Lock_Hook : not null Lock_Handler := Nop'Access;

   type Unlock_Handler is access procedure (Key : String);

   Unlock_Hook : not null Unlock_Handler := Nop'Access;

   type Read_Handler is access function (Key : String)
      return access Ada.Streams.Root_Stream_Type'Class;

   Read_Hook : not null Read_Handler := Nop'Access;

   type Write_Handler is access function (Key : String)
      return access Ada.Streams.Root_Stream_Type'Class;

   Write_Hook : not null Write_Handler := Nop'Access;

   --  required for a protected in a package with pragma Shared_Passive
   --    by compiler (s-shasto.ads)
   procedure Shared_Var_Lock (Var : String);
   procedure Shared_Var_Unlock (Var : String);

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
