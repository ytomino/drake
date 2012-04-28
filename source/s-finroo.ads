pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Finalization_Root is
   pragma Pure;

   --  required for controlled type by compiler (s-finroo.ads)

   type Root_Controlled is abstract tagged null record;

   procedure Adjust (Object : in out Root_Controlled) is null;
   procedure Initialize (Object : in out Root_Controlled) is null;
   procedure Finalize (Object : in out Root_Controlled) is null;

end System.Finalization_Root;
