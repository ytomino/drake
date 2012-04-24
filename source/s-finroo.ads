pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Streams;
package System.Finalization_Root is
   pragma Pure;

   --  required for controlled type by compiler (s-finroo.ads)

   type Root_Controlled is abstract tagged null record;

   procedure Adjust (Object : in out Root_Controlled) is null;
   procedure Initialize (Object : in out Root_Controlled) is null;
   procedure Finalize (Object : in out Root_Controlled) is null;

   package No_Primitives is
      procedure Write (
         Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : Root_Controlled) is null;
      procedure Read (
         Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : out Root_Controlled) is null;
   end No_Primitives;

   for Root_Controlled'Read use No_Primitives.Read;
   for Root_Controlled'Write use No_Primitives.Write;

end System.Finalization_Root;
