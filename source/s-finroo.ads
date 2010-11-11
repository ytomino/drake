pragma License (Unrestricted);
--  implementation package required by compiler
with Ada.Streams;
package System.Finalization_Root is
   pragma Pure;

   --  required for controlled type by compiler (s-finroo.ads)

   type Root_Controlled;

   type Finalizable_Ptr is access all Root_Controlled'Class;
   for Finalizable_Ptr'Storage_Size use 0;
   pragma No_Strict_Aliasing (Finalizable_Ptr);

   type Root_Controlled is tagged record
      Prev, Next : Finalizable_Ptr;
   end record;
   --  not abstract for Ada.Finalization.List_Controller

   procedure Initialize (Object : in out Root_Controlled) is null;
   procedure Finalize (Object : in out Root_Controlled) is null;
   procedure Adjust (Object : in out Root_Controlled) is null;

   function "=" (Left, Right : Root_Controlled) return Boolean;

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

   --  required for controlled type by compiler (s-finroo.ads)
   subtype Finalizable is Root_Controlled'Class;

end System.Finalization_Root;
