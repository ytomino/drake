pragma License (Unrestricted);
--  implementation package required by compiler
with Ada.Streams;
with System.Finalization_Root;
package Ada.Finalization.List_Controller is

   --  unnecessary ??? (a-filico.ads)
   type Simple_List_Controller is
      new Ada.Finalization.Limited_Controlled with
   record
      F : System.Finalization_Root.Finalizable_Ptr;
   end record;
   overriding procedure Finalize (Object : in out Simple_List_Controller);

   package No_Primitives_For_Simple is
      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Simple_List_Controller) is null;
      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Simple_List_Controller) is null;
   end No_Primitives_For_Simple;

   for Simple_List_Controller'Read use No_Primitives_For_Simple.Read;
   for Simple_List_Controller'Write use No_Primitives_For_Simple.Write;

   --  required for access class-wide type by compiler (a-filico.ads)
   type List_Controller is new Finalization.Limited_Controlled with record
      F : System.Finalization_Root.Finalizable_Ptr;
      Item : aliased System.Finalization_Root.Root_Controlled;
      --  in original libgnat, First and Last ... renamable ???
   end record;

   overriding procedure Initialize (Object : in out List_Controller);
   overriding procedure Finalize (Object : in out List_Controller);

   package No_Primitives is
      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : List_Controller) is null;
      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out List_Controller) is null;
   end No_Primitives;

   for List_Controller'Read use No_Primitives.Read;
   for List_Controller'Write use No_Primitives.Write;

end Ada.Finalization.List_Controller;
