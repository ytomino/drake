pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Streams;
with System.Finalization_Root;
package System.Finalization_Implementation is
   pragma Preelaborate;

   --  used in Ada.Finalization.List_Controller (s-finimp.ads)
   Collection_Finalization_Started : constant Address := System'To_Address (1);

   --  required for controlled type by compiler (s-finimp.ads)

   procedure Attach_To_Final_List (
      L : in out Finalization_Root.Finalizable_Ptr;
      Obj : in out Finalization_Root.Finalizable;
      Nb_Link : Short_Short_Integer);

   procedure Detach_From_Final_List (
      Obj : in out Finalization_Root.Finalizable);

   procedure Deep_Tag_Attach (
      L : in out Finalization_Root.Finalizable_Ptr;
      A : Address;
      B : Short_Short_Integer);

   procedure Finalize_List (L : Finalization_Root.Finalizable_Ptr);
   procedure Finalize_One (Obj : in out Finalization_Root.Finalizable);

   type Finalizable_Ptr_Ptr is access all Finalization_Root.Finalizable_Ptr;

   procedure Move_Final_List (
      From : in out Finalization_Root.Finalizable_Ptr;
      To : Finalizable_Ptr_Ptr);

   Global_Final_List : Finalization_Root.Finalizable_Ptr;

   --  record controller (s-finimp.ads)

   type Limited_Record_Controller is
      new Finalization_Root.Root_Controlled with
   record
      F : Finalization_Root.Finalizable_Ptr;
   end record;

   overriding procedure Finalize (Object : in out Limited_Record_Controller);

   package No_Primitives_For_Limited_Record_Controller is
      procedure Write (
         Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : Limited_Record_Controller) is null;
      procedure Read (
         Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : out Limited_Record_Controller) is null;
   end No_Primitives_For_Limited_Record_Controller;

   for Limited_Record_Controller'Read use
      No_Primitives_For_Limited_Record_Controller.Read;
   for Limited_Record_Controller'Write use
      No_Primitives_For_Limited_Record_Controller.Write;

   type Record_Controller is new Limited_Record_Controller with record
      My_Address : Address;
   end record;

   overriding procedure Initialize (Object : in out Record_Controller);
   overriding procedure Adjust (Object : in out Record_Controller);

   package No_Primitives_For_Record_Controller is
      procedure Write (
         Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : Record_Controller) is null;
      procedure Read (
         Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item : out Record_Controller) is null;
   end No_Primitives_For_Record_Controller;

   for Record_Controller'Read use No_Primitives_For_Record_Controller.Read;
   for Record_Controller'Write use No_Primitives_For_Record_Controller.Write;

end System.Finalization_Implementation;
