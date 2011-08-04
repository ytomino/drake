with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Aux_Dec;
with System.Storage_Pools.Overlay;
package body System.Initialization is
   pragma Suppress (All_Checks);
   use Aux_Dec;

   type Object_Access is access all Object;
   for Object_Access'Storage_Pool use Storage_Pools.Overlay.Pool;

   procedure Free is new Ada.Unchecked_Deallocation (
      Object,
      Object_Access);

   function New_Object (Storage : not null access Object_Storage)
      return Object_Pointer is
   begin
      if Object'Has_Access_Values
         or else Object'Has_Tagged_Values
         or else Object'Type_Class = Type_Class_Task
      then
         Storage_Pools.Overlay.Set_Address (Storage.all'Address);
         return Object_Pointer (Object_Access'(new Object));
      else
         declare
            type Storage_Access is access all Object_Storage;
            function Cast is new Ada.Unchecked_Conversion (
               Storage_Access,
               Object_Pointer);
         begin
            return Cast (Storage_Access (Storage));
         end;
      end if;
   end New_Object;

   function New_Object (
      Storage : not null access Object_Storage;
      Value : Object)
      return Object_Pointer is
   begin
      if Object'Has_Access_Values
         or else Object'Has_Tagged_Values
         or else Object'Type_Class = Type_Class_Task
      then
         Storage_Pools.Overlay.Set_Address (Storage.all'Address);
         return Object_Pointer (Object_Access'(new Object'(Value)));
      else
         declare
            type Storage_Access is access all Object_Storage;
            function Cast is new Ada.Unchecked_Conversion (
               Storage_Access,
               Object_Pointer);
         begin
            return Result : constant Object_Pointer :=
               Cast (Storage_Access (Storage))
            do
               Result.all := Value;
            end return;
         end;
      end if;
   end New_Object;

   procedure Delete_Object (Storage : not null access Object_Storage) is
   begin
      if Object'Has_Access_Values
         or else Object'Has_Tagged_Values
         or else Object'Type_Class = Type_Class_Task
      then
         declare
            type Storage_Access is access all Object_Storage;
            function Cast is new Ada.Unchecked_Conversion (
               Storage_Access,
               Object_Access);
            X : Object_Access := Cast (Storage_Access (Storage));
         begin
            Free (X);
         end;
      end if;
   end Delete_Object;

end System.Initialization;
