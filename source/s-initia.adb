with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.Storage_Pools.Overlaps;
package body System.Initialization is
   pragma Suppress (All_Checks);

   type Object_Access is access all Object;
   for Object_Access'Storage_Pool use Storage_Pools.Overlaps.Pool.all;

   procedure Free is
      new Ada.Unchecked_Deallocation (Object, Object_Access);

   package O_Conv is
      new Address_To_Named_Access_Conversions (Object, Object_Access);

   --  implementation

   function New_Object (Storage : not null access Object_Storage)
      return Object_Pointer
   is
      type Storage_Access is access all Object_Storage;
      for Storage_Access'Storage_Size use 0;
      package S_Conv is
         new Address_To_Named_Access_Conversions (
            Object_Storage,
            Storage_Access);
   begin
      if Object'Has_Access_Values
         or else Object'Has_Tagged_Values
         or else Object'Size > Standard'Word_Size * 8 -- large object
      then
         Storage_Pools.Overlaps.Set_Address (
            S_Conv.To_Address (Storage_Access (Storage)));
         return Object_Pointer (Object_Access'(new Object));
      else
         declare
            Item : Object; -- default initialized
            pragma Unmodified (Item);
            Result : constant Object_Pointer :=
               Object_Pointer (
                  O_Conv.To_Pointer (
                     S_Conv.To_Address (Storage_Access (Storage))));
         begin
            Result.all := Item;
            return Result;
         end;
      end if;
   end New_Object;

   function New_Object (
      Storage : not null access Object_Storage;
      Value : Object)
      return Object_Pointer
   is
      type Storage_Access is access all Object_Storage;
      for Storage_Access'Storage_Size use 0;
      package S_Conv is
         new Address_To_Named_Access_Conversions (
            Object_Storage,
            Storage_Access);
   begin
      if Object'Has_Access_Values or else Object'Has_Tagged_Values then
         Storage_Pools.Overlaps.Set_Address (
            S_Conv.To_Address (Storage_Access (Storage)));
         return Object_Pointer (Object_Access'(new Object'(Value)));
      else
         declare
            Result : constant Object_Pointer :=
               Object_Pointer (
                  O_Conv.To_Pointer (
                     S_Conv.To_Address (Storage_Access (Storage))));
         begin
            Result.all := Value;
            return Result;
         end;
      end if;
   end New_Object;

   procedure Delete_Object (Storage : not null access Object_Storage) is
      type Storage_Access is access all Object_Storage;
      for Storage_Access'Storage_Size use 0;
      package S_Conv is
         new Address_To_Named_Access_Conversions (
            Object_Storage,
            Storage_Access);
   begin
      if Object'Has_Access_Values or else Object'Has_Tagged_Values then
         declare
            A : constant Address :=
               S_Conv.To_Address (Storage_Access (Storage));
            X : Object_Access := O_Conv.To_Pointer (A);
         begin
            Storage_Pools.Overlaps.Set_Address (A);
            Free (X);
         end;
      end if;
   end Delete_Object;

end System.Initialization;
