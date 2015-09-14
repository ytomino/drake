pragma License (Unrestricted);
--  extended unit, please see AI05-0001-1
private with System.Storage_Elements;
generic
--  type Object is limited private;
   type Object is private;
   --  limited type could not have copy-assign version New_Object
package System.Initialization is
   --  This is an implementation of Matthew Heaney's plan in AI05-0001-1.
   pragma Preelaborate;

   type Object_Storage is private;
   --  has size and alignment of type Object
   --  need separate versions for limited Obj vs. non-limited Obj?
   --  => yes

   type Object_Pointer is access all Object;
   for Object_Pointer'Storage_Size use 0;

   --  to default-init object
   function New_Object (Storage : not null access Object_Storage)
      return Object_Pointer;

   --  to copy-assign the object
   function New_Object (
      Storage : not null access Object_Storage;
      Value : Object)
      return Object_Pointer;

   --  modified
   --  Because Object_Storage is not tagged type,
   --  so it is not assured to pass-by-reference (?)
--  procedure Delete_Object (Storage : in out Object_Storage);
   procedure Delete_Object (Storage : not null access Object_Storage);
   --  extended
   --  New and Delete seems like sense of C++,
   --  but New and Dispose is suitable for sense of Pascal :-)
   procedure Dispose_Object (Storage : not null access Object_Storage)
      renames Delete_Object;

private

   type Object_Storage is
      new Storage_Elements.Storage_Array (
         1 ..
         Object'Max_Size_In_Storage_Elements);
   for Object_Storage'Alignment use Standard'Maximum_Alignment;
   --  Object'Alignment or Object'Size are not static expression...
   pragma Suppress_Initialization (Object_Storage);

end System.Initialization;
