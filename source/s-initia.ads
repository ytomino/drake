pragma License (Unrestricted);
--  extended unit, see AI05-0001-1
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

   --  Note: Your application-code should set the alignment
   --    because Object'Alignment can not be static expression...

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
   --    so it is not assured to pass-by-reference (?)
--  procedure Delete_Object (Storage : in out Object_Storage);
   procedure Delete_Object (Storage : not null access Object_Storage);
   --  extended
   --  New and Delete seems like sense of C++,
   --    but New and Dispose is suitable for sense of Pascal :-)
   procedure Dispose_Object (Storage : not null access Object_Storage)
      renames Delete_Object;

private
   use type Storage_Elements.Storage_Offset;

   type A1 is array (1 .. 1) of Object;
   pragma Suppress_Initialization (A1);
   type A2 is array (1 .. 2) of Object;
   pragma Suppress_Initialization (A2);

   --  Use A2'Max_Size_In_Storage_Elements - A1'Max_Size_In_Storage_Elements
   --    because Ada.Finalization.Controlled'Max_Size_In_Storage_Elements
   --    includes System.Finalization_Masters.Header_Size.

   type Object_Storage is
      new Storage_Elements.Storage_Array (
         1 ..
         Storage_Elements.Storage_Offset'(A2'Max_Size_In_Storage_Elements)
            - A1'Max_Size_In_Storage_Elements);
   pragma Suppress_Initialization (Object_Storage);

end System.Initialization;
