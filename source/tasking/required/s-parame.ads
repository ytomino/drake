pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Parameters is
   pragma Pure;

   --  required for task by compiler (s-parame.ads)
   type Size_Type is new Integer;
   Unspecified_Size : constant := Size_Type'First;

   --  required for 'Storage_Size by compiler (s-parame.ads)
   function Adjust_Storage_Size (Size : Size_Type) return Size_Type
      renames "+"; -- no effect

   --  required by compiler ??? (s-parame.ads)
--  function Default_Stack_Size return Size_Type;
--  Garbage_Collected : constant Boolean := False;

end System.Parameters;
