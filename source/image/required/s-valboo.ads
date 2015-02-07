pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Val_Bool is
   pragma Pure;

   --  required for Boolean'Value by compiler (s-valboo.ads)
   function Value_Boolean (Str : String) return Boolean;

end System.Val_Bool;
