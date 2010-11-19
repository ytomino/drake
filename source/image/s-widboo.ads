pragma License (Unrestricted);
--  implementation package required by compiler
package System.Wid_Bool is
   pragma Pure;

   --  required for Boolean'Width by compiler (s-widboo.ads)
   function Width_Boolean (Lo, Hi : Boolean) return Natural;

end System.Wid_Bool;
