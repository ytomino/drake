pragma License (Unrestricted);
--  implementation package required by compiler
package System.Assertions is
   pragma Pure; --  called from Ada.Assertions

   --  required for pragma Assert by compiler (s-assert.ads)
   procedure Raise_Assert_Failure (Msg : String);
   pragma No_Return (Raise_Assert_Failure);

end System.Assertions;
