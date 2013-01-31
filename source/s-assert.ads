pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Assertions is
   pragma Pure; -- called from Ada.Assertions

   --  required by compiler ??? (s-assert.ads)
--  Assert_Failure : exception;

   --  required for pragma Assert by compiler, and gdb knows (s-assert.ads)
   procedure Raise_Assert_Failure (Msg : String);
   pragma No_Return (Raise_Assert_Failure);

end System.Assertions;
