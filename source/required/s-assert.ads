pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Assertions;
package System.Assertions is
   pragma Pure; -- called from Ada.Assertions

   --  required by compiler ??? (s-assert.ads)
   Assert_Failure : exception
      renames Ada.Assertions.Assertion_Error;

   --  required for pragma Assert by compiler, and GDB knows (s-assert.ads)
   procedure Raise_Assert_Failure (Msg : String)
      renames Ada.Assertions.Raise_Assertion_Error;

end System.Assertions;
