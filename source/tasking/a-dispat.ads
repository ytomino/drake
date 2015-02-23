pragma License (Unrestricted);
private with System.Synchronous_Control;
package Ada.Dispatching is
   pragma Preelaborate;

   procedure Yield;
   pragma Inline (Yield); -- renamed

   Dispatching_Policy_Error : exception;

private

   procedure Yield
      renames System.Synchronous_Control.Yield;

end Ada.Dispatching;
