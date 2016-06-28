pragma License (Unrestricted);
--  implementation unit specialized for Darwin
package System.Synchronous_Objects.Abortable.Delays is
   pragma Preelaborate;

   procedure Delay_For (D : Duration);

   procedure Register_Delays;
   procedure Unregister_Delays;

end System.Synchronous_Objects.Abortable.Delays;
