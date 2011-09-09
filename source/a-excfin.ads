pragma License (Unrestricted);
--  extended unit
with System;
package Ada.Exceptions.Finally is
   --  This package provides scope guard.
   pragma Preelaborate;

   generic
      type Parameters is limited private;
      with procedure Handler (Item : not null access Parameters);
   package Scoped_Holder is

      procedure Assign (Item : access Parameters);
      procedure Clear;

   end Scoped_Holder;

   procedure Try_Finally (
      Params : System.Address;
      Process : not null access procedure (Params : System.Address);
      Handler : not null access procedure (Params : System.Address));
   procedure Try_When_All (
      Params : System.Address;
      Process : not null access procedure (Params : System.Address);
      Handler : not null access procedure (Params : System.Address));

end Ada.Exceptions.Finally;
