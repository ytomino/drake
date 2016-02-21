pragma License (Unrestricted);
--  extended unit
package Ada.Exceptions.Finally is
   --  This package provides scope guard.
   pragma Preelaborate;

   generic
      type Parameters is limited private;
      with procedure Handler (Params : in out Parameters);
   package Scoped_Holder is

      procedure Assign (Params : aliased in out Parameters);
      procedure Clear;

   end Scoped_Holder;

   generic
      type Parameters is limited private;
   procedure Try_Finally (
      Params : aliased in out Parameters;
      Process : not null access procedure (Params : in out Parameters);
      Handler : not null access procedure (Params : in out Parameters));

   generic
      type Parameters is limited private;
   procedure Try_When_All (
      Params : aliased in out Parameters;
      Process : not null access procedure (Params : in out Parameters);
      Handler : not null access procedure (Params : in out Parameters));

end Ada.Exceptions.Finally;
