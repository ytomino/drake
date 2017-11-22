pragma License (Unrestricted);
--  extended unit
package Ada.Tags.Delegating is
   --  Delphi-like interface delegation.
   pragma Preelaborate;

   generic
      type T (<>) is abstract tagged limited private;
      type I is limited interface;
      with function Get (Object : not null access T'Class)
         return access I'Class
         with Convention => Ada;
   procedure Implements;

end Ada.Tags.Delegating;
