pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
with System;
private with Ada.Finalization;
private with C.windef;
package Ada.Dynamic_Linking is
   --  Loading dynamic-link library.
   pragma Preelaborate;

   type Library is limited private;

   procedure Open (Lib : in out Library; Name : String);
   function Open (Name : String) return Library;

   procedure Close (Lib : in out Library);

   function Is_Open (Lib : Library) return Boolean;

   function Import (Lib : Library; Symbol : String) return System.Address;

   Status_Error : exception
      renames IO_Exceptions.Status_Error;
   Name_Error : exception
      renames IO_Exceptions.Name_Error;
   Data_Error : exception
      renames IO_Exceptions.Data_Error;

private

   package Controlled is

      type Library is limited private;

      function Reference (Lib : Library)
         return not null access C.windef.HMODULE;
      pragma Inline (Reference);

   private

      type Library is new Finalization.Limited_Controlled with record
         Handle : aliased C.windef.HMODULE := null;
      end record;

      overriding procedure Finalize (Object : in out Library);

   end Controlled;

   type Library is new Controlled.Library;

end Ada.Dynamic_Linking;
