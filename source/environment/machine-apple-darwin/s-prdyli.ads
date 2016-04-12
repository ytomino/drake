pragma License (Unrestricted);
--  extended unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with Ada.IO_Exceptions;
private with Ada.Finalization;
private with C;
package System.Program.Dynamic_Linking is
   --  Loading dynamic-link library.
   pragma Preelaborate;

   type Library is limited private;

--  subtype Open_Library is Library
--    with
--       Dynamic_Predicate => Is_Open (Open_Library),
--       Predicate_Failure => raise Status_Error;

   function Is_Open (Lib : Library) return Boolean;
   pragma Inline (Is_Open);

   procedure Open (Lib : in out Library; Name : String);
   function Open (Name : String) return Library;

   procedure Close (Lib : in out Library);

   function Import (
      Lib : Library; -- Open_Library
      Symbol : String)
      return Address;

   Status_Error : exception
      renames Ada.IO_Exceptions.Status_Error;
   Name_Error : exception
      renames Ada.IO_Exceptions.Name_Error;
   Use_Error : exception
      renames Ada.IO_Exceptions.Use_Error;
   Data_Error : exception
      renames Ada.IO_Exceptions.Data_Error;

private

   package Controlled is

      type Library is limited private;

      function Reference (Lib : Dynamic_Linking.Library)
         return not null access C.void_ptr;
      pragma Inline (Reference);

   private

      type Library is
         limited new Ada.Finalization.Limited_Controlled with
      record
         Handle : aliased C.void_ptr := C.void_ptr (Null_Address);
      end record;

      overriding procedure Finalize (Object : in out Library);

   end Controlled;

   type Library is new Controlled.Library;

end System.Program.Dynamic_Linking;
