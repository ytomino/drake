pragma License (Unrestricted);
--  implementation package required by compiler
with System.Unsigned_Types;
package System.Version_Control is
   pragma Pure;

   subtype Version_String is String (1 .. 8);

   --  required for PACKAGE'Version by compiler (s-vercon.ads)
   function Get_Version_String (V : Unsigned_Types.Unsigned)
      return Version_String;

end System.Version_Control;
