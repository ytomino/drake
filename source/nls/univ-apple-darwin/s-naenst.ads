pragma License (Unrestricted);
--  extended unit
with System.Native_Encoding.Generic_Strings;
package System.Native_Encoding.Strings is
   new Generic_Strings (
      Character,
      String);
pragma Preelaborate (System.Native_Encoding.Strings);
