pragma License (Unrestricted);
--  extended unit
with Ada.References.Wide_Strings;
with Ada.Strings.Generic_Bounded;
with System.Strings.Stream_Ops;
package Ada.Strings.Bounded_Wide_Strings is
   new Generic_Bounded (
      Wide_Character,
      Wide_String,
      System.Strings.Stream_Ops.Wide_String_Read_Blk_IO,
      System.Strings.Stream_Ops.Wide_String_Write_Blk_IO,
      References.Wide_Strings.Slicing);
pragma Preelaborate (Ada.Strings.Bounded_Wide_Strings);
