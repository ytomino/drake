pragma License (Unrestricted);
--  extended unit
with Ada.References.Wide_Wide_Strings;
with Ada.Strings.Generic_Unbounded;
with System.Strings.Stream_Ops;
package Ada.Strings.Unbounded_Wide_Wide_Strings is new Generic_Unbounded (
   Wide_Wide_Character,
   Wide_Wide_String,
   System.Strings.Stream_Ops.Wide_Wide_String_Read_Blk_IO,
   System.Strings.Stream_Ops.Wide_Wide_String_Write_Blk_IO,
   References.Wide_Wide_Strings.Slicing);
pragma Preelaborate (Ada.Strings.Unbounded_Wide_Wide_Strings);
