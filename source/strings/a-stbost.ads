pragma License (Unrestricted);
--  extended unit
with Ada.References.Strings;
with Ada.Strings.Generic_Bounded;
with System.Strings.Stream_Ops;
package Ada.Strings.Bounded_Strings is new Generic_Bounded (
   Character,
   String,
   System.Strings.Stream_Ops.String_Read_Blk_IO,
   System.Strings.Stream_Ops.String_Write_Blk_IO,
   References.Strings.Slicing);
pragma Preelaborate (Ada.Strings.Bounded_Strings);
