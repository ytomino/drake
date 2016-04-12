pragma License (Unrestricted);
--  extended unit
package Ada.Streams.Block_Transmission.Wide_Wide_Strings is
   --  There are effective streaming operations for Wide_Wide_String.
   pragma Pure;

   procedure Read is
      new Block_Transmission.Read (
         Positive,
         Wide_Wide_Character,
         Wide_Wide_String);

   procedure Write is
      new Block_Transmission.Write (
         Positive,
         Wide_Wide_Character,
         Wide_Wide_String);

end Ada.Streams.Block_Transmission.Wide_Wide_Strings;
