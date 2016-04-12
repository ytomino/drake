pragma License (Unrestricted);
--  extended unit
package Ada.Streams.Block_Transmission.Strings is
   --  There are effective streaming operations for String.
   pragma Pure;

   procedure Read is
      new Block_Transmission.Read (Positive, Character, String);

   procedure Write is
      new Block_Transmission.Write (Positive, Character, String);

end Ada.Streams.Block_Transmission.Strings;
