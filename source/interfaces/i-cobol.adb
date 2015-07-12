with System.C_Encoding;
with C;
package body Interfaces.COBOL is
   pragma Suppress (All_Checks);
   use type C.size_t;

   function Ada_To_COBOL (
      Item : Character;
      Substitute : COBOL_Character := '?')
      return COBOL_Character is
   begin
      return COBOL_Character (
         System.C_Encoding.To_char (
            Item,
            Substitute => C.char (Substitute)));
   end Ada_To_COBOL;

   function COBOL_To_Ada (
      Item : COBOL_Character;
      Substitute : Character := '?')
      return Character is
   begin
      return System.C_Encoding.To_Character (
         C.char (Item),
         Substitute => Substitute);
   end COBOL_To_Ada;

   function To_COBOL (
      Item : String;
      Substitute : Alphanumeric := "?")
      return Alphanumeric
   is
      Result : Alphanumeric (
         1 ..
         System.C_Encoding.Expanding_To_char * Item'Length);
      Last : Natural;
   begin
      To_COBOL (Item, Result, Last, Substitute => Substitute);
      return Result (1 .. Last);
   end To_COBOL;

   function To_Ada (
      Item : Alphanumeric;
      Substitute : String := "?")
      return String
   is
      Result : String (
         1 ..
         System.C_Encoding.Expanding_To_Character * Item'Length);
      Last : Natural;
   begin
      To_Ada (Item, Result, Last, Substitute => Substitute);
      return Result (1 .. Last);
   end To_Ada;

   procedure To_COBOL (
      Item : String;
      Target : out Alphanumeric;
      Last : out Natural;
      Substitute : Alphanumeric := "?")
   is
      Target_As_C : C.char_array (0 .. Target'Length - 1);
      for Target_As_C'Address use Target'Address;
      Substitute_As_C : C.char_array (0 .. Substitute'Length - 1);
      for Substitute_As_C'Address use Substitute'Address;
      Count : C.size_t;
   begin
      System.C_Encoding.To_Non_Nul_Terminated (
         Item,
         Target_As_C,
         Count,
         Substitute => Substitute_As_C);
      Last := Target'First + Natural (Count) - 1;
   end To_COBOL;

   procedure To_Ada (
      Item : Alphanumeric;
      Target : out String;
      Last : out Natural;
      Substitute : String := "?")
   is
      Item_As_C : C.char_array (0 .. Item'Length - 1);
      for Item_As_C'Address use Item'Address;
      Count : Natural;
   begin
      System.C_Encoding.From_Non_Nul_Terminated (
         Item_As_C,
         Target,
         Count,
         Substitute => Substitute);
      Last := Target'First + Count - 1;
   end To_Ada;

end Interfaces.COBOL;
