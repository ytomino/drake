with System.Formatting;
package body Ada.Numerics.SFMT is
   pragma Suppress (All_Checks);

   --  This function represents a function used in the initialization
   --  by init_by_array
   function func1 (x : Unsigned_32) return Unsigned_32 is
   begin
      return (x xor Shift_Right (x, 27)) * 1664525;
   end func1;

   --  This function represents a function used in the initialization
   --  by init_by_array
   function func2 (x : Unsigned_32) return Unsigned_32 is
   begin
      return (x xor Shift_Right (x, 27)) * 1566083941;
   end func2;

   procedure Hex_Put (To : out String; Item : Unsigned_32) is
      Error : Boolean;
      Last : Natural;
   begin
      pragma Compile_Time_Error (
         System.Formatting.Unsigned'Size < 32,
         "integer size < 32");
      System.Formatting.Image (
         System.Formatting.Unsigned (Item),
         To,
         Last,
         Base => 16,
         Width => 32 / 4,
         Error => Error);
      pragma Assert (not Error and then Last = To'Last);
   end Hex_Put;

   procedure Hex_Get (From : String; Item : out Unsigned_32) is
      Last : Positive;
      Result : System.Formatting.Unsigned;
      Error : Boolean;
   begin
      System.Formatting.Value (
         From,
         Last,
         Result,
         Base => 16,
         Error => Error);
      if Error or else Last /= From'Last then
         raise Constraint_Error;
      end if;
      Item := Unsigned_32 (Result);
   end Hex_Get;

end Ada.Numerics.SFMT;
