pragma License (Unrestricted);
--  implementation package required by compiler
with System.Formatting;
with System.Unsigned_Types;
package System.Val_Uns is
   pragma Pure;

   --  required for Modular'Value by compiler (s-valuns.ads)
   function Value_Unsigned (Str : String) return Unsigned_Types.Unsigned;

   --  helper
   procedure Skip_Spaces (S : String; Index : in out Positive);
   procedure Check_Last (S : String; Index : Positive);
   procedure Get_Unsigned (
      S : String;
      Index : in out Positive;
      Result : out Formatting.Unsigned;
      Base : Formatting.Base_Type);
   procedure Get_Exponent (
      S : String;
      Index : in out Positive;
      Result : out Integer;
      Positive_Only : Boolean);
   procedure Get_Unsigned_Literal_Without_Sign (
      S : String;
      Index : in out Positive;
      Result : out Formatting.Unsigned);

end System.Val_Uns;
