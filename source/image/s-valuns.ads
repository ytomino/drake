pragma License (Unrestricted);
--  implementation package required by compiler
with System.Formatting;
with System.Unsigned_Types;
package System.Val_Uns is
   pragma Pure;

   --  required for Modular'Value by compiler (s-valuns.ads)
   function Value_Unsigned (Str : String) return Unsigned_Types.Unsigned;

   --  helper
   procedure Skip_Spaces (S : String; Last : in out Natural);
   procedure Check_Last (S : String; Last : Natural; Error : out Boolean);
   procedure Get_Unsigned (
      S : String;
      Last : in out Natural;
      Result : out Formatting.Unsigned;
      Base : Formatting.Number_Base;
      Error : out Boolean);
   procedure Get_Exponent (
      S : String;
      Last : in out Natural;
      Result : out Integer;
      Positive_Only : Boolean;
      Error : out Boolean);
   procedure Get_Unsigned_Literal_Without_Sign (
      S : String;
      Last : in out Natural;
      Result : out Formatting.Unsigned;
      Error : out Boolean);
   procedure Get_Unsigned_Literal (
      S : String;
      Last : out Natural;
      Result : out Formatting.Unsigned;
      Error : out Boolean);

end System.Val_Uns;
