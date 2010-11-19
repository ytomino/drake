pragma License (Unrestricted);
--  implementation package required by compiler
with System.Formatting;
with System.Unsigned_Types;
package System.Val_LLU is
   pragma Pure;

   --  required for Modular'Value by compiler (s-valllu.ads)
   function Value_Long_Long_Unsigned (Str : String)
      return Unsigned_Types.Long_Long_Unsigned;

   --  helper
   procedure Get_Longest_Unsigned (
      S : String;
      Index : in out Positive;
      Result : out Formatting.Longest_Unsigned;
      Base : Formatting.Base_Type);
   procedure Get_Longest_Unsigned_Literal_Without_Sign (
      S : String;
      Index : in out Positive;
      Result : out Formatting.Longest_Unsigned);

end System.Val_LLU;
