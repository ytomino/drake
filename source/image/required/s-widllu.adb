with System.Formatting;
with System.Long_Long_Integer_Types;
package body System.Wid_LLU is
   use type Unsigned_Types.Long_Long_Unsigned;

   subtype Long_Long_Unsigned is Long_Long_Integer_Types.Long_Long_Unsigned;

   --  implementation

   function Width_Long_Long_Unsigned (
      Lo, Hi : Unsigned_Types.Long_Long_Unsigned)
      return Natural is
   begin
      if Lo > Hi then
         return 0;
      else
         return 1 + Formatting.Digits_Width (Long_Long_Unsigned (Hi)); -- sign
      end if;
   end Width_Long_Long_Unsigned;

end System.Wid_LLU;
