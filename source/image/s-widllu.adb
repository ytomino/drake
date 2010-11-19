with System.Formatting;
package body System.Wid_LLU is
   pragma Suppress (All_Checks);
   use type Unsigned_Types.Long_Long_Unsigned;

   function Width_Long_Long_Unsigned (
      Lo, Hi : Unsigned_Types.Long_Long_Unsigned)
      return Natural is
   begin
      if Lo > Hi then
         return 0;
      else
         return 1 + Formatting.Width (Hi);
      end if;
   end Width_Long_Long_Unsigned;

end System.Wid_LLU;
