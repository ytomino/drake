with System.Formatting;
with System.Long_Long_Integer_Types;
package body System.Wid_LLI is
   use type Long_Long_Integer_Types.Long_Long_Unsigned;

   subtype Long_Long_Unsigned is Long_Long_Integer_Types.Long_Long_Unsigned;

   --  implementation

   function Width_Long_Long_Integer (Lo, Hi : Long_Long_Integer)
      return Natural is
   begin
      if Lo > Hi then
         return 0;
      else
         declare
            Max_Abs : Long_Long_Unsigned;
         begin
            if Hi <= 0 then
               Max_Abs := -Long_Long_Unsigned'Mod (Lo);
            elsif Lo >= 0 then
               Max_Abs := Long_Long_Unsigned (Hi);
            else -- Lo < 0 and then Hi > 0
               Max_Abs := Long_Long_Unsigned'Max (
                  -Long_Long_Unsigned'Mod (Lo),
                  Long_Long_Unsigned (Hi));
            end if;
            return 1 + Formatting.Digits_Width (Max_Abs); -- sign
         end;
      end if;
   end Width_Long_Long_Integer;

end System.Wid_LLI;
