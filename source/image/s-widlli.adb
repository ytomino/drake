with System.Formatting;
package body System.Wid_LLI is
   pragma Suppress (All_Checks);

   function Width_Long_Long_Integer (Lo, Hi : Long_Long_Integer)
      return Natural is
   begin
      if Lo > Hi then
         return 0;
      else
         declare
            Max_Abs : Formatting.Longest_Unsigned;
         begin
            if Hi <= 0 then
               Max_Abs := Formatting.Longest_Unsigned'Mod (-Lo);
            elsif Lo >= 0 then
               Max_Abs := Formatting.Longest_Unsigned (Hi);
            else --  Lo < 0 and then Hi > 0
               Max_Abs := Formatting.Longest_Unsigned'Max (
                  Formatting.Longest_Unsigned'Mod (-Lo),
                  Formatting.Longest_Unsigned (Hi));
            end if;
            return 1 + Formatting.Width (Max_Abs);
         end;
      end if;
   end Width_Long_Long_Integer;

end System.Wid_LLI;
