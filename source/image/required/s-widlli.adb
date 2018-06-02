with System.Formatting;
with System.Long_Long_Integer_Types;
package body System.Wid_LLI is
   use type Long_Long_Integer_Types.Long_Long_Unsigned;

   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;
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
            Digits_Width : Natural;
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
            if Long_Long_Integer'Size <= Standard'Word_Size then
               Digits_Width :=
                  Formatting.Digits_Width (Word_Unsigned (Max_Abs));
            else
               Digits_Width := Formatting.Digits_Width (Max_Abs);
            end if;
            return Digits_Width + 1; -- sign
         end;
      end if;
   end Width_Long_Long_Integer;

end System.Wid_LLI;
