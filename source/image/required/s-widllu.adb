with System.Formatting;
with System.Long_Long_Integer_Types;
package body System.Wid_LLU is
   use type Unsigned_Types.Long_Long_Unsigned;

   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;
   subtype Long_Long_Unsigned is Long_Long_Integer_Types.Long_Long_Unsigned;

   --  implementation

   function Width_Long_Long_Unsigned (
      Lo, Hi : Unsigned_Types.Long_Long_Unsigned)
      return Natural is
   begin
      if Lo > Hi then
         return 0;
      else
         declare
            Digits_Width : Natural;
         begin
            if Unsigned_Types.Long_Long_Unsigned'Size <=
               Standard'Word_Size
            then
               Digits_Width := Formatting.Digits_Width (Word_Unsigned (Hi));
            else
               Digits_Width :=
                  Formatting.Digits_Width (Long_Long_Unsigned (Hi));
            end if;
            return Digits_Width + 1; -- sign
         end;
      end if;
   end Width_Long_Long_Unsigned;

end System.Wid_LLU;
