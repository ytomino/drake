with System.Val_Enum;
with System.Value_Errors;
package body System.Val_Bool is
   pragma Suppress (All_Checks);

   function Value_Boolean (Str : String) return Boolean is
      First : Positive;
      Last : Natural;
   begin
      Val_Enum.Trim (Str, First, Last);
      if First <= Last then
         declare
            S : String := Str (First .. Last);
         begin
            Val_Enum.To_Upper (S);
            if S'Length = 5
               and then S (S'First .. S'First + 3) = "FALS"
               and then S (S'First + 4) = 'E'
            then
               return False;
            elsif S'Length = 4
               and then S (S'First .. S'First + 3) = "TRUE"
            then
               return True;
            end if;
         end;
      end if;
      Value_Errors.Raise_Value_Failure ("Boolean", Str);
   end Value_Boolean;

end System.Val_Bool;
