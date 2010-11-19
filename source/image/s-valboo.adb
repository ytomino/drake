with System.Val_Enum;
package body System.Val_Bool is
   pragma Suppress (All_Checks);

   function Value_Boolean (Str : String) return Boolean is
      First : Positive;
      Last : Natural;
   begin
      Val_Enum.Trim (Str, First, Last);
      declare
         S : String := Str (First .. Last);
      begin
         Val_Enum.To_Upper (S);
         if S = "FALSE" then
            return False;
         elsif S = "TRUE" then
            return True;
         else
            raise Constraint_Error;
         end if;
      end;
   end Value_Boolean;

end System.Val_Bool;
