package body System.Img_Bool is
   pragma Suppress (All_Checks);

   procedure Image_Boolean (
      V : Boolean;
      S : in out String;
      P : out Natural) is
   begin
      if S'Length < 5 then
         raise Constraint_Error;
      end if;
      if V then
         P := S'First + 3;
         S (S'First .. P) := "TRUE";
      else
         P := S'First + 4;
         S (S'First .. P) := "FALSE";
      end if;
   end Image_Boolean;

end System.Img_Bool;
