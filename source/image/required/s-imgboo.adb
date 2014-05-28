package body System.Img_Bool is
   pragma Suppress (All_Checks);

   procedure Image_Boolean (
      V : Boolean;
      S : in out String;
      P : out Natural) is
   begin
      if V then
         pragma Assert (S'Length >= 4);
         P := S'First + 3;
         S (S'First .. P) := "TRUE";
      else
         pragma Assert (S'Length >= 5);
         P := S'First + 4;
         S (S'First .. P) := "FALSE";
      end if;
   end Image_Boolean;

end System.Img_Bool;
