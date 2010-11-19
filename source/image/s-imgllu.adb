package body System.Img_LLU is
   pragma Suppress (All_Checks);

   procedure Image_Long_Long_Unsigned (
      V : Unsigned_Types.Long_Long_Unsigned;
      S : in out String;
      P : out Natural) is
   begin
      raise Program_Error;
   end Image_Long_Long_Unsigned;

end System.Img_LLU;
