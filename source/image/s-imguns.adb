with System.Formatting;
package body System.Img_Uns is
   pragma Suppress (All_Checks);

   procedure Image_Unsigned (
      V : Unsigned_Types.Unsigned;
      S : in out String;
      P : out Natural)
   is
      Error : Boolean;
   begin
      if S'Last < S'First then
         Error := True;
      else
         S (S'First) := ' ';
         Formatting.Image (
            V,
            S (S'First + 1 .. S'Last),
            P,
            Error => Error);
      end if;
      if Error then
         raise Constraint_Error;
      end if;
   end Image_Unsigned;

end System.Img_Uns;
