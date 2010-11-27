with System.Formatting;
with System.Img_LLI;
package body System.Img_LLD is
   pragma Suppress (All_Checks);

   procedure Image_Long_Long_Decimal (
      V : Long_Long_Integer;
      S : in out String;
      P : out Natural;
      Scale : Integer) is
   begin
      if Scale > 0 then
         declare
            Sp : constant Long_Long_Integer := 10 ** Scale;
            Int_Part : constant Long_Long_Integer := V / Sp;
            Dec_Part : constant Long_Long_Integer := abs V rem Sp;
            Error : Boolean;
         begin
            Img_LLI.Image_Long_Long_Integer (Int_Part, S, P);
            pragma Assert (P + 1 <= S'Last);
            P := P + 1;
            S (P) := '.';
            Formatting.Image (
               Formatting.Longest_Unsigned (Dec_Part),
               S (P + 1 .. S'Last),
               P,
               Width => Scale,
               Padding => '0',
               Error => Error);
            pragma Assert (not Error);
         end;
      else
         Img_LLI.Image_Long_Long_Integer (V, S, P);
         if V /= 0 then
            for I in Scale .. -1 loop
               pragma Assert (P + 1 <= S'Last);
               P := P + 1;
               S (P) := '0';
            end loop;
         end if;
         pragma Assert (P + 2 <= S'Last);
         P := P + 1;
         S (P) := '.';
         P := P + 1;
         S (P) := '0';
      end if;
   end Image_Long_Long_Decimal;

end System.Img_LLD;
