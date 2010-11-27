with System.Formatting;
with System.Img_Int;
package body System.Img_Dec is
   pragma Suppress (All_Checks);

   procedure Image_Decimal (
      V : Integer;
      S : in out String;
      P : out Natural;
      Scale : Integer) is
   begin
      if Scale > 0 then
         declare
            Sp : constant Integer := 10 ** Scale;
            Int_Part : constant Integer := V / Sp;
            Dec_Part : constant Integer := abs V rem Sp;
            Error : Boolean;
         begin
            Img_Int.Image_Integer (Int_Part, S, P);
            pragma Assert (P + 1 <= S'Last);
            P := P + 1;
            S (P) := '.';
            Formatting.Image (
               Formatting.Unsigned (Dec_Part),
               S (P + 1 .. S'Last),
               P,
               Width => Scale,
               Padding => '0',
               Error => Error);
            pragma Assert (not Error);
         end;
      else
         Img_Int.Image_Integer (V, S, P);
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
   end Image_Decimal;

end System.Img_Dec;
