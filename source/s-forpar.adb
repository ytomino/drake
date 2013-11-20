package body System.Form_Parameters is

   --  parsing form parameter

   procedure Get (
      Form : String;
      Keyword_First : out Positive;
      Keyword_Last : out Natural;
      Item_First : out Positive;
      Item_Last : out Natural;
      Last : out Natural) is
   begin
      --  get keyword
      Keyword_First := Form'First;
      Keyword_Last := Keyword_First - 1;
      loop
         if Keyword_Last >= Form'Last then
            Item_First := Keyword_Last + 1;
            Item_Last := Keyword_Last;
            Last := Keyword_Last;
            exit;
         elsif Form (Keyword_Last + 1) = ',' then
            Item_First := Keyword_Last + 1;
            Item_Last := Keyword_Last;
            Last := Keyword_Last + 1; -- skip ','
            exit;
         elsif Form (Keyword_Last + 1) = '=' then
            --  get value
            Item_First := Keyword_Last + 2; -- skip '='
            Item_Last := Keyword_Last + 1;
            loop
               if Item_Last >= Form'Last then
                  Last := Item_Last;
                  exit;
               elsif Form (Item_Last + 1) = ',' then
                  Last := Item_Last + 1; -- skip ','
                  exit;
               end if;
               Item_Last := Item_Last + 1;
            end loop;
            exit;
         end if;
         Keyword_Last := Keyword_Last + 1;
      end loop;
   end Get;

end System.Form_Parameters;
