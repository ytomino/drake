with System.UTF_Conversions;
package body Ada.Strings.UTF_Encoding.Conversions is

   function Convert (
      Item : UTF_8_String;
      Output_BOM : Boolean := False)
      return UTF_16_Wide_String
   is
      Item_First : Positive := Item'First;
      Result : Wide_String (1 .. Item'Length + 1);
      Index : Positive := Result'First;
      Last : Natural;
      Error : Boolean;
   begin
      if Item_First + 2 <= Item'Last
         and then Item (Item_First .. Item_First + 2) = BOM_8
      then
         Item_First := Item_First + 3;
      end if;
      if Output_BOM then
         Result (Index) := BOM_16 (1);
         Index := Index + 1;
      end if;
      System.UTF_Conversions.UTF_8_To_UTF_16 (
         Item (Item_First .. Item'Last),
         Result (Index .. Result'Last),
         Last,
         Error);
      if Error then
         raise Encoding_Error;
      end if;
      return Result (Result'First .. Last);
   end Convert;

   function Convert (
      Item : UTF_16_Wide_String;
      Output_BOM : Boolean := False)
      return UTF_8_String
   is
      Item_First : Positive := Item'First;
      Result : String (
         1 ..
         Item'Length * System.UTF_Conversions.UTF_8_Max_Length + 3);
      Index : Positive := Result'First;
      Last : Natural;
      Error : Boolean;
   begin
      if Item_First <= Item'Last
         and then Item (Item_First) = BOM_16 (1)
      then
         Item_First := Item_First + 1;
      end if;
      if Output_BOM then
         Result (Index .. Index + 2) := BOM_8;
         Index := Index + 3;
      end if;
      System.UTF_Conversions.UTF_16_To_UTF_8 (
         Item (Item_First .. Item'Last),
         Result (Index .. Result'Last),
         Last,
         Error);
      if Error then
         raise Encoding_Error;
      end if;
      return Result (Result'First .. Last);
   end Convert;

   function Convert (
      Item : UTF_8_String;
      Output_BOM : Boolean := False)
      return UTF_32_Wide_Wide_String
   is
      Item_First : Positive := Item'First;
      Result : Wide_Wide_String (1 .. Item'Length + 1);
      Index : Positive := Result'First;
      Last : Natural;
      Error : Boolean;
   begin
      if Item_First + 2 <= Item'Last
         and then Item (Item_First .. Item_First + 2) = BOM_8
      then
         Item_First := Item_First + 3;
      end if;
      if Output_BOM then
         Result (Index) := BOM_32 (1);
         Index := Index + 1;
      end if;
      System.UTF_Conversions.UTF_8_To_UTF_32 (
         Item (Item_First .. Item'Last),
         Result (Index .. Result'Last),
         Last,
         Error);
      if Error then
         raise Encoding_Error;
      end if;
      return Result (Result'First .. Last);
   end Convert;

   function Convert (
      Item : UTF_32_Wide_Wide_String;
      Output_BOM : Boolean := False)
      return UTF_8_String
   is
      Item_First : Positive := Item'First;
      Result : String (
         1 ..
         Item'Length * System.UTF_Conversions.UTF_8_Max_Length + 3);
      Index : Positive := Result'First;
      Last : Natural;
      Error : Boolean;
   begin
      if Item_First <= Item'Last
         and then Item (Item_First) = BOM_32 (1)
      then
         Item_First := Item_First + 1;
      end if;
      if Output_BOM then
         Result (Index .. Index + 2) := BOM_8;
         Index := Index + 3;
      end if;
      System.UTF_Conversions.UTF_32_To_UTF_8 (
         Item (Item_First .. Item'Last),
         Result (Index .. Result'Last),
         Last,
         Error);
      if Error then
         raise Encoding_Error;
      end if;
      return Result (Result'First .. Last);
   end Convert;

end Ada.Strings.UTF_Encoding.Conversions;
