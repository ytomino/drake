package body System.Wid_Enum is
   pragma Suppress (All_Checks);

   function Width_Enumeration_8 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural)
      return Natural
   is
      pragma Unreferenced (Names);
      type Index_Type is mod 2 ** 8;
      type Array_Type is array (Natural) of Index_Type;
      Indexes_2 : Array_Type;
      for Indexes_2'Address use Indexes;
      Result : Natural := 0;
   begin
      for I in Lo .. Hi loop
         Result := Natural'Max (
            Natural (Indexes_2 (I + 1) - Indexes_2 (I)),
            Result);
      end loop;
      return Result;
   end Width_Enumeration_8;

   function Width_Enumeration_16 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural)
      return Natural
   is
      pragma Unreferenced (Names);
      type Index_Type is mod 2 ** 16;
      type Array_Type is array (Natural) of Index_Type;
      Indexes_2 : Array_Type;
      for Indexes_2'Address use Indexes;
      Result : Natural := 0;
   begin
      for I in Lo .. Hi loop
         Result := Natural'Max (
            Natural (Indexes_2 (I + 1) - Indexes_2 (I)),
            Result);
      end loop;
      return Result;
   end Width_Enumeration_16;

   function Width_Enumeration_32 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural)
      return Natural
   is
      pragma Unreferenced (Names);
      type Index_Type is mod 2 ** 32;
      type Array_Type is array (Natural) of Index_Type;
      Indexes_2 : Array_Type;
      for Indexes_2'Address use Indexes;
      Result : Natural := 0;
   begin
      for I in Lo .. Hi loop
         Result := Natural'Max (
            Natural (Indexes_2 (I + 1) - Indexes_2 (I)),
            Result);
      end loop;
      return Result;
   end Width_Enumeration_32;

end System.Wid_Enum;
