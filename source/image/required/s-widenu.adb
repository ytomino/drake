package body System.Wid_Enum is

   function Width_Enumeration_8 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural)
      return Natural
   is
      pragma Unreferenced (Names);
      pragma Suppress (Alignment_Check);
      type Index_Type is mod 2 ** 8;
      type Index_Array_Type is array (0 .. Hi + 1) of Index_Type;
      Indexes_All : Index_Array_Type;
      for Indexes_All'Address use Indexes;
      Result : Natural := 0;
   begin
      for I in Lo .. Hi loop
         Result := Natural'Max (
            Natural (Indexes_All (I + 1) - Indexes_All (I)),
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
      pragma Suppress (Alignment_Check);
      type Index_Type is mod 2 ** 16;
      type Index_Array_Type is array (0 .. Hi + 1) of Index_Type;
      Indexes_All : Index_Array_Type;
      for Indexes_All'Address use Indexes;
      Result : Natural := 0;
   begin
      for I in Lo .. Hi loop
         Result := Natural'Max (
            Natural (Indexes_All (I + 1) - Indexes_All (I)),
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
      pragma Suppress (Alignment_Check);
      type Index_Type is mod 2 ** 32;
      type Index_Array_Type is array (0 .. Hi + 1) of Index_Type;
      Indexes_All : Index_Array_Type;
      for Indexes_All'Address use Indexes;
      Result : Natural := 0;
   begin
      for I in Lo .. Hi loop
         Result := Natural'Max (
            Natural (Indexes_All (I + 1) - Indexes_All (I)),
            Result);
      end loop;
      return Result;
   end Width_Enumeration_32;

end System.Wid_Enum;
