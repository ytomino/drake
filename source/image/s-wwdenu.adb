with System.Wid_Enum;
package body System.WWd_Enum is
   pragma Suppress (All_Checks);

   function Wide_Width_Enumeration_8 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural;
      EM : WC_Encoding_Method := 1)
      return Natural
   is
      pragma Unreferenced (EM);
   begin
      return Wid_Enum.Width_Enumeration_8 (Names, Indexes, Lo, Hi);
   end Wide_Width_Enumeration_8;

   function Wide_Width_Enumeration_16 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural;
      EM : WC_Encoding_Method := 1)
      return Natural
   is
      pragma Unreferenced (EM);
   begin
      return Wid_Enum.Width_Enumeration_16 (Names, Indexes, Lo, Hi);
   end Wide_Width_Enumeration_16;

   function Wide_Width_Enumeration_32 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural;
      EM : WC_Encoding_Method := 1)
      return Natural
   is
      pragma Unreferenced (EM);
   begin
      return Wid_Enum.Width_Enumeration_32 (Names, Indexes, Lo, Hi);
   end Wide_Width_Enumeration_32;

   function Wide_Wide_Width_Enumeration_8 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural;
      EM : WC_Encoding_Method := 1)
      return Natural
   is
      pragma Unreferenced (EM);
   begin
      return Wid_Enum.Width_Enumeration_8 (Names, Indexes, Lo, Hi);
   end Wide_Wide_Width_Enumeration_8;

   function Wide_Wide_Width_Enumeration_16 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural;
      EM : WC_Encoding_Method := 1)
      return Natural
   is
      pragma Unreferenced (EM);
   begin
      return Wid_Enum.Width_Enumeration_16 (Names, Indexes, Lo, Hi);
   end Wide_Wide_Width_Enumeration_16;

   function Wide_Wide_Width_Enumeration_32 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural;
      EM : WC_Encoding_Method := 1)
      return Natural
   is
      pragma Unreferenced (EM);
   begin
      return Wid_Enum.Width_Enumeration_32 (Names, Indexes, Lo, Hi);
   end Wide_Wide_Width_Enumeration_32;

end System.WWd_Enum;
