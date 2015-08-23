pragma License (Unrestricted);
--  implementation unit required by compiler
package System.WWd_Enum is
   pragma Pure;

   --  (s-wchcon.ads)
   type WC_Encoding_Method is range 1 .. 6;

   --  required for Enum'Wide_Width by compiler (s-wwdenu.ads)
   function Wide_Width_Enumeration_8 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural;
      EM : WC_Encoding_Method := 1)
      return Natural;
   function Wide_Width_Enumeration_16 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural;
      EM : WC_Encoding_Method := 1)
      return Natural;
   function Wide_Width_Enumeration_32 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural;
      EM : WC_Encoding_Method := 1)
      return Natural;

   pragma Pure_Function (Wide_Width_Enumeration_8);
   pragma Pure_Function (Wide_Width_Enumeration_16);
   pragma Pure_Function (Wide_Width_Enumeration_32);

   --  required for Enum'Wide_Wide_Width by compiler (s-wwdenu.ads)
   function Wide_Wide_Width_Enumeration_8 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural;
      EM : WC_Encoding_Method := 1)
      return Natural;
   function Wide_Wide_Width_Enumeration_16 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural;
      EM : WC_Encoding_Method := 1)
      return Natural;
   function Wide_Wide_Width_Enumeration_32 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural;
      EM : WC_Encoding_Method := 1)
      return Natural;

   pragma Pure_Function (Wide_Wide_Width_Enumeration_8);
   pragma Pure_Function (Wide_Wide_Width_Enumeration_16);
   pragma Pure_Function (Wide_Wide_Width_Enumeration_32);

   --  [gcc 4.5/4.6] it needs default values for EM to avoiding bug of compiler
   --  (missing argument for parameter "EM" in call to ...)

end System.WWd_Enum;
