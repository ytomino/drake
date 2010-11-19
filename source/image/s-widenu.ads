pragma License (Unrestricted);
--  implementation package required by compiler
package System.Wid_Enum is
   pragma Pure;

   --  required for Enum'Width by compiler (s-widenu.ads)
   function Width_Enumeration_8 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural)
      return Natural;

   function Width_Enumeration_16 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural)
      return Natural;

   function Width_Enumeration_32 (
      Names : String;
      Indexes : Address;
      Lo, Hi : Natural)
      return Natural;

end System.Wid_Enum;
