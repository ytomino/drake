pragma License (Unrestricted);
--  implementation package required by compiler
package System.Val_Enum is
   pragma Pure;

   --  required for Enum'Value by compiler (s-valenu.ads)
   function Value_Enumeration_8 (
      Names : String;
      Indexes : Address;
      Num : Natural;
      Str : String)
      return Natural;

   --  required for Enum'Value by compiler (s-valenu.ads)
   function Value_Enumeration_16 (
      Names : String;
      Indexes : Address;
      Num : Natural;
      Str : String)
      return Natural;

end System.Val_Enum;
