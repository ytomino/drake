package body System.Val_Enum is
   pragma Suppress (All_Checks);

   function Value_Enumeration_8 (
      Names : String;
      Indexes : Address;
      Num : Natural;
      Str : String)
      return Natural is
   begin
      raise Program_Error;
      return Value_Enumeration_8 (Names, Indexes, Num, Str);
   end Value_Enumeration_8;

   function Value_Enumeration_16 (
      Names : String;
      Indexes : Address;
      Num : Natural;
      Str : String)
      return Natural is
   begin
      raise Program_Error;
      return Value_Enumeration_16 (Names, Indexes, Num, Str);
   end Value_Enumeration_16;

end System.Val_Enum;
