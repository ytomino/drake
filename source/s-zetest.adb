package body System.Zero_Terminated_Strings is
   pragma Suppress (All_Checks);

   function strlen (Item : Address) return Natural;
   pragma Import (Intrinsic, strlen, "__builtin_strlen");

   --  implementation

   function Value (First : Address) return String is
      Result : String (1 .. strlen (First));
      for Result'Address use First;
   begin
      return Result;
   end Value;

end System.Zero_Terminated_Strings;
