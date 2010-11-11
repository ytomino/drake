package body Ada.Strings.Generic_Bounded is

   package body Generic_Bounded_Length is

      function Null_Bounded_String return Bounded_String is
      begin
         return (Max => Max, Length => 0, Data => <>);
      end Null_Bounded_String;

      function Length (Source : Bounded_String) return Length_Range is
      begin
         return Source.Length;
      end Length;

   end Generic_Bounded_Length;

end Ada.Strings.Generic_Bounded;
