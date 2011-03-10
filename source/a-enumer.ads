pragma License (Unrestricted);
--  extended package
package Ada.Enumeration is
   pragma Pure;

   generic
      type Enum is (<>);
      type Distance is range <>;
   package Arithmetic is

      function "+" (Left : Enum; Right : Distance) return Enum;
      pragma Pure_Function ("+");
      pragma Inline_Always ("+");
      function "+" (Left : Distance; Right : Enum) return Enum;
      pragma Pure_Function ("+");
      pragma Inline_Always ("+");
      function "-" (Left : Enum; Right : Distance) return Enum;
      pragma Pure_Function ("-");
      pragma Inline_Always ("-");
      function "-" (Left, Right : Enum) return Distance;
      pragma Pure_Function ("-");
      pragma Inline_Always ("-");

      procedure Increment (Ref : in out Enum);
      pragma Inline_Always (Increment);
      procedure Decrement (Ref : in out Enum);
      pragma Inline_Always (Decrement);

   end Arithmetic;

end Ada.Enumeration;
