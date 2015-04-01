pragma License (Unrestricted);
--  extended unit
package Ada.Enumeration is
   --  There are helper subprograms for enumeration types.
   pragma Pure;

   generic
      type Enum is (<>);
      type Distance is range <>;
   package Arithmetic is

      function "+" (Left : Enum; Right : Distance) return Enum
         with Convention => Intrinsic;
      pragma Pure_Function ("+");
      pragma Inline_Always ("+");
      function "+" (Left : Distance; Right : Enum) return Enum
         with Convention => Intrinsic;
      pragma Pure_Function ("+");
      pragma Inline_Always ("+");
      function "-" (Left : Enum; Right : Distance) return Enum
         with Convention => Intrinsic;
      pragma Pure_Function ("-");
      pragma Inline_Always ("-");
      function "-" (Left, Right : Enum) return Distance
         with Convention => Intrinsic;
      pragma Pure_Function ("-");
      pragma Inline_Always ("-");

      procedure Increment (Ref : in out Enum)
         with Convention => Intrinsic;
      pragma Inline_Always (Increment);
      procedure Decrement (Ref : in out Enum)
         with Convention => Intrinsic;
      pragma Inline_Always (Decrement);

   end Arithmetic;

end Ada.Enumeration;
