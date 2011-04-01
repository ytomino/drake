package body Ada.Enumeration is
   pragma Suppress (All_Checks);

   package body Arithmetic is

      function "+" (Left : Enum; Right : Distance) return Enum is
      begin
         return Enum'Val (Enum'Pos (Left) + Right);
      end "+";

      function "+" (Left : Distance; Right : Enum) return Enum is
      begin
         return Enum'Val (Left + Enum'Pos (Right));
      end "+";

      function "-" (Left : Enum; Right : Distance) return Enum is
      begin
         return Enum'Val (Enum'Pos (Left) - Right);
      end "-";

      function "-" (Left, Right : Enum) return Distance is
      begin
         return Enum'Pos (Left) - Enum'Pos (Right);
      end "-";

      procedure Increment (Ref : in out Enum) is
      begin
         Ref := Enum'Succ (Ref);
      end Increment;

      procedure Decrement (Ref : in out Enum) is
      begin
         Ref := Enum'Pred (Ref);
      end Decrement;

   end Arithmetic;

end Ada.Enumeration;
