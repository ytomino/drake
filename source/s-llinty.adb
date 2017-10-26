package body System.Long_Long_Integer_Types is
   pragma Suppress (All_Checks);

   --  libgcc
   function udivmoddi4 (
      a, b : Long_Long_Unsigned;
      c : not null access Long_Long_Unsigned)
      return Long_Long_Unsigned
      with Import, Convention => C, External_Name => "__udivmoddi4";

   --  implementation

   procedure Divide (
      Left, Right : Long_Long_Unsigned;
      Quotient, Remainder : out Long_Long_Unsigned) is
   begin
      if Long_Long_Integer'Size <= Standard'Word_Size then
         --  word size "/" and "rem" would be optimized
         Quotient := Left / Right;
         Remainder := Left rem Right;
      else
         declare
            Aliased_Remainder : aliased Long_Long_Unsigned;
         begin
            Quotient := udivmoddi4 (Left, Right, Aliased_Remainder'Access);
            Remainder := Aliased_Remainder;
         end;
      end if;
   end Divide;

end System.Long_Long_Integer_Types;
