package body System.Mantissa is
   pragma Suppress (All_Checks);

   type Unsigned is mod 2 ** Integer'Size;

   function clz (X : Unsigned) return Unsigned
      with Import, Convention => Intrinsic, External_Name => "__builtin_clz";

   --  implementation

   function Mantissa_Value (First, Last : Integer) return Natural is
      Max : Integer;
   begin
      if First < 0 then
         declare
            Actual_First : constant Integer := -(First + 1); -- bitwise not
         begin
            if Last <= 0 then
               Max := Actual_First; -- First <= Last <= 0
            else
               Max := Integer'Max (Actual_First, Last);
            end if;
         end;
      else
         Max := Last; -- 0 <= First <= Last
      end if;
      if Max = 0 then
         return 0;
      else
         return Natural (
            (clz (Unsigned'Mod (Max)) xor (Integer'Size - 1)) + 1);
      end if;
   end Mantissa_Value;

end System.Mantissa;
