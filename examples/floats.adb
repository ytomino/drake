with Ada.Float;
procedure floats is
	X : Long_Long_Float := Long_Long_Float'Value ("2.0");
	function Infinity is new Ada.Float.Infinity (Long_Long_Float);
	function NaN is new Ada.Float.NaN (Long_Long_Float);
begin
	pragma Assert (Long_Long_Float'Image (Infinity) = " INF");
	pragma Assert (Long_Long_Float'Image (-Infinity) = "-INF");
	pragma Assert (Long_Long_Float'Image (NaN) = " NAN");
	pragma Assert (Long_Long_Float'Image (-NaN) = "-NAN");
	pragma Assert (not (-NaN < 0.0)); -- comparison NaN is always False
	pragma Assert (X'Valid);
	pragma Assert (Long_Long_Float'Fraction (X) = 0.5);
	pragma Assert (Long_Long_Float'Exponent (X) = 2);
	pragma Debug (Ada.Debug.Put ("OK"));
	null;
end floats;
